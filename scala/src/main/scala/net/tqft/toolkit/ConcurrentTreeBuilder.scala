package net.tqft.toolkit
import scala.actors.Actor
import scala.actors.Actor._
import net.tqft.toolkit.collections.NonStrictIterable
import scala.actors.TIMEOUT

object ConcurrentTreeBuilder extends Logging {
  case class Node[A, B](a: A, b: Option[B], children: Option[List[Node[A, B]]]) {
    def data(condition: A => Boolean): Iterable[(A, B)] = NonStrictIterable.from(b.map(a -> _)) ++ NonStrictIterable.from(children.getOrElse(Nil)).flatMap(_.data(condition))
    def incomplete(condition: A => Boolean): Iterable[A] = children match {
      case None => NonStrictIterable(a)
      case Some(list) => NonStrictIterable.from(list).flatMap(_.incomplete(condition))
    }
  }

  def apply[A, B](builderFunction: A => Option[(B, List[A])])(roots: List[A]): ConcurrentTreeBuilder[A, B] = new ConcurrentTreeBuilderImpl[A, B](roots) {
    def builder(a: A) = builderFunction(a)
  }

}

trait ConcurrentTreeBuilder[A, B] {
  def snapshot(condition: A => Boolean): Iterable[ConcurrentTreeBuilder.Node[A, B]]
  def update(condition: A => Boolean)
  def builder(a: A): Option[(B, List[A])]
  def delayed(condition: A => Boolean): Iterable[A]

  def data(condition: A => Boolean) = snapshot(condition).flatMap(_.data(condition))
  def incomplete(condition: A => Boolean) = snapshot(condition).flatMap(_.incomplete(condition))
  def completed(condition: A => Boolean = { _ => true }) = incomplete(condition).isEmpty
  def complete(condition: A => Boolean, throttle: Throttle) = {
    while (!completed(condition)) {
      throttle(false)
      update(condition)
    }
    snapshot(condition)
  }
}

private abstract class ConcurrentTreeBuilderImpl[A, B](roots: List[A]) extends ConcurrentTreeBuilder[A, B] {
  private case class UpdateRequest(condition: A => Boolean)

  private final class NodeActor(val a: A) extends Actor {
    private[this] var result: Option[(B, List[NodeActor])] = None
    private[this] var finished = false
    private[this] var buildAttempts = 0
    private def delayed = !finished && buildAttempts > 0
    private[ConcurrentTreeBuilderImpl] def delayedDescendants(condition: A => Boolean): Iterable[A] = {
      if (condition(a)) {
        if (delayed) {
          NonStrictIterable(a)
        } else {
          result match {
            case None => NonStrictIterable()
            case Some((_, children)) => children flatMap (_.delayedDescendants(condition))
          }
        }
      } else {
        NonStrictIterable()
      }
    }
    private def isFinished = finished

    private[ConcurrentTreeBuilderImpl] def asNode(condition: A => Boolean): ConcurrentTreeBuilder.Node[A, B] = result match {
      case None => ConcurrentTreeBuilder.Node(a, None, None)
      case Some((b, children)) => ConcurrentTreeBuilder.Node(a, Some(b), Some(children.filter({ na: NodeActor => condition(na.a) }).map(_.asNode(condition))))
    }

    def act() {
      def emptyMailBox: Nothing = reactWithin(0) {
        case msg: UpdateRequest => emptyMailBox
        case TIMEOUT => ()
      }

      loop {
        react {
          case msg @ UpdateRequest(condition) =>
            if (!finished && condition(a)) {
              result match {
                case None => {
                  result = builder(a).map { case (b, as) => (b, as.map(x => { val na = new NodeActor(x); na.start; na ! msg; na })) }
                  if (result.isEmpty) {
                    buildAttempts = buildAttempts + 1
                  }
                }
                case Some((b, actors)) => {
                  val unfinished = actors.filter(!_.isFinished)
                  if (unfinished.isEmpty) {
                    finished = true
                  } else {
                    for (actor <- unfinished) actor ! msg
                  }
                }
              }
            }
            emptyMailBox
        }
      }
    }
  }

  private[this] val actors = roots.map(x => { val na = new NodeActor(x); na.start; na })
  override def delayed(condition: A => Boolean) = NonStrictIterable.from(actors).flatMap(_.delayedDescendants(condition))
  override def snapshot(condition: A => Boolean) = NonStrictIterable.from(actors).map(_.asNode(condition))
  override def update(condition: A => Boolean) = actors.map(_ ! UpdateRequest(condition))
}