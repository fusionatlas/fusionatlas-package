package org.fusionatlas.enumerators

import org.fusionatlas.graphs.obstructions.PairObstructions
import org.fusionatlas.graphs._
import org.fusionatlas.enumerators.scheduling._
import net.tqft.toolkit.Logging
import scala.collection.SortedSet

abstract class PairClassifier(d: Double, roots: List[PairOfBigraphsWithDuals], ignoredWeeds: List[PairOfBigraphsWithDuals]) extends Classifier[PairOfBigraphsWithDuals](d, roots, ignoredWeeds) with PairObstructions
abstract class DatabasePairClassifier(d: Double, roots: List[PairOfBigraphsWithDuals], ignoredWeeds: List[PairOfBigraphsWithDuals]) extends PairClassifier(d, roots, ignoredWeeds) {
  val enumerator = DatabasePairEnumerator
}

class FuturePairClassifier(d: Double, roots: List[PairOfBigraphsWithDuals], ignoredWeeds: List[PairOfBigraphsWithDuals]) extends DatabasePairClassifier(d, roots, ignoredWeeds) with FutureScheduler[PairOfBigraphsWithDuals]
class QueuePairClassifier(d: Double, roots: List[PairOfBigraphsWithDuals], ignoredWeeds: List[PairOfBigraphsWithDuals]) extends DatabasePairClassifier(d, roots, ignoredWeeds) with SQSScheduler[PairOfBigraphsWithDuals]
class LocalPairClassifier(d: Double, roots: List[PairOfBigraphsWithDuals], ignoredWeeds: List[PairOfBigraphsWithDuals]) extends PairClassifier(d, roots, ignoredWeeds) with ImmediateScheduler[PairOfBigraphsWithDuals] {
  val enumerator = InMemoryPairEnumerator
}

object PairClassifierApplication {
  def main(args: Array[String]): Unit = {
    import net.tqft.toolkit.mathematica.MathematicaExpression._

    //			val classifier = new ImmediatePairClassifier(sqrt5, A3, ignoredWeeds)
    //			val classifier = new ImmediatePairClassifier(2.22923, FSM5, List())
    //			val classifier = new ImmediatePairClassifier(2.24607, FSM5, List())

    //	  val classifier = new QueuePairClassifier(sqrt3plusSqrt5, A3, ignoredWeeds)
    //	  val classifier = new QueuePairClassifier(sqrt5, A3, ignoredWeeds)
    val classifier = new QueuePairClassifier(sqrt3plusSqrt5, List(A2), ignoredWeeds)
    //	  val classifier = new QueuePairClassifier(badSeedD, BadSeed, List())

    var pause = 0
    while (!classifier.completed) {
      println("Completed: " + classifier.completed)
      val weeds = classifier.weeds()
      val vines = classifier.vines().toList
      println("Weeds (" + weeds.size + "): " + weeds.toMathematicaInputString)
      println("Vines (" + vines.size + "): " + vines.toMathematicaInputString)

      classifier.schedulePending()

      Thread.sleep(pause)
      pause = pause + 100
      classifier.update()

    }
  }

  val sqrt5 = 2.23607
  val sqrt3plusSqrt5 = 2.28826
  val sqrt6 = 2.44949
  val badSeedD = 2.19737

  val A2 = PairOfBigraphsWithDuals("bwd1duals1", "bwd1duals1")
  val A3 = PairOfBigraphsWithDuals("bwd1v1duals1v1", "bwd1v1duals1v1")
  val A5 = PairOfBigraphsWithDuals("bwd1v1v1v1duals1v1v1", "bwd1v1v1v1duals1v1v1")
  val HaagerupTruncatedPair = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1duals1v1v1x2", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")

  val mismatchedD6 = PairOfBigraphsWithDuals("bwd1v1v1v1p1duals1v1v1x2", "bwd1v1v1v1p1duals1v1v2x1")
  val mismatchedBroom = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1duals1v1v1x2x3", "bwd1v1v1v1p1p1duals1v1v2x1x3")

  val BadSeed = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v0x0x1x0p0x1x0x0v1x0p0x1v0x1p1x0duals1v1v1x2v1x3x2x4v1x2", "bwd1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2v2x1")
  val FSM = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p1x0p0x1v0x1x0p0x0x1duals1v1v1x2v1x2v2x1", "bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v1x0x0x0p0x0x1x0p0x1x0x0v1x0x0p0x1x0p0x1x0p0x0x1duals1v1v1x2v1x3x2x4v2x1x3x4")
  val FSM5 = PairOfBigraphsWithDuals("bwd1v1v1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p1x0p0x1v0x1x0p0x0x1duals1v1v1v1x2v1x2v2x1", "bwd1v1v1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v1x0x0x0p0x0x1x0p0x1x0x0v1x0x0p0x1x0p0x1x0p0x0x1duals1v1v1v1x2v1x3x2x4v2x1x3x4")
  val Crab = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0v1p1duals1v1v1x2v1", "bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1v1x0x0p0x0x1duals1v1v1x2v1x3x2")

  val Q = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1v1x0x0duals1v1v1x3x2", "bwd1v1v1v1p1p1v1x0x0duals1v1v1x3x2")
  val Qp = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1v1x0x0duals1v1v1x2x3", "bwd1v1v1v1p1p1v1x0x0duals1v1v1x2x3")

  val ignoredWeeds = List(A5)

}


