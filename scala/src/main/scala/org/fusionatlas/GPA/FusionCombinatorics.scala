package org.fusionatlas.GPA

import scala.language.implicitConversions

import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.numberfields.NumberField
import net.tqft.toolkit.algebra.fusion.FusionBimoduleWithDimensions
import net.tqft.toolkit.algebra.fusion.FusionRingWithDimensions
import org.fusionatlas.graphs.PairOfBigraphsWithDuals

object FusionCombinatorics {
  implicit def fromFusionRing(ring: FusionRingWithDimensions): FusionCombinatorics = {
    ???
  }
  implicit def fromFusionBimoduleWithDimensions(bimodule: FusionBimoduleWithDimensions): FusionCombinatorics = {
    ???
  }

  implicit def fromPairOfBigraphsWithDualsAndDimensions(data: (PairOfBigraphsWithDuals, F, (List[List[F]], List[List[F]]))): FusionCombinatorics = {
    val graph = data._1
    def g(leftLabel: Int) = leftLabel match {
      case 0 => graph.gg0
      case 1 => graph.gg1
    }
    def depth(leftLabel: Int, rightLabel: Int, index: Int): Int = {
      val ranks = for (d <- rightLabel to g(leftLabel).graphDepth by 2) yield g(leftLabel).rankAtDepth(d)
      ranks.scanLeft(0)(_ + _).tail.indexWhere(_ > index) * 2 + rightLabel
    }
    def depth_(v: Vertex): Int = depth(v.leftLabel, v.rightLabel, v.index)
    def indexWithinDepth(leftLabel: Int, rightLabel: Int, index: Int): Int = {
      1 + index - (for (d <- rightLabel until depth(leftLabel, rightLabel, index) by 2) yield g(leftLabel).rankAtDepth(d)).sum
    }
    def indexWithinDepth_(v: Vertex): Int = indexWithinDepth(v.leftLabel, v.rightLabel, v.index)
    def indexFromIndexWithinDepth(leftLabel: Int, rightLabel: Int, depth: Int, index: Int): Int = {
      require(index > 0)
      index - 1 + (for (d <- rightLabel until depth by 2) yield g(leftLabel).rankAtDepth(d)).sum
    }

    // TODO compute everything once!
    new FusionCombinatorics {
      override val numberFieldGenerator = data._2
      override def numberOfObjects(leftLabel: Int, rightLabel: Int): Int = {
        (leftLabel, rightLabel) match {
          case (0, 0) => {
            (for (d <- 0 to graph.gg0.graphDepth by 2) yield {
              graph.gg0.rankAtDepth(d)
            }).sum
          }
          case (0, 1) => {
            (for (d <- 1 to graph.gg0.graphDepth by 2) yield {
              graph.gg0.rankAtDepth(d)
            }).sum
          }
          case (1, 1) => {
            (for (d <- 0 to graph.gg1.graphDepth by 2) yield {
              graph.gg1.rankAtDepth(d)
            }).sum
          }
          case (1, 0) => {
            (for (d <- 1 to data._1.gg1.graphDepth by 2) yield {
              data._1.gg1.rankAtDepth(d)
            }).sum
          }
        }
      }
      override def dimension(o: Vertex): F = {
        (o.leftLabel match {
          case 0 => data._3._1
          case 1 => data._3._2
        })(depth_(o))(indexWithinDepth_(o) - 1)
      }
      override def dual(leftLabel: Int, rightLabel: Int, index: Int): Int = {
        if (leftLabel != rightLabel) {
          index
        } else {
          val d = depth(leftLabel, rightLabel, index)
          val k = indexWithinDepth(leftLabel, rightLabel, index)
          indexFromIndexWithinDepth(leftLabel, rightLabel, d, g(leftLabel).involutions((g(leftLabel).graphDepth - d) / 2)(k - 1))
        }
      }
      override def multiplicity(a: Vertex, b: Vertex, c: Vertex): Int = {

        b match {
          case Vertex(0, 1, 0, 0) | Vertex(1, 0, 0, 0) => {
            val ad = depth_(a)
            val ak = indexWithinDepth_(a)
            val cd = depth_(c)
            val ck = indexWithinDepth_(c)
            g(a.leftLabel).bigraph.neighbours((ad, ak)).count(_ == (cd, ck))
          }
          case _ => {
            a match {
              case Vertex(0, 1, 0, 0) | Vertex(1, 0, 0, 0) => {
                multiplicity(b.dual, a.dual, c.dual)
              }
              case _ => {
                // uhoh, we can't answer this based on just the principal graph
                ???
              }
            }
          }
        }
      }
    }
  }

}

case class Vertex(leftLabel: Int, rightLabel: Int, index: Int, dualIndex: Int) extends StrandType[Vertex, Int] {
  require(index != 0 || dualIndex == 0)
  def dual = Vertex(rightLabel, leftLabel, dualIndex, index)
}

case class Edge(start: Vertex, finish: Vertex, colour: ChiralStrand, index: Int) {
  def reverse = Edge(finish, start, colour.dual, index)
}

case class Path(start: Vertex, edges: Seq[Edge]) {
  require(edges.isEmpty || start == edges.head.start)
  for (Seq(e1, e2) <- edges.sliding(2)) require(e1.finish == e2.start)

  def finish = if (edges.isEmpty) start else edges.last.finish
  def over = edges.map(_.colour)
  def length = edges.size

  def andThen(other: Path) = {
    require(finish == other.start)
    Path(start, edges ++ other.edges)
  }
  def andThen(edge: Edge) = {
    require(finish == edge.start)
    Path(start, edges :+ edge)
  }

  def reverse = Path(finish, edges.reverse.map(_.reverse))
}

trait FusionCombinatorics {

  def numberFieldGenerator: F
  implicit lazy val dimensionField: Field[F] = NumberField(numberFieldGenerator)

  def numberOfObjects(leftLabel: Int, rightLabel: Int): Int
  def objects(leftLabel: Int, rightLabel: Int): Iterator[Vertex] = {
    for (i <- (0 until numberOfObjects(leftLabel, rightLabel)).iterator) yield Vertex(leftLabel, rightLabel, i, dual(leftLabel, rightLabel, i))
  }
  def dual(leftLabel: Int, rightLabel: Int, index: Int): Int

  def dimension(o: Vertex): F
  def multiplicity(a: Vertex, b: Vertex, c: Vertex): Int
  def multiplicity(start: Vertex, finish: Vertex, over: ChiralStrand): Int = {
    over match {
      case Laevo(v, a) => multiplicity(v, start, finish)
      case Dextro(v, a) => multiplicity(start, v, finish)
    }
  }

  def edgesFrom(start: Vertex, over: ChiralStrand): Iterator[Edge] = {
    for (
      finish <- objects(over.rightLabel._1, over.rightLabel._2);
      e <- edgesBetween(start, finish, over)
    ) yield e

  }
  def edgesBetween(start: Vertex, finish: Vertex, over: ChiralStrand): Seq[Edge] = {
    for (k <- 0 until multiplicity(start, finish, over)) yield {
      Edge(start, finish, over, k)
    }
  }

  def edgesOver(over: ChiralStrand): Iterator[Edge] = {
    for (
      start <- objects(over.leftLabel._1, over.leftLabel._2);
      e <- edgesFrom(start, over)
    ) yield e
  }

  def cycles(leftLabel: Int, rightLabel: Int, over: Seq[ChiralStrand]): Iterator[Path] = {
    require(over.isEmpty || over.head.leftLabel._1 == leftLabel && over.head.leftLabel._2 == rightLabel)
    for (o <- objects(leftLabel, rightLabel); p <- pathsBetween(o, o, over)) yield p
  }

  def pathsFrom(start: Vertex, over: Seq[ChiralStrand]): Iterator[Path] = {
    def extend(path: Path, v: ChiralStrand) = {
      def m: Vertex => Int = {
        v match {
          case Dextro(w, a) => { t: Vertex => multiplicity(path.finish, v.strand, t) }
          case Laevo(w, a) => { t: Vertex => multiplicity(v.strand, path.finish, t) }
        }
      }

      for (finish <- objects(v.rightLabel._1, v.rightLabel._2); k <- 0 until m(finish)) yield {
        path.andThen(Edge(path.finish, finish, v, k))
      }
    }
    over.foldLeft(Iterator(Path(start, Seq.empty)))({ (iterator, v) => iterator.flatMap(p => extend(p, v)) })
  }
  def pathsTo(finish: Vertex, over: Seq[Vertex]): Iterator[Path] = {
    ???
  }
  def pathsBetween(start: Vertex, finish: Vertex, over: Seq[ChiralStrand]): Iterator[Path] = {
    over match {
      case Seq() => {
        if (start == finish) {
          Iterator(Path(start, Seq()))
        } else {
          Iterator.empty
        }
      }
      case Seq(cv) => {
        val m = cv match {
          case Dextro(v, a) => multiplicity(start, v, finish)
          case Laevo(v, a) => multiplicity(v, start, finish)
        }
        for (k <- (0 until m).iterator) yield Path(start, Seq(Edge(start, finish, cv, k)))
      }
      case _ => {
        for (
          p1 <- pathsFrom(start, over.take(over.size / 2));
          p2 <- pathsBetween(p1.finish, finish, over.drop(over.size / 2))
        ) yield {
          p1.andThen(p2)
        }
      }
    }
  }
}