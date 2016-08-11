package org.fusionatlas.GPA

import net.tqft.toolkit.algebra.Algebra
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.Ring
import org.fusionatlas.graphs.PairOfBigraphsWithDuals
import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.Module

class GraphPlanarAlgebra[A](val combinatorics: FusionCombinatorics, override val ring: Algebra[F, A]) extends AdditiveTwoSidedPlanarAlgebra[Vertex, Int, A, Path, GPAElement[A]] { self =>
  def dimensionField = combinatorics.dimensionField
  override val operad = new OrientedPlanarOperad[ChiralStrand, (Int, Int)]
  override def module(disk: ChiralDisk) = {
    new Module[A, GPAElement[A]] {
      private val mapModule: Module[A, Map[Path, A]] = Module.moduleMap(Module.moduleOverItself(ring))
      
      def add(x: GPAElement[A], y: GPAElement[A]) = GPAElement(disk, mapModule.add(x.coefficients, y.coefficients))
      def negate(x: GPAElement[A]) = GPAElement(disk, mapModule.negate(x.coefficients))
      def zero = GPAElement(disk, mapModule.zero)
      def scalarMultiply(a: A, x: GPAElement[A]) = GPAElement(disk, mapModule.scalarMultiply(a, x.coefficients))
    }
  }
  override def basis(disk: ChiralDisk) = (for (cycle <- cycles(disk)) yield {
    GPAElement(disk, Map(cycle -> ring.one))
  }).toSeq

  private def states(tangle: ChiralTangle, cycle: Path): Iterator[Seq[Path]] = {
    // we can only do this for connected tangles.
    // maybe one day we can add extra information to the PlanarTangle trait that makes it possible to do this in general
    require(tangle.isInstanceOf[ConnectedPlanarTangle[_, _]])

    val initialPartialState = for ((boundary, disk) <- tangle.disks.zipWithIndex) yield for (point <- (0 until boundary.size).toList) yield {
      (disk, point) match {
        case (0, k) => Some(cycle.edges(k))
        case (d, k) => tangle.matchedPoint(d, k) match {
          case (0, k) => Some(cycle.edges(k).reverse)
          case _ => None
        }
      }
    }

    def leafIterator[A, B](f: A => Either[Iterator[A], B])(root: A): Iterator[B] = {
      val iteratorStack = scala.collection.mutable.Stack[Iterator[A]](Iterator(root))
      var nextB: Option[B] = None

      def lookAhead = {
        while (nextB.isEmpty && iteratorStack.nonEmpty) {
          if (iteratorStack.head.hasNext) {
            f(iteratorStack.head.next) match {
              case Left(newIterator) => {
                if(!iteratorStack.head.hasNext) {
                  iteratorStack.pop
                }
                iteratorStack.push(newIterator)
              }
              case Right(b) => nextB = Some(b)
            }
          } else {
            iteratorStack.pop
          }
        }
      }

      lookAhead

      new Iterator[B] {
        def hasNext = nextB.nonEmpty
        def next = {
          val result = nextB.get
          nextB = None
          lookAhead
          result
        }
      }
    }

    def extendPartialState(state: Seq[List[Option[Edge]]]): Iterator[Seq[Path]] = {

      def next(state: Seq[List[Option[Edge]]]): Either[Iterator[Seq[List[Option[Edge]]]], Seq[Path]] = {
        import net.tqft.toolkit.arithmetic.Mod._
        import net.tqft.toolkit.collections.Position._
        import net.tqft.toolkit.collections.Iterators._
        state.position({ case None => true }).headOption match {
          case None => {
            // this state is complete, package it up
            // FIXME; if any of the internal disks are empty this will break; it assumes those disks are near the outer basepoint
            Right(state.map(c => Path(c.headOption.map(_.get.start).getOrElse(cycle.start), c.map(_.get))))
          }
          case Some(disk :: point :: Nil) => {
            // consider all possible edges we could assign at this point
            // first, locate the relevant spaghetti
            val (otherDisk, otherPoint) = tangle.matchedPoint(disk, point)
            // next find constraints from adjacent edges either at this end or the other end of the spaghetti
            val thisPreviousEdge = state(disk)((point - 1) mod state(disk).size)
            val thisNextEdge = state(disk)((point + 1) mod state(disk).size)
            val otherPreviousEdge = state(otherDisk)((otherPoint + 1) mod state(otherDisk).size)
            val otherNextEdge = state(otherDisk)((otherPoint - 1) mod state(otherDisk).size)
            // make sure the constraints are compatible
            if (thisPreviousEdge.nonEmpty && otherPreviousEdge.nonEmpty && thisPreviousEdge != otherPreviousEdge) {
              Left(Iterator.empty)
            } else {
              if (thisNextEdge.nonEmpty && otherNextEdge.nonEmpty && thisNextEdge != otherNextEdge) {
                Left(Iterator.empty)
              } else {
                // now consider all possible new edges
                val previousEdge = thisPreviousEdge.orElse(otherPreviousEdge)
                val nextEdge = thisNextEdge.orElse(otherNextEdge)
                val over = tangle.disks(disk).strands(point)
                val alternatives: Iterator[Edge] = (previousEdge.map(_.start), nextEdge.map(_.finish)) match {
                  case (None, None) => {
                    combinatorics.edgesOver(over)
                  }
                  case (Some(vp), None) => {
                    ???
                    //                  vp.edgesLeaving(over)
                  }
                  case (None, Some(vn)) => {
                    ???
                    //                  vn.edgesLeaving(over).map(_.reverse)
                  }
                  case (Some(vp), Some(vn)) => {
                    combinatorics.edgesBetween(vp, vn, over).iterator
                  }
                }
                val updatedStates = for (a <- alternatives) yield {
                  val updated1 = state.updated(disk, state(disk).updated(point, Some(a)))
                  val updated2 = updated1.updated(otherDisk, updated1(otherDisk).updated(otherPoint, Some(a.reverse)))
                  require(updated2 != state)
                  updated2
                }
                Left(updatedStates)
              }
            }
          }
        }
      }

      leafIterator(next _)(state)
    }

    extendPartialState(initialPartialState)
  }
  private def criticalPointCoefficient(vertex: Vertex, sign: Int) = {
    val d = combinatorics.dimension(vertex)
    sign match {
      case 1 => d
      case -1 => dimensionField.inverse(d)
      case _ => throw new IllegalArgumentException
    }
  }
  private def criticalPointCoefficient(strand: Spaghetti[ChiralStrand], state: Seq[Path]): F = {
    val initialEdge = state(strand.initialDisk).edges(strand.initialPoint)
    (strand.initialTangentDirection, strand.curvature) match {
      case (_, 0) => dimensionField.one
      case (1, 1) => {
        // a right cap
        criticalPointCoefficient(initialEdge.start, -1)
      }
      case (1, -1) => {
        // a left cap
        criticalPointCoefficient(initialEdge.finish, -1)
      }
      case (-1, 1) => {
        // a left cup
        criticalPointCoefficient(initialEdge.finish, 1)
      }
      case (-1, -1) => {
        // a right cup
        criticalPointCoefficient(initialEdge.start, 1)
      }
      case (d, c) => {
        dimensionField.multiply(
          criticalPointCoefficient(strand.copy(curvature = scala.math.signum(c)), state),
          criticalPointCoefficient(strand.copy(initialTangentDirection = -d, curvature = strand.curvature - scala.math.signum(c)), state))
      }
    }
  }
  private def criticalPointCoefficient(tangle: ChiralTangle, state: Seq[Path]): F = {
    val factors = for (strand <- tangle.spaghetti; if strand.curvature != 0) yield criticalPointCoefficient(strand, state)

    dimensionField.product(factors)
  }
  private def multiplyCoefficients(elements: Seq[Map[Path, A]], state: Seq[Path]) = {
    val coefficients = (elements zip state).map(p => p._1.get(p._2).getOrElse(ring.zero))
    ring.product(coefficients)
  }

  def cycles(disk: ChiralDisk): Iterator[Path] = {
    combinatorics.cycles(disk.basepointLabel._1, disk.basepointLabel._2, disk.strands)
  }

  override def act(tangle: ChiralTangle): Seq[GPAElement[A]] => GPAElement[A] = {
    // FIXME precompile to a (lazy) sparse matrix

    { elements: Seq[GPAElement[A]] =>

      println(combinatorics.objects(0,0).toList)
      println("computing cycles for: " + tangle.outerDisk)
      println(cycles(tangle.outerDisk).toList)
      
      val result = Map() ++ (for (
        cycle <- cycles(tangle.outerDisk);
        nothing = { println("cycle: " + cycle) };
        sum = ring.sum(
          for (state <- states(tangle, cycle); nothing = { println("state: " + state) }; x = multiplyCoefficients(elements.map(_.coefficients), state.tail); if x != ring.zero) yield {
            ring.scalarMultiply(criticalPointCoefficient(tangle, state), x)
          });
        if sum != ring.zero
      ) yield {
        cycle -> sum
      })

      GPAElement(tangle.outerDisk, result)
    }
  }
}

class LinearGraphPlanarAlgebra(combinatorics: FusionCombinatorics) extends GraphPlanarAlgebra[F](combinatorics, Algebra.fromRing[F](combinatorics.dimensionField)) with LinearTwoSidedPlanarAlgebra[Vertex, Int, F, Path, GPAElement[F]] {
  override val field = combinatorics.dimensionField

  override def coefficients(n: GPAElement[F]): Seq[F] = {
    // create SparseSeq objects?
    (for (cycle <- cycles(n.disk)) yield n.coefficients.get(cycle).getOrElse(ring.zero)).toSeq
  }

}

