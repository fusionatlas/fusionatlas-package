package org.fusionatlas.enumerators

import org.fusionatlas.matrices._
import net.tqft.toolkit.algebra.enumeration.Odometer
import org.fusionatlas.graphs._
import org.fusionatlas.permutations.Involutions
import org.fusionatlas.permutations.Involutions._
import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.collections.RemoveDuplicates
import scala.collection.GenIterable

object PairEnumerator extends PairEnumerator

trait PairEnumerator extends Enumerator[PairOfBigraphsWithDuals] with Logging {

  private def reverseVerticesAtMaximalDepth(h: Bigraph) = Bigraph(new RectangularMatrix(h.inclusions.head.asArray.reverse) :: h.inclusions.tail)

  // extend and extendBackwards (for Bigraphs) give the same answers, work differently, but seem similarly fast (or at least, neither is the bottleneck)
  def extendBigraph(d: Double, g: Bigraph, onlyRowsOrdered: Boolean): Iterable[Bigraph] = {
    val limit: Bigraph => Boolean = { _.isFPEigenvaluePossiblyBelow(d) }
    if (!g.isFPEigenvaluePossiblyBelow(d)) return NonStrictIterable()

    def increaseRankAtMaximalDepth(g1: Bigraph): Iterator[Bigraph] = {
      if (onlyRowsOrdered && !g1.inclusions.head.asArray.isEmpty) {
        val newRow = g1.inclusions.head.asArray.head
        Odometer(limit)(Bigraph(new RectangularMatrix(newRow, g1.inclusions.head) :: g1.inclusions.tail))
      } else {
        val newRow = List.fill(g1.rankAtDepth(g1.graphDepth - 1))(0)
        val odometer = Odometer(limit)(Bigraph(new RectangularMatrix(newRow, g1.inclusions.head) :: g1.inclusions.tail))
        odometer.next // discard
        odometer
      }
    }

    val extensionsByRank: Iterable[Iterable[Bigraph]] = {
      // first, increase the depth, but with 0 vertices
      val g0 = Bigraph(g, new RectangularMatrix(g.rankAtDepth(g.graphDepth)))
      NonStrictIterable.iterate(NonStrictIterable(g0))({ _ flatMap { increaseRankAtMaximalDepth _ } }).tail // we need tail here to drop the 0 vertex case
    }

    // now just collapse all the different numbers of vertices
    val result = extensionsByRank.takeWhile(!_.isEmpty).flatten

    // finally reverse the vertices we've just added, so there are fewer crossings.
    result.map(reverseVerticesAtMaximalDepth(_))
  }
  private def extendBackwards(d: Double, g: Bigraph, onlyRowsOrdered: Boolean, maximumGlobalDimensionIncrease: Option[Double]): Iterable[Bigraph] = {
    def normLimit(h: Bigraph) = h.isFPEigenvaluePossiblyBelow(d)
    def normAndGlobalDimensionLimit(h: Bigraph) = normLimit(h) && {
      maximumGlobalDimensionIncrease match {
        case None => true
        case Some(increase) => {
          // ACHTUNG there's a hardcoded 5 here, because we already know all subfactors up to index 5!
          val index = math.max(5, h.FPEigenvalueLowerBounds(10) * h.FPEigenvalueLowerBounds(10))
          h.rankAtMaximalDepth * index <= increase
        }
      }
    }

    if (!g.isFPEigenvaluePossiblyBelow(d)) return NonStrictIterable()

    def orderingLimit(rowLimit: List[Int])(h: Bigraph) = {
      import Ordering.Implicits._
      (h.inclusions.head.asArray.head <= rowLimit) && normAndGlobalDimensionLimit(h)
    }

    def increaseRankAtMaximalDepth(g1: Bigraph): Iterator[Bigraph] = {
      val newRow = List.fill(g1.rankAtDepth(g1.graphDepth - 1))(0)
      val limit = if (onlyRowsOrdered && !g1.inclusions.head.asArray.isEmpty) {
        orderingLimit(g1.inclusions.head.asArray.head) _
      } else {
        normAndGlobalDimensionLimit _
      }
      val g2 = Bigraph(new RectangularMatrix(newRow, g1.inclusions.head) :: g1.inclusions.tail)

      if(limit(g2)) {
        val odometer = Odometer(limit)(g2)
        odometer.next // discard
        odometer
      } else {
        Iterator.empty
      }
    }

    val extensionsByRank: Iterable[Iterable[Bigraph]] = {
      // first, increase the depth, but with 0 vertices
      val g0 = Bigraph(g, new RectangularMatrix(g.rankAtDepth(g.graphDepth)))
//      NonStrictIterable.iterate(NonStrictIterable(g0))({ _ flatMap { increaseRankAtMaximalDepth _ } }).tail // we need tail here to drop the 0 vertex case
      NonStrictIterable.iterate[Iterable[Bigraph]](List(g0))({ _.par.flatMap({ increaseRankAtMaximalDepth _ }).toList }).tail // we need tail here to drop the 0 vertex case
    }

    // now just collapse all the different numbers of vertices
    val result = extensionsByRank.takeWhile(!_.isEmpty).flatten

    // finally reverse the vertices we've just added, so there are fewer crossings.
    result.map(reverseVerticesAtMaximalDepth(_))
  }

  private def extendBigraphWithDuals(d: Double, g: BigraphWithDuals, rowsOrdered: Boolean, maximumGlobalDimensionIncrease: Option[Double]): Iterable[BigraphWithDuals] = {
    if (g.graphDepth % 2 == 0) {
      // We're adding an odd depth, no need to come up with an involution.
      extendBackwards(d, g.bigraph, rowsOrdered, maximumGlobalDimensionIncrease) map ((g1: Bigraph) => new BigraphWithDuals(g1, g.involutions))
    } else {
      addEvenDepth1(d, g, rowsOrdered, maximumGlobalDimensionIncrease)
    }
  }

  private def addEvenDepth1(d: Double, g: BigraphWithDuals, rowsOrdered: Boolean, maximumGlobalDimensionIncrease: Option[Double]): Iterable[BigraphWithDuals] = {
    // We're adding an even depth, try all involutions.
    require(g.graphDepth % 2 == 1)

    extendBackwards(d, g.bigraph, rowsOrdered, maximumGlobalDimensionIncrease) flatMap ((g1: Bigraph) => {

      val involutions = if (rowsOrdered) {
        import net.tqft.toolkit.collections.Split._
        val chunks = g1.inclusions.head.asArray.rle.map(_._2)
        Involutions.ofChunks(chunks)
      } else {
        Involutions.of(g1.rankAtMaximalDepth)
      }
      (involutions map (i => new BigraphWithDuals(g1, i :: g.involutions)))
    })
  }

//  // mysteriously, significantly slower than addEvenDepth1 above :-(
//  private def addEvenDepth2(d: Double, g: BigraphWithDuals, rowsOrdered: Boolean): Iterable[BigraphWithDuals] = {
//    require(g.graphDepth % 2 == 1)
//    if (!g.bigraph.isFPEigenvaluePossiblyBelow(d)) return NonStrictIterable()
//
//    implicit val normLimit: Bigraph => Boolean = { _.isFPEigenvaluePossiblyBelow(d) }
//
//    def addDualVerticesAtMaximalDepth(g1: BigraphWithDuals): Iterable[BigraphWithDuals] = {
//      //      info("Adding a pair of dual vertices to " + g1)
//      val addOneRow = addSingleVertexAtMaximalDepth(g1, true, 0)
//      addOneRow flatMap { g2 => addSingleVertexAtMaximalDepth(g2, true, 1) }
//    }
//
//    def addSingleVertexAtMaximalDepth(g1: BigraphWithDuals, partOfDualPair: Boolean = false, dualDataIncrement: Int = 0): Iterable[BigraphWithDuals] = {
//      val newInvolutions = dualDataIncrement match {
//        case 0 => (1 :: g1.involutions.head.map(_ + 1)) :: g1.involutions.tail
//        case 1 => (List(2, 1) ::: g1.involutions.head.tail.map(_ + 1)) :: g1.involutions.tail
//      }
//
//      val zeroRow = List.fill(g1.rankAtDepth(g1.graphDepth - 1))(0)
//      val comparisonRow = if (rowsOrdered) {
//        (partOfDualPair, dualDataIncrement) match {
//          case (false, 0) => if (Involutions.countPairs(g1.involutions.head) * 2 == g1.bigraph.rankAtMaximalDepth) zeroRow else g1.bigraph.inclusions.head.asArray.head
//          case (true, 0) => if (g1.bigraph.rankAtMaximalDepth == 0) zeroRow else g1.bigraph.inclusions.head.asArray(1)
//          case (true, 1) => g1.bigraph.inclusions.head.asArray.head
//        }
//      } else {
//        zeroRow
//      }
//
//      val comparisonRow2 = if (rowsOrdered) {
//        (partOfDualPair, dualDataIncrement) match {
//          case (false, 0) => if (Involutions.countPairs(g1.involutions.head) * 2 == g1.bigraph.rankAtMaximalDepth) None else Some(g1.bigraph.inclusions.head.asArray.head)
//          case (true, 0) => if (g1.bigraph.rankAtMaximalDepth == 0) None else Some(g1.bigraph.inclusions.head.asArray(1))
//          case (true, 1) => Some(g1.bigraph.inclusions.head.asArray.head)
//        }
//      } else {
//        None
//      }
//
//      def orderingLimit(rowLimit: Option[List[Int]])(h: Bigraph) = {
//        import net.tqft.toolkit.collections.LexicographicOrdering._
//        (!rowsOrdered || rowLimit.isEmpty || h.inclusions.head.asArray.head <= rowLimit.get) && normLimit(h)
//      }
//
//      Odometer(
//        Bigraph(new RectangularMatrix(zeroRow, g1.bigraph.inclusions.head) :: g1.bigraph.inclusions.tail))(x => x, orderingLimit(comparisonRow2)_) filter {
//          _.inclusions.head.connected_?
//        } map { new BigraphWithDuals(_, newInvolutions) }
//    }
//
//    val dualPairExtensions: Iterable[BigraphWithDuals] = {
//      //      info("Adding pairs of dual vertices to " + g)
//      val g0 = new BigraphWithDuals(Bigraph(g.bigraph, new RectangularMatrix(g.rankAtDepth(g.graphDepth))), Nil :: g.involutions)
//      val byRank = NonStrictIterable.iterate(NonStrictIterable(g0))({ l: Iterable[BigraphWithDuals] =>
//        l flatMap {
//          addDualVerticesAtMaximalDepth _
//        }
//      }) takeWhile { _.nonEmpty }
//      byRank.flatten
//    }
//
//    val completeExtensions: Iterable[BigraphWithDuals] = {
//      val byRank = NonStrictIterable.iterate(dualPairExtensions)({ l: Iterable[BigraphWithDuals] =>
//        l flatMap { x =>
//          //          info("Adding a self-dual vertex to " + x)
//          addSingleVertexAtMaximalDepth(x)
//        }
//      }) takeWhile { _.nonEmpty }
//      byRank.flatten filter { _.bigraph.rankAtMaximalDepth > 0 }
//    }
//
//    completeExtensions
//
//  }

  def extend(d: Double, g: PairOfBigraphsWithDuals, maximumGlobalDimensionIncrease: Option[Double] = None): (List[PairOfBigraphsWithDuals], List[PairOfBigraphsWithDuals]) /* (vines, weeds) */ = {
    require(g.gg0.graphDepth == g.gg1.graphDepth)

    import net.tqft.toolkit.{ SerializedSoftReferenceIterable, ChunkedSoftReferencedIterableBuilder }
    import net.tqft.toolkit.collections.GroupBy._

    val thingsToRelease = scala.collection.mutable.ListBuffer[SerializedSoftReferenceIterable[_]]()

    var counter1 = 0
    var counter2 = 0

    val (vinesWithDuplicates, weedsWithDuplicates) = {
      info("Looking for extensions of " + g + " out to norm " + d)

      val (vinesWithDuplicates, w1grouped, w2grouped) = {
        val ((v1, w1), (v2, w2)) = {
          def g1extensions = extendBigraphWithDuals(d, g.gg0, true, maximumGlobalDimensionIncrease) filter { h: BigraphWithDuals => new PairOfBigraphsWithDuals(h, g.gg1).leftExtensionAssociativityTest }
          def g2extensions = extendBigraphWithDuals(d, g.gg1, g.gg1.graphDepth % 2 == 1, maximumGlobalDimensionIncrease) filter { h: BigraphWithDuals => new PairOfBigraphsWithDuals(g.gg0, h).rightExtensionAssociativityTest }

          if (g.gg0.graphDepth % 2 == 0) {
            // we're adding an odd depth
            ((List(), g1extensions),
              (List(), g2extensions))
          } else {
            // we're adding an even depth
            (g1extensions partition ((g1e: BigraphWithDuals) => new PairOfBigraphsWithDuals(g1e, g.gg1).fullOcneanuTest),
              g2extensions partition ((g2e: BigraphWithDuals) => new PairOfBigraphsWithDuals(g.gg0, g2e).fullOcneanuTest))
          }
        }

        val vines0 = NonStrictIterable(g) filter { _.fullOcneanuTest }
        val vines1 = v1 map (g1e => new PairOfBigraphsWithDuals(g1e, g.gg1))
        val vines2 = v2 map (g2e => new PairOfBigraphsWithDuals(g.gg0, g2e))

        implicit val serializedIterableBuilder = () => new ChunkedSoftReferencedIterableBuilder[(BigraphWithDuals, List[List[Int]])](500, BigraphWithDuals.getClass.getClassLoader)

        ((vines0 ++ vines1 ++ vines2) map { _.sorted },
          w1.map(h => (h, new PairOfBigraphsWithDuals(h, g.gg1).leftExtensionOddOddMultiplicities)).groupByWithCustomBuilder(p => (p._1.bigraph.annularMultiplicitiesIfAvailable)),
          w2.map(h => (h, new PairOfBigraphsWithDuals(g.gg0, h).rightExtensionOddOddMultiplicities)).groupByWithCustomBuilder(p => (p._1.bigraph.annularMultiplicitiesIfAvailable)))
      }

      for (chunkIterable <- w1grouped.values ++ w2grouped.values; iterable <- chunkIterable) {
        thingsToRelease += iterable
      }

      val annularMultiplicities = NonStrictIterable.from(w1grouped.keySet.intersect(w2grouped.keySet).toList)

      (vinesWithDuplicates,
        for {
          annularMultiplicity <- annularMultiplicities
          w1chunks = w1grouped.getOrElse(annularMultiplicity, List())
          w2chunks = w2grouped.getOrElse(annularMultiplicity, List())
          fewerChunks = if (w1chunks.size < w2chunks.size) w1chunks else w2chunks
          moreChunks = if (w1chunks.size < w2chunks.size) w2chunks else w1chunks
          chunk1 <- moreChunks
          chunk2 <- fewerChunks
          split1 = chunk1.groupBy(_._2)
          split2 = chunk2.groupBy(_._2)
          oddOddMultiplicity <- split1.keySet ++ split2.keySet
          (g1e, _) <- split1.getOrElse(oddOddMultiplicity, Nil)
          (g2e, _) <- split2.getOrElse(oddOddMultiplicity, Nil)
          if (g1e.graphDepth % 2 == 0 || g1e.bigraph.rankAtMaximalDepth == g2e.bigraph.rankAtMaximalDepth)
          pair = { counter1 = counter1 + 1; new PairOfBigraphsWithDuals(g1e, g2e) }
          if pair.partialOcneanuTest
        } yield {
          counter2 = counter2 + 1;
          if (counter2 % 1000 == 0) info(". considered " + counter1 + " pairs of which " + counter2 + " passed the associativity test")
          pair.sorted
        })

    }

    info("looking for unique vines")
    val vines = PairOfBigraphsWithDuals.removeDuplicates(vinesWithDuplicates).map(_.permuteToReduceCrossings())
    info("looking for unique weeds")
    val weeds = PairOfBigraphsWithDuals.removeDuplicates(weedsWithDuplicates).map(_.permuteToReduceCrossings())

    info(".finished removing isomorphic duplicates, found " + vines.size + " vines and " + weeds.size + " weeds.")
    info(".considered " + counter1 + " pairs of which " + counter2 + " passed the associativity test")

    for (thingToRelease <- thingsToRelease) {
      thingToRelease.release
    }

    return (vines, weeds)
  }

}
