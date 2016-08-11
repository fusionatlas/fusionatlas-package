package org.fusionatlas.enumerators

import org.fusionatlas.graphs._
import net.tqft.toolkit.algebra.enumeration.Odometer
import org.fusionatlas.matrices._
import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.collections.RemoveDuplicates

object DirectedFusionEnumerator extends DirectedFusionEnumerator

/*
 * Note from Scott, May 17 2011: modifying this from the existing FusionEnumerator code
 */
trait DirectedFusionEnumerator extends Enumerator[DirectedFusionGraph] with Logging {
  def extend(d: Double, g: DirectedFusionGraph, maximumGlobalDimensionIncrease: Option[Double] = None): (List[DirectedFusionGraph], List[DirectedFusionGraph]) = {
    // not supported for now!
    require(maximumGlobalDimensionIncrease == None)

    val limit: AbstractDirectedFusionGraph => Boolean = { _.evenPart.isFPEigenvaluePossiblyBelow(d * d) }

    def addDualVerticesAtMaximalDepth(g1: NextDirectedFusionGraph): Iterator[NextDirectedFusionGraph] = {
      info("Adding a pair of dual vertices to " + g1)
      val addOneRow = addSingleVertexAtMaximalDepth(g1, true)
      addOneRow flatMap { g2 => addSingleVertexAtMaximalDepth(g2, false) }
    }

    def addSingleVertexAtMaximalDepth(g1: NextDirectedFusionGraph, selfDual: Boolean = true): Iterator[NextDirectedFusionGraph] = {
      // Scott, May 18 2011; I think this is right, but it's the most likely source of errors
      val addSingleVertex = NextDirectedFusionGraph(
        // add a row of zeros to the first matrix in nextDepth
        nextDepth = new RectangularMatrix(List.fill(g1.rankAtDepth(g1.graphDepth - 1))(0), g1.nextDepth.head) :: g1.nextDepth.tail,
        // fill in sameDepth with the appropriate size zero matrix
        sameDepth = new RectangularMatrix(g1.sameDepth.head.numberOfSources + 1, g1.sameDepth.head.numberOfTargets + 1) :: g1.sameDepth.tail,
        // fill in lastDepth with the appropriate size zero matrix
        lastDepth = new RectangularMatrix(g1.lastDepth.head.numberOfSources + 1, g1.lastDepth.head.numberOfTargets) :: g1.lastDepth.tail,
        dualData = (if(selfDual) { g1.dualData.head :+ g1.dualData.size } else { g1.dualData.head.init :+ g1.dualData.size :+ (g1.dualData.size - 1) }) :: g1.dualData.tail)
      val odometer = Odometer[NextDirectedFusionGraph](limit)(addSingleVertex)
      odometer filter {
        // make sure that we actually connected something to the new vertex (this filter probably has the same effect as .tail)
        _.nextDepth.head.connected_?
      } filter { g2 =>
        require(g2.rankAtMaximalDepth == g1.rankAtMaximalDepth + 1); true
      }
    }

    val dualPairExtensions: Iterable[NextDirectedFusionGraph] = {
      info("Adding pairs of dual vertices to " + g)
      val g0 = NextDirectedFusionGraph(g)
      val byRank = NonStrictIterable.iterate(NonStrictIterable(g0))({ l: Iterable[NextDirectedFusionGraph] =>
        l flatMap {
          addDualVerticesAtMaximalDepth _
        }
      }) takeWhile { _.nonEmpty }
      byRank.flatten
    }

    val nextExtensions: Iterable[NextDirectedFusionGraph] = {
      val byRank = NonStrictIterable.iterate(dualPairExtensions)({ l: Iterable[NextDirectedFusionGraph] =>
        l flatMap {
          info("Adding a self-dual vertex to " + l)
          addSingleVertexAtMaximalDepth(_)
        }
      }) takeWhile { _.nonEmpty }
      byRank.flatten filter { _.rankAtMaximalDepth > 0 }
    }

    val lastExtensions: Iterable[LastDirectedFusionGraph] = nextExtensions flatMap { f: NextDirectedFusionGraph =>
      {
        require(f.rankAtMaximalDepth > 0)
        info("adding edges back to last depth to " + f)
        Odometer[LastDirectedFusionGraph](limit)(LastDirectedFusionGraph(f))
      }
    }

    val allExtensions: Iterable[DirectedFusionGraph] = lastExtensions flatMap { f: LastDirectedFusionGraph =>
      {
        info("Adding constant depth edges to " + f)
        Odometer[DirectedFusionGraph](limit)(DirectedFusionGraph(f))
      }
    }

    val associativeExtensions = allExtensions.filter(_.partialAssociativityTest)

    val vines = associativeExtensions.filter(_.fullAssociativityTest())

    def isomorphic(p1: DirectedFusionGraph, p2: DirectedFusionGraph) = p1.isIsomorphicTo(p2)

    import RemoveDuplicates._
    info("looking for unique graphs")
    (vines.toSeq.removeDuplicatesAndSort(isomorphic, DirectedFusionGraph.ordering),
      associativeExtensions.toSeq.removeDuplicatesAndSort(isomorphic, DirectedFusionGraph.ordering))

  }

}
