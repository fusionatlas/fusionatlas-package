package org.fusionatlas.enumerators

import org.fusionatlas.graphs._
import net.tqft.toolkit.algebra.enumeration.Odometer
import org.fusionatlas.matrices._
import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.collections.RemoveDuplicates

object FusionEnumerator extends FusionEnumerator

trait FusionEnumerator extends Enumerator[FusionGraph] with Logging {
  def extend(d: Double, g: FusionGraph, maximumGlobalDimensionIncrease: Option[Double] = None): (List[FusionGraph], List[FusionGraph]) = {
    // not supported for now!
    require(maximumGlobalDimensionIncrease == None)

    val limit: AbstractFusionGraph => Boolean = { _.isFPEigenvaluePossiblyBelow(d) }

    def addDualVerticesAtMaximalDepth(g1: IncompleteFusionGraph): Iterator[IncompleteFusionGraph] = {
      info("Adding a pair of dual vertices to " + g1)
      val addOneRow = addSingleVertexAtMaximalDepth(g1, true)
      addOneRow flatMap { g2 => addSingleVertexAtMaximalDepth(g2, false) }
    }

    def addSingleVertexAtMaximalDepth(g1: IncompleteFusionGraph, selfDual: Boolean = true): Iterator[IncompleteFusionGraph] = {
      Odometer[IncompleteFusionGraph](limit)(IncompleteFusionGraph(
        new RectangularMatrix(List.fill(g1.rankAtDepth(g1.graphDepth - 1))(0), g1.inclusions.head) :: g1.inclusions.tail,
        new SymmetricMatrix(g1.sameDepth.head.size + 1) :: g1.sameDepth.tail,
        (if(selfDual) { g1.dualData.head :+ g1.dualData.size } else { g1.dualData.head.init :+ g1.dualData.size :+ (g1.dualData.size - 1) }) :: g1.dualData.tail)).filter({
        _.inclusions.head.connected_?
      }).filter({ g2 =>
        require(g2.rankAtMaximalDepth == g1.rankAtMaximalDepth + 1)
        true
      })
    }

    val dualPairExtensions: Iterable[IncompleteFusionGraph] = {
      info("Adding pairs of dual vertices to " + g)
      val g0 = IncompleteFusionGraph(g)
      val byRank = NonStrictIterable.iterate(NonStrictIterable(g0))({ l: Iterable[IncompleteFusionGraph] =>
        l flatMap {
          addDualVerticesAtMaximalDepth _
        }
      }) takeWhile { _.nonEmpty }
      byRank.flatten
    }

    val incompleteExtensions: Iterable[IncompleteFusionGraph] = {
      val byRank = NonStrictIterable.iterate(dualPairExtensions)({ l: Iterable[IncompleteFusionGraph] =>
        l flatMap {
          info("Adding a self-dual vertex to " + l)
          addSingleVertexAtMaximalDepth(_)
        }
      }) takeWhile { _.nonEmpty }
      byRank.flatten filter { _.rankAtMaximalDepth > 0 }
    }

    val allExtensions: Iterable[FusionGraph] = incompleteExtensions flatMap { f: IncompleteFusionGraph =>
      {
        info("Adding constant depth edges to " + f)
        Odometer[FusionGraph](limit)(FusionGraph(f))
      }
    }

    val associativeExtensions = allExtensions.filter(_.partialAssociativityTest)

    val vines = associativeExtensions.filter(_.fullAssociativityTest())

    def isomorphic(p1: FusionGraph, p2: FusionGraph) = p1.isIsomorphicTo(p2)

    import RemoveDuplicates._
    info("looking for unique graphs")
    (vines.toSeq.removeDuplicatesAndSort(isomorphic, FusionGraph.ordering),
      associativeExtensions.toSeq.removeDuplicatesAndSort(isomorphic, FusionGraph.ordering))

  }

}
