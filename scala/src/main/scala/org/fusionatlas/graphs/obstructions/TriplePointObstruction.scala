package org.fusionatlas.graphs.obstructions

import org.fusionatlas.graphs.PairOfBigraphsWithDuals
import org.fusionatlas.graphs.GraphVertex

object TriplePointObstruction {
  def apply(p: PairOfBigraphsWithDuals): Option[String] = {
    import net.tqft.toolkit.permutations._

    (for (
      depth <- (0 to p.gg0.graphDepth - 2); principalIndex <- (1 to p.gg0.rankAtDepth(depth));
      dualIndex <- (1 to p.gg1.rankAtDepth(depth));
      principalVertex = GraphVertex(p, 0, depth, principalIndex);
      dualVertex = GraphVertex(p, 1, depth, dualIndex) //Check if dim(Hom(X\otimes principalVertex\otimes X, dualVertex))==3, i.e., check for triple point
      if (p.sizeOfConnection(principalVertex, dualVertex) == 3);
      rightNeighboursOfPrincipalVertex = principalVertex.rightNeighbours;
      rightNeighboursOfDualVertex = dualVertex.rightNeighbours;
      if (rightNeighboursOfPrincipalVertex.size == 3);
      if (rightNeighboursOfDualVertex.size == 3);
      permutedRightNeighboursOfDualVertex <- Permutations.of(rightNeighboursOfDualVertex);
      if (((0 to 2) map { i => p.combinatorialSameDimensionsQ(rightNeighboursOfPrincipalVertex(i), permutedRightNeighboursOfDualVertex(i)) }).reduceLeft(_ && _));
      //see if dim(Hom(X\otimes A\otimes X, B))==1 for A,B neighbors of principalVertex and dualVertex
      if (p.sizeOfConnection(rightNeighboursOfPrincipalVertex(0), permutedRightNeighboursOfDualVertex(1)) == 1
        && p.sizeOfConnection(rightNeighboursOfPrincipalVertex(0), permutedRightNeighboursOfDualVertex(2)) == 1
        && p.sizeOfConnection(rightNeighboursOfPrincipalVertex(1), permutedRightNeighboursOfDualVertex(0)) == 1
        && p.sizeOfConnection(rightNeighboursOfPrincipalVertex(1), permutedRightNeighboursOfDualVertex(2)) == 1
        && p.sizeOfConnection(rightNeighboursOfPrincipalVertex(2), permutedRightNeighboursOfDualVertex(0)) == 1
        && p.sizeOfConnection(rightNeighboursOfPrincipalVertex(2), permutedRightNeighboursOfDualVertex(1)) == 1)
    ) yield ("Fails the triple point obstruction at " + (principalVertex, dualVertex))).headOption
  }

}