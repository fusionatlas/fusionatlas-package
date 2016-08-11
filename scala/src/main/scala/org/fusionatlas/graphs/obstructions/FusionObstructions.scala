package org.fusionatlas.graphs.obstructions

import org.fusionatlas.graphs.FusionGraph
import org.fusionatlas.graphs.PersistentGraph
import org.fusionatlas.enumerators.Norms
import org.fusionatlas.enumerators.Classifier

trait FusionObstructions extends Obstructions[FusionGraph] {
 import Norms._	
	
  val obstructions = List(triplePoint _, selfLoop _, tinyVertexLemma _)

  def triplePoint(f: FusionGraph): Option[(String, List[FusionGraph], List[(Double, FusionGraph)])] = {
    if (d <= 2) return None
    (for (
      depth <- (0 to f.graphDepth - 1); index <- (1 to f.rankAtDepth(depth));
      vertex = (depth, index) if (f.dual(vertex) == vertex);
      neighbours = f.neighbours(vertex) if (neighbours.size == 3) if (f.sizeOfConnection(neighbours(0), neighbours(1)) == 1
        && f.sizeOfConnection(neighbours(1), neighbours(2)) == 1
        && f.sizeOfConnection(neighbours(0), neighbours(2)) == 1)
    ) yield ("Fails the triple point obstruction at " + vertex, Nil, Nil)).headOption
  }

  def selfLoop(f: FusionGraph): Option[(String, List[FusionGraph], List[(Double, FusionGraph)])] = {
    if (d <= 2) return None
    (for (
      depth <- (0 to f.graphDepth - 1); index <- (1 to f.rankAtDepth(depth));
      vertex = (depth, index) if (f.dual(vertex) == vertex);
      neighbours = f.neighbours(vertex) if (neighbours.size == 2);
      if (neighbours.contains(vertex));
      other = neighbours.filterNot(_ == vertex).head if (f.dual(other) == other)
    ) yield ("Fails the self-loop test at " + vertex, Nil, Nil)).headOption
  }

  def intermediateSubfactorLemma(f: FusionGraph): Option[(String, List[FusionGraph], List[(Double, FusionGraph)])] = {
	if(d >= sqrt5 - epsilon) return None
	  
    val univalent1ST = List(
      FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0v1v0p0x0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v1v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v1v0p0x0p0x0x0v1x0x0p0x1x0v1v0p0x0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0")
      )

    univalent1ST find (_.isIsomorphicTo(f)) map (w => ("1ST graphs below \\sqrt{5} give subfactors with impossible intermediates.", Nil, List((d, w.translate(1)))))
  }
  
  def tinyVertexLemma(f: FusionGraph): Option[(String, List[FusionGraph], List[(Double, FusionGraph)])] = {
    val quadruplePointsWithTinyVertex = List(
      FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0v1x0p0x1v0v0p0x0v1x0v0v0"),
      FusionGraph("fg0v0v1v0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0v1x0p0x1v0v0p0x0v1x0v0v0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0v1x0v0v0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v1v0p0x0p0x0x0v0x0x1v0v0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v1v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v1v0p0x0p0x0x0v0x0x1v0v0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v1v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v0v0p0x0p0x0x0v0x0x1v0v0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v0v0p0x0p0x0x0v1x0x0v0v0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0v1x0v0v0"),
      FusionGraph("fg0v0v1v0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0v1x0v0v0"),
      FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0v1x0p0x1v0v0p0x0v1x0v0v0"),
      FusionGraph("fg0v0v1v0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0v1x0p0x1v0v0p0x0v1x0v0v0"),
      FusionGraph("fg0v0v1v0v0v1v0v0v1v0v0v1p1p1v0v0p0x0p0x0x0v1x0x0p0x1x0p0x0x1v0v0p0x0p0x0x0v1x0x0p0x1x0v0v0p0x0v1x0p0x1v0v0p0x0v1x0v0v0")
      )

    quadruplePointsWithTinyVertex find (_.isIsomorphicTo(f)) map (w => ("At this translation, there's an object that always has dimension less than 1.", Nil, List((d, w.translate(1)))))
  }
}