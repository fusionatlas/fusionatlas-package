package org.fusionatlas.graphs

import org.fusionatlas.matrices.RectangularMatrix
//import org.fusionatlas.matrices.SymmetricMatrix

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class DirectedFusionGraphTest extends FlatSpec with ShouldMatchers {

  // dictionary: v -> ;  p-> ],[  x-> ,
  //DirectedFusionGraphs:
  val DirectedAffineA1String = "dfg 0;[[0]];[[1]];[[1]];0;[[0]]"
  val DirectedAffineA1 = DirectedFusionGraph(DirectedAffineA1String)
  val DirectedAffineA3String = "dfg 0;[[0]];[[1],[0]];[[0,1]];1;[[0,0],[0,0]];[[1,0]];[[0],[1]];0;[[0]]"
  val DirectedAffineA3 = DirectedFusionGraph(DirectedAffineA3String)
  val SU3AtLevel2String = "dfg 0;[[0]];[[1],[0]];[[0,1]];1;[[0,0],[1,0]];[[1,0],[0,0],[0,1]];[[0,0,1],[0,1,0]];1;[[0,0,0],[0,0,1],[1,0,0]]"
  val SU3AtLevel2 = DirectedFusionGraph(SU3AtLevel2String)
  val SU3AtLevel4String = "dfg 0;[[0]];" + 
  							"[[1],[0]];[[0,1]];1;[[0,0],[1,0]];" +
  							"[[1,0],[0,0],[0,1]];[[0,0,1],[0,1,0]];1;[[0,0,0],[0,0,1],[1,0,0]];" +
	  						"[[1,0,0],[0,0,0],[0,0,1],[0,1,0]];[[0,0,1,0],[0,1,0,0],[0,0,0,1]];2;[[0,0,0,0],[0,0,0,1],[1,0,0,0],[0,0,1,0]];" + 
	  						"[[1,0,0,0],[0,0,0,0],[0,0,1,0],[0,1,0,0],[0,0,0,1]];[[0,0,1,0,0],[0,1,0,0,0],[0,0,0,0,1],[0,0,0,1,0]];2;[[0,0,0,0,0],[0,0,0,1,0],[1,0,0,0,0],[0,0,0,0,1],[0,0,1,0,0]]"// FIXME
  val SU3AtLevel4 = DirectedFusionGraph(SU3AtLevel4String)
  
//  val SU3AtLevel4 = DirectedFusionGraph("" /* FIXME maybe this is too much work */)
  val testMatrix = (new RectangularMatrix("[[0,1,0],[0,0,1]]"))

  // TODO we might want to allow a more general notation, which allows us to have arbitrary dual date (i.e. not just the pairs first)
  // this would be nice for drawing pictures, but is low priority
  // these wouldn't need to support many operations --- essentially just conversion to and from the existing notation.
  
  "RectangularMatrix" should "be able to parse the above notation" in {
    testMatrix.asArray should equal(List(List(0, 1, 0), List(0, 0, 1)))
    testMatrix.niceToString should equal("[[0,1,0],[0,0,1]]")
  }

  "A DirectedFusionGraph" should "convert to and from string notation" in {
    DirectedAffineA1.toString should equal(DirectedAffineA1String)
    DirectedAffineA3.toString should equal(DirectedAffineA3String)
    SU3AtLevel2.toString should equal(SU3AtLevel2String)
  }

  "A DirectedFusionGraph" should "correctly report its rank at each depth" in {
    (0 to DirectedAffineA3.graphDepth) map { DirectedAffineA3.rankAtDepth(_) } should equal(List(1, 2, 1))
    (0 to SU3AtLevel2.graphDepth) map { SU3AtLevel2.rankAtDepth(_) } should equal(List(1, 2, 3))
  }

  "A DirectedFusionGraph" should "report the neighbours of a vertex" in {
    DirectedAffineA3.neighbours(0, 1) should equal(List((1, 1)))
    DirectedAffineA3.neighbours(1, 1) should equal(List((2, 1)))
    DirectedAffineA3.neighbours(2, 1) should equal(List((1, 2)))
    SU3AtLevel2.neighbours(1, 1) should equal(List((1, 2), (2, 1)))
    SU3AtLevel2.neighbours(2, 2) should equal(List((1, 2)))
    SU3AtLevel2.neighbours(2, 3) should equal(List((1, 1), (2, 2)))
  }
  
  "A DirectedFusionGraph" should "report the X^* neighbours of a vertex" in {
    DirectedAffineA3.neighboursXdual(0, 1) should equal(List((1, 2)))
    DirectedAffineA3.neighboursXdual(1, 1) should equal(List((0, 1)))
    DirectedAffineA3.neighboursXdual(2, 1) should equal(List((1, 1)))
    SU3AtLevel2.neighboursXdual(1, 1) should equal(List((0, 1), (2, 3)))
    SU3AtLevel2.neighboursXdual(2, 2) should equal(List((2, 3)))
    SU3AtLevel2.neighboursXdual(2, 3) should equal(List((1, 2), (2, 1)))
  }

  "evenPart" should "construct a FusionGraph, with generator X (x) X^*" in {
    SU3AtLevel2.evenPart.isIsomorphicTo(FusionGraph("fg0v0v1v0v1" /* this is T2 */ )) should equal(true)
    DirectedAffineA1.evenPart.isIsomorphicTo(FusionGraph("fg0v0")) should equal(true)
    DirectedAffineA3.evenPart.isIsomorphicTo(FusionGraph("fg0v0")) should equal(true)
    SU3AtLevel4.evenPart.isIsomorphicTo(FusionGraph("fg0v0v1v0v2v1p1p1v1v1p0x1p1x1x1")) should equal(true)
  }

// TODO: this might be nice at some point.
  //  "toPairOfBigraphsWithDuals" should "construct the corresponding subfactor" in {
//    SU3AtLevel4.toPairOfBigraphsWithDuals.isIsomorphicTo(
//      PairOfBigraphsWithDuals("bwd1v1v1p1v1x0p0x1p0x1v0x1x0p1x0x1duals1v1v2x1x3",
//        "bwd1v1v1p1v1x0p0x1p0x1v0x1x0p1x0x1duals1v1v2x1x3")) should equal(true)
//  }

  "findIsomorphism" should "find the correct number of isomorphisms between graphs" in {
    SU3AtLevel2.findIsomorphisms(SU3AtLevel2).size should equal(1)
    SU3AtLevel4.findIsomorphisms(SU3AtLevel4).size should equal(1)
    val K1 = DirectedFusionGraph("dfg 0;[[0]];[[1],[0]];[[0,1]];1;[[0,0],[0,0]];[[1,0],[1,0]];[[0,0],[1,1]];0;[[0,0],[0,0]]")
    K1.findIsomorphisms(K1).size should equal(2)
    val K2 = DirectedFusionGraph("dfg 0;[[0]];[[1],[0]];[[0,1]];1;[[0,0],[0,0]];[[1,0],[1,0]];[[0,0],[1,1]];1;[[0,0],[0,0]]")
    K2.findIsomorphisms(K2).size should equal(2)
    val K3 = DirectedFusionGraph("dfg 0;[[0]];[[1],[0]];[[0,1]];1;[[0,0],[0,0]];[[1,0],[1,0]];[[0,0],[1,1]];1;[[0,1],[0,0]]")
    K3.findIsomorphisms(K3).size should equal(1)
    val K4 = DirectedFusionGraph("dfg 0;[[0]];[[1],[0]];[[0,1]];1;[[0,0],[0,0]];[[1,0],[1,0]];[[0,0],[1,1]];1;[[1,1],[1,1]]")
    K4.findIsomorphisms(K4).size should equal(2)
    K1.isIsomorphicTo(K2) should equal(false)
    K1.isIsomorphicTo(K3) should equal(false)
    K1.isIsomorphicTo(K4) should equal(false)
    K2.isIsomorphicTo(K3) should equal(false)
    K2.isIsomorphicTo(K4) should equal(false)
    K3.isIsomorphicTo(K4) should equal(false)
  }

}