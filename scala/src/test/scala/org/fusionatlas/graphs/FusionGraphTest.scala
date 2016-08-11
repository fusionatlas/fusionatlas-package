package org.fusionatlas.graphs

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
 
import scala.math._

@RunWith(classOf[JUnitRunner])
class FusionGraphTest extends FlatSpec with ShouldMatchers  {
	
	val A2String = "fg1v0v1v1v0"
	val A2 = FusionGraph(A2String)
	val T2String = "fg1v0v1v1v1"
	val T2 = FusionGraph(T2String)
	val EvenE6 = FusionGraph("fg1v0v1v1v2v1v1v0")
	val EvenHaagerupString = "fg1v0v1v1v1v1p1v1x2v1p1x1v1x0p0x1v1x2v0p0x0"
	val EvenHaagerup = FusionGraph(EvenHaagerupString)
	val brokenString = "fg1v0v1p1p1v1x2x3v1x2x1p2x1x1p1x1x0"
	
  "A FusionGraph" should "convert to and from string notation" in {
    A2.toString should equal (A2String)
    T2.toString should equal (T2String)
    EvenHaagerup.toString should equal (EvenHaagerupString)
    FusionGraph("fg1v0v1v1v0vvv").toString should equal ("fg1v0v1v1v0vvv")
    FusionGraph(brokenString).toString should equal(brokenString) // actually, not really broken; it's just replacing a symmetric matrix in the input with its lower triangular piece in the output
  }
  "An IncompleteFusionGraph" should "convert to and from string notation" in {
    IncompleteFusionGraph("fg1v0v1v1v0vvv").toString should equal ("fg1v0v1v1v0vvv")
  }
  "A FusionGraph" should "correctly report its rank at each depth" in {
	  (0 to T2.graphDepth) map { T2.rankAtDepth(_) } should equal (List(1,1)) 
	  (0 to 2) map { FusionGraph("fg1v0v1v1v0vv1v").rankAtDepth(_) } should equal (List(1,1,0))
  }
  "An IncompleteFusionGraph" should "correctly report its rank at each depth" in {
	  (0 to 2) map { IncompleteFusionGraph("fg1v0v1v1v0vvv").rankAtDepth(_) } should equal (List(1,1,0))
  }
  "A FusionGraph" should "report the neighbours of a vertex" in {
	  EvenHaagerup.neighbours(0,1) should equal (List((1,1)))
	  EvenHaagerup.neighbours(1,1) should equal (List((0,1),(1,1),(2,1),(2,2)))
	  EvenHaagerup.neighbours(2,1) should equal (List((1,1),(2,1),(2,2),(3,1)))
	  EvenE6.neighbours(1,1) should equal (List((0,1),(1,1),(1,1),(2,1)))
	  T2.neighbours(1,1) should equal (List((0,1),(1,1)))
  }
  "A FusionGraph" should "estimate lower bounds on its FP eigenvalue (1)" in {
	  EvenHaagerup.FPEigenvalueLowerBounds.take(5).toList should equal (List(2.915475947422651, 3.2988411512494005, 3.3027425131408754, 3.302775359350227, 3.3027756353924866))
  }
  "A FusionGraph" should "check associativity correctly" in {
	  FusionGraph("fg1v0v1v1v0v1p1v2x1v1p0x0").partialAssociativityTest should equal (false)
	  FusionGraph("fg1v0v1v1v0v1p1v2x1v0p0x0").partialAssociativityTest should equal (true)
	  FusionGraph("fg1v0v1v1v0v1p1p1v1x2x3v0p0x0p0x0x0").partialAssociativityTest should equal (true)
	  FusionGraph("fg1v0v1v1v0v1p1p1v2x1x3v0p0x0p0x0x0").partialAssociativityTest should equal (true)
	  FusionGraph("fg1v0v1v1v1v1p1v2x1v0p0x0").partialAssociativityTest should equal (true)
	  EvenHaagerup.partialAssociativityTest should equal (true)
	  EvenE6.partialAssociativityTest should equal (true)
  }
  "A FusionGraph" should "identify isomorphisms correctly" in {
	  EvenHaagerup.findIsomorphisms(EvenHaagerup).size should equal (2)
  }
  "A FusionGraph" should "compute connection sizes correctly" in {
	  EvenHaagerup.sizeOfConnection((1,1),(1,1)) should equal (4)
	  T2.sizeOfConnection((1,1),(1,1)) should equal (2)
	  EvenE6.sizeOfConnection((1,1),(1,1)) should equal (6)
	  FusionGraph("fg1v0v1v1v0v1p1v1x2v0p0x0v1x0p0x1v2x1v0x0p0x0").sizeOfConnection((2,1),(2,2)) should equal (2)
  }  
  
}