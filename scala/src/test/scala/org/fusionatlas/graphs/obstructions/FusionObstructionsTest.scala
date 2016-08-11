package org.fusionatlas.graphs.obstructions

import org.fusionatlas.graphs.FusionGraph
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
 
import scala.math._

@RunWith(classOf[JUnitRunner])
class FusionObstructionsTest extends FlatSpec with ShouldMatchers  {
	
	val obstructions = new FusionObstructions {
		override val d: Double = 3
		override val ignoredWeeds = Nil
	}
	
	val T2 = FusionGraph("fg0v0v1v0v1")
	val EvenA5 = FusionGraph("fg0v0v1v0v1v1v0v0")
	val EvenHaagerup = FusionGraph("fg0v0v1v0v1v1p1v0v1p1x1v1x0p0x1v1v0p0x0")
	
  "FusionObstructions" should "correctly apply the self-loop obstruction" in {
    obstructions.selfLoop(T2) should equal (None)
    obstructions.selfLoop(FusionGraph("fg0v0v1v0v0v1p1v0v1p0x0v1x0v0v0v1v0v0")) should equal (None)
    obstructions.selfLoop(FusionGraph("fg0v0v1v0v0v1p1v0v1p0x0v0x1v0v0v1v0v0")) should equal (Some("Fails the self-loop test at (2,1)"))
    obstructions.selfLoop(FusionGraph("fg0v0v1v0v0v1p1p1v0v0p0x0p0x0x1v0x1x0v0v0")) should equal (Some("Fails the self-loop test at (2,3)"))
    obstructions.selfLoop(FusionGraph("fg0v0v1v0v0v1p1v0v1p0x0v0x1p0x1v1v0p0x0")) should equal (Some("Fails the self-loop test at (2,1)"))
    obstructions.selfLoop(FusionGraph("fg0v0v1v0v0v1p1v0v1p0x0v0x1v0v1")) should equal (Some("Fails the self-loop test at (2,1)"))
	}

  "FusionObstructions" should "correctly apply the triple-point obstruction" in {
    obstructions.triplePoint(T2) should equal (None)
    obstructions.triplePoint(EvenA5) should equal (Some("Fails the triple point obstruction at (1,1)"))
    obstructions.triplePoint(FusionGraph("fg0v0v1v0v0v1p1v0v1p0x0v1x0v0v0v1v0v0")) should equal (Some("Fails the triple point obstruction at (1,1)"))
    obstructions.triplePoint(FusionGraph("fg0v0v1v0v0v1p1v0v0p0x0v1x0p0x1v1v0x0p0x0")) should equal (None)
    obstructions.triplePoint(EvenHaagerup) should equal (None)    
  }

}