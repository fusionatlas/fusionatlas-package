package org.fusionatlas.enumerators

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.fusionatlas.graphs._
 
import scala.math._

@RunWith(classOf[JUnitRunner])
class DirectedFusionEnumeratorTest extends FlatSpec with ShouldMatchers  {
	
  // FIXME fill out these tests, run them, work out the actual correct values, etc!
  
  val SU3AtLevel2 = DirectedFusionGraph("dfg 0;[[0]];[[1],[0]];[[0,1]];1;[[0,0],[1,0]];[[1,0],[0,0],[0,1]];[[0,0,1],[0,1,0]];1;[[0,0,0],[0,0,1],[1,0,0]]")
  
	"DirectedFusionEnumerator" should "find the correct depth 1 extensions of SU(3)_2 to norm 2.1" in {
		val (vines, weeds) = DirectedFusionEnumerator.extend(1.7, SU3AtLevel2)
		for(w <- weeds.take(5)) println(w)
		weeds.size should equal (-1 /* FIXME */)
		println(weeds)
	}

	"DirectedFusionEnumerator" should "find the correct depth 1 extensions of SU(3)_2 to norm 2.5" in {
		val (vines, weeds) = DirectedFusionEnumerator.extend(2.5, SU3AtLevel2)
		weeds.size should equal (-1 /* FIXME */)
		println(weeds)
	}

	"The associativity test" should "not be horribly broken" in {
		val candidates = DirectedFusionEnumerator.extend(2.1, SU3AtLevel2)._2.toList
		candidates.map(_.partialAssociativityTest) should equal (List() /* FIXME */)
	}
	

}