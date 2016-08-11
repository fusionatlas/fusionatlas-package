package org.fusionatlas.enumerators

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.fusionatlas.graphs._
 
import scala.math._

@RunWith(classOf[JUnitRunner])
class FusionEnumeratorTest extends FlatSpec with ShouldMatchers  {
	
	val A2 = FusionGraph("fg0v0v1v0v0")
	val T2 = FusionGraph("fg0v0v1v0v1")
	val EvenE6 = FusionGraph("fg0v0v1v0v2v1v0v0")
	val EvenHaagerupString = "fg0v0v1v0v1v1p1v0v1p1x1v1x0p0x1v1v0p0x0"
	val EvenHaagerup = FusionGraph("fg0v0v1v0v1v1p1v0v1p1x1v1x0p0x1v1v0p0x0")
	
	"FusionEnumerator" should "find the correct depth 1 extensions of A2 to norm 2.1" in {
		val (vines, weeds) = FusionEnumerator.extend(2.1, A2)
		weeds.size should equal (7)
		println(weeds)
	}

	"FusionEnumerator" should "find the correct depth 1 extensions of A2 to norm 2.5" in {
		val (vines, weeds) = FusionEnumerator.extend(2.5, A2)
		weeds.size should equal (33)
		println(weeds)
	}
	
	
	"FusionEnumerator" should "find the correct depth 1 extensions of T2" in {
		val (vines, weeds) = FusionEnumerator.extend(2.5, T2)
		weeds.size should equal (5)
		println(weeds)
	}

	"The associativity test" should "not be horribly broken" in {
		val candidates = FusionEnumerator.extend(2.1, A2)._2.toList
		candidates.map(_.partialAssociativityTest) should equal (List(true, true, true, true, true, true, true))
	}
	

}