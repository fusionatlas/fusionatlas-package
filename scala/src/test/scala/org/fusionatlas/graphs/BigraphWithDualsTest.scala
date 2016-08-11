package org.fusionatlas.graphs

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class BigraphWthDualsTest extends FlatSpec with ShouldMatchers {

  val HaagerupString = "bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1"
  val Haagerup = BigraphWithDuals(HaagerupString)

  val TwoA1sString = "bwd1x0p0x1duals1x2"
  val TwoA1s = BigraphWithDuals(TwoA1sString)

  "A BigraphWthDuals" should "convert to and from string notation" in {
    Haagerup.toString should equal(HaagerupString)
    TwoA1s.toString should equal(TwoA1sString)
  }

  "A BigraphWthDuals" should "correctly report its rank at each depth" in {
    (0 to Haagerup.graphDepth) map { Haagerup.rankAtDepth(_) } should equal(List(1, 1, 1, 1, 2, 2, 2))
    (0 to TwoA1s.graphDepth) map { TwoA1s.rankAtDepth(_) } should equal(List(2, 2))
  }
  
  "A BigraphWthDuals" should "properly truncate" in {
    (0 to Haagerup.truncate.graphDepth) map { Haagerup.truncate.rankAtDepth(_) } should equal(List(1, 1, 1, 1, 2, 2))
    (0 to TwoA1s.truncate.graphDepth) map { TwoA1s.truncate.rankAtDepth(_) } should equal(List(2))
  }

}

