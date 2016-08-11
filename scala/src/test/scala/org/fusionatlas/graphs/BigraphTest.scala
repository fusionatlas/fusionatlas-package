package org.fusionatlas.graphs

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class BigraphTest extends FlatSpec with ShouldMatchers {

  val HaagerupString = "gbg1v1v1v1p1v1x0p0x1v1x0p0x1"
  val Haagerup = Bigraph(HaagerupString)

  "A Bigraph" should "convert to and from string notation" in {
    Haagerup.toString should equal(HaagerupString)
  }

  "A Bigraph" should "correctly report its rank at each depth" in {
    (0 to Haagerup.graphDepth) map { Haagerup.rankAtDepth(_) } should equal(List(1, 1, 1, 1, 2, 2, 2))
  }

  "A Bigraph" should "report the neighbours of a vertex" in {
    Haagerup.neighbours(0, 1) should equal(List((1, 1)))
    Haagerup.neighbours(1, 1) should equal(List((0, 1), (2, 1)))
    Haagerup.neighbours(3, 1) should equal(List((2, 1), (4, 1), (4, 2)))
    Bigraph("gbg1v1v1v1p1v1x0p0x1v1x0p0x1v1x1").neighbours(7, 1) should equal(List((6, 1), (6, 2)))
  }

  "A Bigraph" should "calculate annular multiplicities correctly" in {
    Haagerup.loops should equal(List(1, 1, 2, 5, 15, 52, 199))

    val BadSeed = Bigraph("gbg1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v0x0x1x0p0x1x0x0v1x0p0x1v0x1p1x0")

    BadSeed.annularMultiplicities should equal(List(1, 0, 0, 0, 1, 0, 3, 4, 12, 27))
  }

  "A Bigraph" should "estimate lower bounds on its FP eigenvalue (1)" in {
    Haagerup.FPEigenvalueLowerBounds.take(5).toList should equal(List(1.951380678630378, 2.0705336967120798, 2.0742135906871737, 2.074310674831708, 2.074313224304702))
    Bigraph("gbg1v1v1v1p1v1x0p0x1v1x0p0x1v2x2").FPEigenvalueLowerBounds(10) should equal(3.0281828657095025)
  }
 
  "A Bigraph" should "report whether its FP eigenvalue might be below some threshold" in {
    Haagerup.isFPEigenvaluePossiblyBelow(5) should equal(true)
    Haagerup.isFPEigenvaluePossiblyBelow(2) should equal(false)
    Bigraph(Haagerup.toString + "v1x4").isFPEigenvaluePossiblyBelow(sqrt(5)) should equal(false)
  }

}

