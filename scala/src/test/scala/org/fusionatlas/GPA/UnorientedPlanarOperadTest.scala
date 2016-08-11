package org.fusionatlas.GPA

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class UnorientedPlanarOperadTest extends FlatSpec with ShouldMatchers {

  val operad = new UnorientedPlanarOperad[NoLabel, Unit]
  val * = NoLabel

  // TODO construct many more tests, exercising all the methods in PlanarOperad
  /*
   *  TODO, increase "code coverage":
   *  just because you've tested "turnDownTopRightCorner" should "be inverse to turnUpBottomRightCorner",
   *  doesn't mean you shouldn't also check "turnDownTopLeftCorner" should "be inverse to turnUpBottomLeftCorner"
   *  
   *  Especially for this sort of stuff, where there's going to be a bug in one but not the other...
   */

  "turnUpBottomRightCorner" should "have the right boundaries" in {
    operad.turnUpBottomRightCorner(UnlabeledDisk(2, 2)).outerDisk should equal(UnlabeledDisk(up = 3, down = 1))
    operad.turnUpBottomRightCorner(UnlabeledDisk(2, 2)).innerDisks.head should equal(UnlabeledDisk(up = 2, down = 2))
  }

  "turnUpBottomLeftCorner" should "have the right boundaries" in {
    operad.turnUpBottomLeftCorner(UnlabeledDisk(2, 2)).outerDisk should equal(UnlabeledDisk(up = 3, down = 1))
    operad.turnUpBottomLeftCorner(UnlabeledDisk(2, 2)).innerDisks.head should equal(UnlabeledDisk(up = 2, down = 2))
  }

  "turnDownTopRightCorner" should "have the right boundaries" in {
    operad.turnDownTopRightCorner(UnlabeledDisk(2, 2)).outerDisk should equal(UnlabeledDisk(up = 1, down = 3))
    operad.turnDownTopRightCorner(UnlabeledDisk(2, 2)).innerDisks.head should equal(UnlabeledDisk(up = 2, down = 2))
  }

  "turnDownTopLeftCorner" should "have the right boundaries" in {
    operad.turnDownTopLeftCorner(UnlabeledDisk(2, 2)).outerDisk should equal(UnlabeledDisk(up = 1, down = 3))
    operad.turnDownTopLeftCorner(UnlabeledDisk(2, 2)).innerDisks.head should equal(UnlabeledDisk(up = 2, down = 2))
  }

  "compose" should "combine two identities into one identity (with zero boundary points)" in {
    operad.compose(operad.identity(UnlabeledDisk(0, 0)), operad.identity(UnlabeledDisk(0, 0))) should equal(operad.identity(UnlabeledDisk(0, 0)))
  }

  "compose" should "combine two identities into one identity (with multiple boundary points)" in {
    operad.compose(operad.identity(UnlabeledDisk(1, 1)), operad.identity(UnlabeledDisk(1, 1))) should equal(operad.identity(UnlabeledDisk(1, 1)))
  }

  "capAt" should "commute with a disjoint capAt" in {
    val LR = operad.compose(operad.capAt(UnlabeledDisk(up = 2, down = 4), 0), operad.capAt(UnlabeledDisk(4, 4), 2))
    val RL = operad.compose(operad.capAt(UnlabeledDisk(up = 2, down = 4), 0), operad.capAt(UnlabeledDisk(4, 4), 0))

    LR should equal(RL)
  }

  "turnDownTopRightCorner" should "be inverse to turnUpBottomRightCorner" in {
    operad.compose(operad.turnDownTopRightCorner(UnlabeledDisk(3, 1)), operad.turnUpBottomRightCorner(UnlabeledDisk(2, 2))) should equal(operad.identity(UnlabeledDisk(2, 2)))
  }

  "turnDownTopLeftCorner" should "be inverse to turnUpBottomLeftCorner" in {
    operad.compose(operad.turnDownTopLeftCorner(UnlabeledDisk(3, 1)), operad.turnUpBottomLeftCorner(UnlabeledDisk(2, 2))) should equal(operad.identity(UnlabeledDisk(2, 2)))
  }

  "turnUpBottomRightCorner" should "be inverse to turnDownTopRightCorner" in {
    operad.compose(operad.turnUpBottomRightCorner(UnlabeledDisk(1, 3)), operad.turnDownTopRightCorner(UnlabeledDisk(2, 2))) should equal(operad.identity(UnlabeledDisk(2, 2)))
  }

  "turnUpBottomLeftCorner" should "be inverse to turnDownTopLeftCorner" in {
    operad.compose(operad.turnUpBottomLeftCorner(UnlabeledDisk(1, 3)), operad.turnDownTopLeftCorner(UnlabeledDisk(2, 2))) should equal(operad.identity(UnlabeledDisk(2, 2)))
  }

  "addStrandOnLeft" should "commute with addStrandOnRight" in {
    val LR = operad.compose(operad.addStrandOnLeft(UnlabeledDisk(2, 2), *), operad.addStrandOnRight(UnlabeledDisk(1, 1), *))
    val RL = operad.compose(operad.addStrandOnRight(UnlabeledDisk(2, 2), *), operad.addStrandOnLeft(UnlabeledDisk(1, 1), *))
    LR should equal(RL)
  }

  "addStrandOnLeft, then capAt on the top left" should "turnDownTopLeftCorner" in {
    val LHS = operad.compose(operad.capAt(UnlabeledDisk(3, 3), 0), operad.addStrandOnLeft(UnlabeledDisk(2, 2), *))
    val RHS = operad.turnDownTopLeftCorner(UnlabeledDisk(2, 2))

    LHS should equal(RHS)
  }

  "addStrandOnRight, then capAt on the top right" should "turnDownTopRightCorner" in {
    val LHS = operad.compose(operad.capAt(UnlabeledDisk(3, 3), 1), operad.addStrandOnRight(UnlabeledDisk(2, 2), *))
    val RHS = operad.turnDownTopRightCorner(UnlabeledDisk(2, 2))

    LHS should equal(RHS)
  }

  "addStrandOnLeft, then capAt on the bottom left" should "turnUpBottomLeftCorner" in {
    val LHS = operad.compose(operad.capAt(UnlabeledDisk(3, 3), 4), operad.addStrandOnLeft(UnlabeledDisk(2, 2), *))
    val RHS = operad.turnUpBottomLeftCorner(UnlabeledDisk(2, 2))

    LHS should equal(RHS)
  }

  "addStrandOnRight, then capAt on the bottom right" should "turnUpBottomRightCorner" in {
    val LHS = operad.compose(operad.capAt(UnlabeledDisk(3, 3), 3), operad.addStrandOnRight(UnlabeledDisk(2, 2), *))
    val RHS = operad.turnUpBottomRightCorner(UnlabeledDisk(2, 2))

    LHS should equal(RHS)
  }

  "rotatePi" should "return the identity if there are no boundary strings" in {
    operad.rotatePi(UnlabeledDisk(0, 0), 1) should equal(operad.identity(UnlabeledDisk(0, 0)))
  }

  "rotatePi" should "return the identity if asked to rotate by zero" in {
    operad.rotatePi(UnlabeledDisk(2, 2), 0) should equal(operad.identity(UnlabeledDisk(2, 2)))
  }

  "rotatePi" should "return the identity if asked to rotate by pi, then by minus pi" in {
    operad.compose(operad.rotatePi(UnlabeledDisk(2, 2), -1), operad.rotatePi(UnlabeledDisk(2, 2), 1)) should equal(operad.identity(UnlabeledDisk(2, 2)))
  }

  "rotatePi" should "not return the identity if asked to rotate by 2pi" in {
    operad.rotatePi(UnlabeledDisk(2, 2), 2) should not equal (operad.identity(UnlabeledDisk(2, 2)))
  }

  "rotatePi" should "not create rotations by multiples of pi" in {
    operad.rotatePi(UnlabeledDisk(2, 2), 1) should not equal (operad.rotatePi(UnlabeledDisk(2, 2), -1))
  }

  "multiply" should "have the right boundary" in {
    operad.multiply(UnlabeledDisk(2, 1), UnlabeledDisk(3, 2)).outerDisk should equal(UnlabeledDisk(3,1))
  }

  // I think you mean that "multiply should take two identities and produce a multiply"
  // 'identity' is the annular identity, not the identity with zero inputs, consisting of vertical strands.
  // I admit this is slightly confusing nomenclature --- but from the operad point of view this is definitely what identity means.
  "multiply" should "take two identities and produce a single identity" in {
    val LHS = operad.compose(operad.multiply(UnlabeledDisk(2, 2), UnlabeledDisk(2, 2)), List(operad.identity(UnlabeledDisk(2, 2)), operad.identity(UnlabeledDisk(2, 2))))
    val RHS = operad.identity(UnlabeledDisk(2, 2))
    LHS should equal(RHS)
  }
}

