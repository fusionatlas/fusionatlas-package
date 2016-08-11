package org.fusionatlas.graphs.obstructions

import org.fusionatlas.graphs.PairOfBigraphsWithDuals
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import org.fusionatlas.graphs.GraphVertex

@RunWith(classOf[JUnitRunner])
class PairObstructionsTest extends FlatSpec with ShouldMatchers {

  val A2 = PairOfBigraphsWithDuals("bwd1duals1", "bwd1duals1")
  val A3 = PairOfBigraphsWithDuals("bwd1v1duals1v1", "bwd1v1duals1v1")
  val haagerup = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")
  val doubleHaagerup = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1")
  val evenTriple = PairOfBigraphsWithDuals("bwd1v1v1p1v1x0p1x0p0x1p0x1duals1v1v4x3x2x1", "bwd1v1v1p1v1x0p1x0p0x1p0x1duals1v1v4x3x2x1")
  val izumi2221 = PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0p0x1x0duals1v1v2x1", "bwd1v1v1p1p1v1x0x0p0x1x0duals1v1v2x1")
  val asaedaHaagerup = PairOfBigraphsWithDuals("bwd1v1v1v1v1v1p1v1x0p0x1v1x0p0x1p0x1v1x0x0v1duals1v1v1v1x2v2x1x3v1", "bwd1v1v1v1v1v1p1v0x1p0x1v0x1v1duals1v1v1v1x2v1")
  val SU3 = PairOfBigraphsWithDuals("bwd1v1v1p1v1x0p0x1p0x1v0x1x0p1x0x1duals1v1v2x1x3", "bwd1v1v1p1v1x0p0x1p0x1v0x1x0p1x0x1duals1v1v2x1x3")

  val mismatchedD6 = PairOfBigraphsWithDuals("bwd1v1v1v1p1duals1v1v1x2", "bwd1v1v1v1p1duals1v1v2x1")
  val mismatchedBroom = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1duals1v1v1x2x3", "bwd1v1v1v1p1p1duals1v1v2x1x3")

  "A PairOfBigraphsWithDuals" should "compute connection sizes correctly" in {
    haagerup.sizeOfConnection(GraphVertex(haagerup, 0, 4, 1), GraphVertex(haagerup, 0, 6, 1)) should equal(0)
    haagerup.sizeOfConnection(GraphVertex(haagerup, 0, 4, 1), GraphVertex(haagerup, 0, 4, 2)) should equal(0)
    haagerup.sizeOfConnection(GraphVertex(haagerup, 0, 3, 1), GraphVertex(haagerup, 1, 3, 1)) should equal(3)
    haagerup.sizeOfConnection(GraphVertex(haagerup, 0, 4, 1), GraphVertex(haagerup, 1, 4, 2)) should equal(1)
    haagerup.sizeOfConnection(GraphVertex(haagerup, 0, 4, 2), GraphVertex(haagerup, 1, 4, 1)) should equal(2)
  }

  //  "A PairOfBigraphsWithDuals" should "correctly clump vertices by dimension" in {
  //    print(doubleHaagerup.verticesByDimension)
  //	doubleHaagerup.verticesByDimension should equal (false)
  //    haagerup.verticesByDimension should equal (false)
  //  }

  "A PairOfBigraphsWithDuals" should "correctly identify dimension-preserving bijections of neighbours of triple points" in {
    doubleHaagerup.combinatorialSameDimensionsQ(GraphVertex(doubleHaagerup, 0, 4, 1), GraphVertex(doubleHaagerup, 1, 4, 1)) should equal(true)
    haagerup.combinatorialSameDimensionsQ(GraphVertex(haagerup, 0, 4, 1), GraphVertex(haagerup, 1, 4, 1)) should equal(false)

  }

  val obstructions = new PairObstructions {
    override val d: Double = 3
    override val ignoredWeeds = Nil
  }

  "obstructions" should "correctly check the number of self-dual objects past the branch point" in {
    obstructions.paper1duality(mismatchedD6).isEmpty should equal(false)
    obstructions.paper1duality(mismatchedBroom).isEmpty should equal(false)
    obstructions.paper1duality(haagerup).isEmpty should equal(true)
  }

  "obstructions" should "apply the triple-point obstruction correctly in the presence of multiple edges" in {
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1p1v1x1v2duals1v1x2v1", "bwd1v1p1v2x0v1p1duals1v1x2v1x2")) should equal(None)
  }

  "obstructions" should "correctly apply the triple-point obstruction" in {
    //odd triple points
    obstructions.triplePoint(doubleHaagerup) should not be ('empty)
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0duals1v1v1x2", "bwd1v1v1v1p1v1x0duals1v1v1x2")) should not be ('empty)
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1duals1v1v1x2", "bwd1v1v1v1p1v1x0p0x1duals1v1v1x2")) should not be ('empty)
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0duals1v1v1x2", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")) should not be ('empty)
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0p0x1duals1v1v1x2", "bwd1v1v1v1p1v1x0p1x0p0x1duals1v1v1x2")) should not be ('empty)
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0p1x0duals1v1v1x2", "bwd1v1v1v1p1v1x0p1x0p1x0duals1v1v1x2")) should not be ('empty)
    obstructions.triplePoint(haagerup) should equal(None)
    obstructions.triplePoint(asaedaHaagerup) should equal(None)
    //even triple points
    obstructions.triplePoint(SU3) should equal(None)
    obstructions.triplePoint(SU3.truncate(-1)) should equal(None)
    obstructions.triplePoint(SU3.truncate(-2)) should equal(None)
    obstructions.triplePoint(SU3.truncate(-3)) should equal(None)
    obstructions.triplePoint(evenTriple) should not be ('empty)
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1v1p1v1x0p0x1duals1v1v2x1", "bwd1v1v1p1v1x0p0x1duals1v1v2x1")) should not be ('empty)
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1v1p1v1x0p1x0duals1v1v1x2", "bwd1v1v1p1v1x0p1x0duals1v1v1x2")) should not be ('empty)
    obstructions.triplePoint(PairOfBigraphsWithDuals("bwd1v1v1p1v1x0p1x0p1x0duals1v1v1x2x3", "bwd1v1v1p1v1x0p1x0p1x0duals1v1v1x2x3")) should not be ('empty)
    obstructions.triplePoint(izumi2221) should equal(None)
  }

//  "obstructions" should "correctly apply the triple-point obstruction to the large list obstructedTriplePoints" in {
//    TriplePointObstructionExamples.examples filter (obstructions.triplePoint(_) == None) should equal(TriplePointObstructionExamples.combinatorialExceptions)
//  }

  "stability" should "not rule out Dietmar's fish" in {
    val fish = PairOfBigraphsWithDuals("bwd1v1p1v0x1p0x1v0x1v1p1v0x1v1p1v0x1v1p1v0x1v1p1v0x1p0x1v1x1duals1v1x2v1v1v1v1v2x1",
      "bwd1v1p1v1x0p0x1v1x0p0x1p0x1v1x0x0p0x0x1v1x0p1x0p0x1p0x1v0x1x0x0p0x0x0x1v1x0p1x0p0x1p0x1v0x1x0x0p0x0x0x1v1x0p1x0p0x1p0x1v0x1x0x0p0x0x0x1v1x0p1x0p0x1p0x1p0x1v0x1x0x0x1v1duals1v1x2v2x1x3v1x3x2x4v1x3x2x4v1x3x2x4v1x3x2x4x5v1")
    obstructions.stability(fish) should be('empty)
  }
  
  "paper1duality" should "not choke on A1" in {
    obstructions.stability(PairOfBigraphsWithDuals("bwd1duals1", "bwd1duals1")) should be ('empty)
  }

}