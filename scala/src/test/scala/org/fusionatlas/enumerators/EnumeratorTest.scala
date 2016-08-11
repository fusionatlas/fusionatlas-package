package org.fusionatlas.enumerators

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.fusionatlas.graphs._

import scala.math._

@RunWith(classOf[JUnitRunner])
class EnumeratorTest extends FlatSpec with ShouldMatchers {
  
  "Enumerator" should "allow a global dimension limit" in {
    PairEnumerator.extend(4.3589, PairOfBigraphsWithDuals("bwd1duals1", "bwd1duals1"), Some(19.0))
  }
  "Enumerator" should "allow a global dimension limit 2" in {
    PairEnumerator.extend(3.87299, PairOfBigraphsWithDuals("bwd1v1p1duals1v1x2", "bwd1v1p1duals1v1x2"), Some(15.0))
  }
  
//  "Enumerator" should "allow a global dimension limit 3" in {
//    PairEnumerator.extend(3.60555, PairOfBigraphsWithDuals("bwd1v1p1p1duals1v1x2x3", "bwd1v1p1p1duals1v1x2x3"), Some(35.))
//  }
  
  val Haagerup = Bigraph("gbg1v1v1v1p1v1x0p0x1v1x0p0x1")
  val HaagerupTruncatedPair = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1duals1v1v1x2", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")
  val HaagerupPair = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")

  "Enumerator" should "find the correct depth 1 extensions of Haagerup" in {
    PairEnumerator.extendBigraph(2.1, Haagerup, false).size should equal(4)
    PairEnumerator.extendBigraph(2.1, Haagerup, true).size should equal(3)
  }

  "Enumerator" should "find the correct depth 1 extensions of the Haagerup pair up to norm 2.1" in {
    val (vines, weeds) = PairEnumerator.extend(2.1, HaagerupTruncatedPair)
    weeds should equal(List())
    vines.size should equal(1)
    vines(0).isIsomorphicTo(HaagerupPair) should equal(true)
  }

  "Enumerator" should "find the correct depth 1 extensions of the Haagerup pair up to norm 2.2" in {
    // c.f. EnumeratorTest2.2.nb
    import net.tqft.toolkit.permutations.Permutations.findOneMappingWithSameTest

    val (vines, weeds) = PairEnumerator.extend(2.2, HaagerupTruncatedPair)

    weeds.size should equal(3)
    val desiredWeeds = List(
      PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0duals1v1v1x2v1", "bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1duals1v1v1x2v3x2x1"),
      PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0p0x1duals1v1v1x2v1x2", "bwd1v1v1v1p1v1x0p0x1v1x1duals1v1v1x2v1"),
      PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0p0x1duals1v1v1x2v1x2", "bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1duals1v1v1x2v4x2x3x1"))
    //		weeds(0).isIsomorphicTo(desiredWeeds(1).switch) should equal (true)
    //		weeds(1).isIsomorphicTo(desiredWeeds(0).switch) should equal (true)
    //		weeds(2).isIsomorphicTo(desiredWeeds(2).switch) should equal (true)

    vines.size should equal(1)
    vines(0).isIsomorphicTo(HaagerupPair) should equal(true)
  }

  "Enumerator" should "find the correct depth 1 extensions of a graph out to norm 2.23829" in {
    PairEnumerator.extend(2.23829, PairOfBigraphsWithDuals("bwd1v1p1p1v1x0x0p0x1x0p0x0x1v1x1x0duals1v2x1x3v1", "bwd1v1p1p1v1x0x0p0x1x0p0x0x1v1x1x0duals1v2x1x3v1"))._2.size should equal(0)
  }

  "Enumerator" should "find the correct depth 1 extensions of the Haagerup pair up to norm 2.4" in {
    val (vines, weeds) = PairEnumerator.extend(2.4, HaagerupTruncatedPair)

    weeds.size should equal(67)
    vines.size should equal(1)
    vines(0).isIsomorphicTo(HaagerupPair) should equal(true)
  }

      val sqrt6 = 2.44949
    val sqrt3plusSqrt5 = 2.28826
    val badSeedD = 2.19737
    
    val slowPair = PairOfBigraphsWithDuals("bwd1v1v1p1v1x0p1x0p0x1v1x0x0p0x1x0p0x0x1p0x0x1duals1v1v1x3x2", "bwd1v1v1p1v1x0p1x0p0x1v0x0x1p0x1x0p1x0x0p0x0x1duals1v1v1x3x2")
    val slowPair2 = PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0p0x1x0v1x0p0x1v1x0p0x1v1x0p1x0p0x1p0x1duals1v1v2x1v2x1", "bwd1v1v1p1p1v1x0x0p0x1x0v1x0p0x1v1x0p0x1v1x0p1x0p0x1p0x1duals1v1v2x1v2x1")
    val memoryPair = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v0x0x1x0p0x1x0x0v1x0p0x1v0x1p1x0v1x0p0x1v0x1p1x0v1x0p0x1v0x1p1x0v1x1duals1v1v1x2v1x3x2x4v1x2v2x1v1x2v1", "bwd1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1v1x0p1x0p0x1p0x1duals1v1v1x2v1x2v2x1v1x2v2x1v1x2x3x4")
    val memoryPair2 = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0p0x1duals1v1v1x2", "bwd1v1v1v1p1v1x0p0x1p0x1duals1v1v1x2")
    val slowPair3 = PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0p0x1x0p0x0x1v1x0x0p0x1x0v1x0p0x1v1x0p0x1v1x0p0x1p0x1v1x0x0p1x0x0p1x0x0p0x1x0p0x0x1v1x0x0x0x0p1x0x0x0x0p0x0x0x1x0p0x0x0x1x0p0x0x0x0x1p0x0x0x0x1duals1v1v1x2x3v1x2v1x3x2v1x2x5x6x3x4", "bwd1v1v1p1p1v1x0x0p0x1x0p0x0x1v1x0x0p0x1x0v1x0p0x1v1x0p0x1v1x0p0x1p0x1v1x0x0p1x0x0p1x0x0p0x1x0p0x0x1v1x0x0x0x0p1x0x0x0x0p0x0x0x1x0p0x0x0x1x0p0x0x0x0x1p0x0x0x0x1duals1v1v1x2x3v1x2v1x3x2v1x2x5x6x3x4")
    val tooManyWeeds = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1duals1v1v1x2x3", "bwd1v1v1v1p1p1duals1v1v1x2x3")

  
  "PairEnumerator" should "work correctly" in {
    val (vines, weeds) = PairEnumerator.extend(sqrt3plusSqrt5, slowPair)
    vines.size should equal(0)
    weeds.size should equal(5)
  }
  "PairEnumerator" should "work correctly 2" in {
    val (vines, weeds) = PairEnumerator.extend(sqrt6, memoryPair2)
    vines.size should equal(0)
    weeds.size should equal(612)
  }
  "PairEnumerator" should "work correctly 3" in {
    val (vines, weeds) = PairEnumerator.extend(sqrt3plusSqrt5, slowPair2)
    vines.size should equal(1)
    weeds.size should equal(20)
  }
  "PairEnumerator" should "work correctly 4" in {
    val (vines, weeds) = PairEnumerator.extend(badSeedD, memoryPair)
    vines.size should equal(1)
    weeds.size should equal(1)
  }
  "PairEnumerator" should "work correctly 5" in {
    val (vines, weeds) = PairEnumerator.extend(sqrt6, tooManyWeeds)
    vines.size should equal(1)
    weeds.size should equal(66)
  }


  
}