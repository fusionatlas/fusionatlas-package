package org.fusionatlas.graphs

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math._

@RunWith(classOf[JUnitRunner])
class PairOfBigraphsWithDualsTest extends FlatSpec with ShouldMatchers {

  "A PairOfBigraphsWithDuals" should "report whether it passes the Ocneanu associativity test (1)" in {
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0duals1v1v1x2v1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2").fullOcneanuTest should equal(false)
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v0x1duals1v1v1x2v1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2").fullOcneanuTest should equal(false)
  }
  "A PairOfBigraphsWithDuals" should "report whether it passes the Ocneanu associativity test (2)" in {
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v0x1p1x0duals1v1v1x2v1x2", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2").fullOcneanuTest should equal(false)
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v0x1p1x0duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2").fullOcneanuTest should equal(true)
  }
  "A PairOfBigraphsWithDuals" should "report whether it passes the Ocneanu associativity test (3)" in {
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2").fullOcneanuTest should equal(false)
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2").fullOcneanuTest should equal(true)
  }

  val A2 = PairOfBigraphsWithDuals("bwd1duals1", "bwd1duals1")
  val A3 = PairOfBigraphsWithDuals("bwd1v1duals1v1", "bwd1v1duals1v1")
  val haagerup = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")
  val haagerupTwisted = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v0x1p1x0duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")

  "A PairOfBigraphsWithDuals" should "report whether it is isomorphic to another (1)" in {
    A2.isIsomorphicTo(A2) should equal(true)
  }
  "A PairOfBigraphsWithDuals" should "report whether it is isomorphic to another (2)" in {
    A3.isIsomorphicTo(A3) should equal(true)
  }
  "A PairOfBigraphsWithDuals" should "report whether it is isomorphic to another (3)" in {
    haagerup.isIsomorphicTo(haagerup) should equal(true)
  }

  "A PairOfBigraphsWithDuals" should "report whether it is isomorphic to another (4-\\epsilon)" in {
   haagerup.truncate(-1).findIsomorphisms(haagerup.truncate(-1)).size should equal(2)
  }

  "A PairOfBigraphsWithDuals" should "report whether it is isomorphic to another (4)" in {
    haagerup.findIsomorphisms(haagerupTwisted).size should equal(2)
  }

  "A PairOfBigraphsWithDuals" should "report whether it is isomorphic to another (5)" in {
    // c.f. PairOfBigraphsWithDualsIsomorphismTestExample.nb
    val g1 = PairOfBigraphsWithDuals("bwd1v1p1p1v1x0x0p0x1x0v1x0duals1v2x1x3v1", "bwd1v1p1p1v1x0x0p0x1x0v1x0duals1v2x1x3v1")
    val g2 = PairOfBigraphsWithDuals("bwd1v1p1p1v0x1x0p0x0x1v1x0duals1v1x3x2v1", "bwd1v1p1p1v0x1x0p0x0x1v1x0duals1v1x3x2v1")
    g1.isIsomorphicTo(g2) should equal(true)
  }
  "isIsomorphic" should "return true when applies to itself" in {
    val g = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0p0x1duals1v1v1x2v1x2", "bwd1v1v1v1p1v1x0p0x1v1x0p0x1p1x0p0x1duals1v1v1x2v2x1x3x4")
    g.truncate(-1).isIsomorphicTo(g.truncate(-1)) should equal(true)
    g.isIsomorphicTo(g) should equal(true)
    val h = PairOfBigraphsWithDuals("bwd1v1duals1v1", "bwd1v1duals1v1")
    h.isIsomorphicTo(h) should equal(true)
    
  }

  val graphsToTest = List(
      PairOfBigraphsWithDuals("bwd1v1p1p1duals1v1x2x3", "bwd1v1p1p1duals1v1x2x3"),
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2"),
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0p0x1duals1v1v1x2v1x2", "bwd1v1v1v1p1v1x0p0x1v1x0p0x1p1x0p0x1duals1v1v1x2v2x1x3x4"),
    PairOfBigraphsWithDuals("bwd1v1p1p1v1x0x0p0x1x0v1x0duals1v2x1x3v1", "bwd1v1p1p1v1x0x0p0x1x0v1x0duals1v2x1x3v1"),
    PairOfBigraphsWithDuals("bwd1v1p1p1v0x1x0p0x0x1v1x0duals1v1x3x2v1", "bwd1v1p1p1v0x1x0p0x0x1v1x0duals1v1x3x2v1"),
    PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v0x0x1x0p0x1x0x0v1x0p0x1v0x1p1x0duals1v1v1x2v1x3x2x4v1x2", "bwd1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2v2x1"))

  "permuteToReduceCrossings" should "produce an isomorphic graph" in {
    for (g <- graphsToTest) {
      g.permuteToReduceCrossings(false).isIsomorphicTo(g) should equal(true)
      g.permuteToReduceCrossings(true).isIsomorphicTo(g) should equal(true)
    }
  }
  // Because we work randomly for large graphs, it's unreasonable to expect idempotence
//  "permuteToReduceCrossings" should "be idempotent" in {
//    for (g <- graphsToTest) {
//      val h = g.permuteToReduceCrossings(true)
//      h.permuteToReduceCrossings(true) == h should equal(true)
//    }
//  }

  "A PairOfBigraphsWith Duals" should "compute left and right neighbours" in {

  }

  "A PairOfBigraphsWith Duals" should "find paths in the 4-partite graph" in {
    haagerup.graphVertices(0)(0)(0).paths(List()).size should equal(1)
    haagerup.graphVertices(0)(3)(0).paths(List(Right)).size should equal(3)
    haagerup.graphVertices(0)(3)(0).paths(List(Right, Left)).size should equal(6)
    haagerup.graphVertices(0)(3)(0).paths(List(Right, Left, Right)).size should equal(13)
    haagerup.graphVertices(0)(3)(0).paths(List(Right, Left, Right, Left)).size should equal(27)    
    (haagerup.graphVertices(0)(3)(0).paths(List(Right, Left, Right, Left)) filter { path => path.edges(1).target == haagerup.graphVertices(1)(3)(0) && path.finish == haagerup.graphVertices(0)(3)(0) }).size should equal(9)
  }

  //  "A PairOfBigraphsWithDuals" should "detect double one-by-one connection entries" in {
  //	  	  val BadSeed = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v0x0x1x0p0x1x0x0v1x0p0x1v0x1p1x0duals1v1v1x2v1x3x2x4v1x2", "bwd1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2v2x1")
  ////	  	  BadSeed.doubleOneByOnes should equal (0)
  //	  	   haagerup.doubleOneByOnes should equal (0)
  //  }
}