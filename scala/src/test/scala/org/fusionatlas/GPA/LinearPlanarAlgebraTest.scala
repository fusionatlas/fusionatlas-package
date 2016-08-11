package org.fusionatlas.GPA

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.math._
import net.tqft.toolkit.algebra.Fraction
import net.tqft.toolkit.algebra.polynomials.Polynomial
import org.fusionatlas.graphs.PairOfBigraphsWithDuals

@RunWith(classOf[JUnitRunner])
class LinearPlanarAlgebraTest extends FlatSpec with ShouldMatchers {

  val HaagerupGPA = {
    val p: Polynomial[Fraction[Int]] = Polynomial(0 -> 13, 1 -> 0, 2 -> 1)
    val dimensions = (List(List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(3, 2), 2 -> Fraction(1, 2))), List(Polynomial(0 -> Fraction(1, 2), 2 -> Fraction(1, 2))), List(Polynomial(0 -> Fraction(3, 2), 2 -> Fraction(1, 2)), Polynomial(0 -> Fraction(3, 2), 2 -> Fraction(1, 2))), List(Polynomial(0 -> Fraction(1, 1)), Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(1, 1)), Polynomial(0 -> Fraction(1, 1)))),
      List(List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(5, 2), 2 -> Fraction(1, 2))), List(Polynomial(0 -> Fraction(3, 2), 2 -> Fraction(1, 2))), List(Polynomial(0 -> Fraction(9, 2), 2 -> Fraction(1, 2))), List(Polynomial(0 -> Fraction(5, 2), 2 -> Fraction(1, 2)), Polynomial(0 -> Fraction(1, 2), 2 -> Fraction(1, 2))), List(Polynomial(0 -> Fraction(5, 2), 2 -> Fraction(1, 2)), Polynomial(0 -> Fraction(5, 2), 2 -> Fraction(1, 2)))))
    val G = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")

    val combinatorics: FusionCombinatorics = (G, p, dimensions)

    new LinearGraphPlanarAlgebra(combinatorics)
  }

  val A2GPA = {
    val p: Polynomial[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1))
    val dimensions = (List(List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(1, 1)))), List(List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(1, 1)))))
    val G = PairOfBigraphsWithDuals("bwd1duals1", "bwd1duals1")
    val combinatorics: FusionCombinatorics = (G, p, dimensions)

    new LinearGraphPlanarAlgebra(combinatorics)
  }
  val A3GPA = {
    val p: Polynomial[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1))
    val dimensions = (List(List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(1, 1)))), List(List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(2, 1))), List(Polynomial(0 -> Fraction(1, 1)))))
    val G = PairOfBigraphsWithDuals("bwd1v1duals1v1", "bwd1duals1v1")
    val combinatorics: FusionCombinatorics = (G, p, dimensions)

    new LinearGraphPlanarAlgebra(combinatorics)
  }

  def positiveDisk(up: Int, down: Int): ChiralDisk = {
    Disk[ChiralStrand, (Int, Int)]((0, 0), strands = for (i <- 0 until (up + down) / 2; l <- Seq(Dextro(Vertex(0, 1, 0, 0), 0), Dextro(Vertex(1, 0, 0, 0), 0))) yield l, up, down)
  }

//  {
//    val disk = positiveDisk(1, 1)
//    val basis = A2GPA.basis(disk)
//
//    "basis" should "produce a 1 dimensional space for A2" in {
//      basis should have size (1)
//    }
//
//    "capAt" should "correctly act on the unique 1-box for A2" in {
//      val cap0 = A2GPA.act(A2GPA.operad.capAt(disk, 0))
//      cap0(Seq(basis.head)).coefficients should equal(Map(Path(Vertex(0, 0, 0, 0), List()) -> Polynomial(0 -> Fraction(1, 1))))
//      val cap1 = A2GPA.act(A2GPA.operad.capAt(disk, 1))
//      cap1(Seq(basis.head)).coefficients should equal(Map(Path(Vertex(0, 1, 0, 0), List()) -> Polynomial(0 -> Fraction(1, 1))))
//    }
//  }

  {
    val disk00 = positiveDisk(0, 0)
    val disk11 = positiveDisk(1, 1)

    "basis" should "produce a 1 dimensional space for A3" in {
      println(A3GPA.basis(disk00).toList)
      A3GPA.basis(disk00) should have size (2)
      println(A3GPA.basis(disk11).toList)
      A3GPA.basis(disk11) should have size (2)
    }

    val basis = A3GPA.basis(disk11)

    "capAt" should "correctly act on the 1-boxes for A3" in {
      val cap0 = A3GPA.act(A3GPA.operad.capAt(disk11, 0))
      cap0(Seq(basis(0))).coefficients should equal(Map(Path(Vertex(0, 0, 0, 0), List()) -> Polynomial(0 -> Fraction(1, 1))))
      cap0(Seq(basis(1))).coefficients should equal(Map(Path(Vertex(0, 0, 1, 1), List()) -> Polynomial(0 -> Fraction(1, 1))))
//      val cap1 = A2GPA.act(A2GPA.operad.capAt(disk11, 1))
//      cap1(Seq(basis(0))).coefficients should equal(Map(Path(Vertex(0, 1, 0, 0), List()) -> Polynomial(0 -> Fraction(1, 1))))
//      cap1(Seq(basis(1))).coefficients should equal(Map(Path(Vertex(0, 1, 0, 0), List()) -> Polynomial(0 -> Fraction(1, 1))))
    }
  }

  //  "basisForLowWeightSpace" should "produce a 14 dimensional space for Haagerup" in {
  //    val disk = positiveDisk(8, 0)
  //
  //    HaagerupGPA.basis(disk).size should equal(375)
  //    val b = HaagerupGPA.basis(disk).head
  //    HaagerupGPA.lowWeightSpace(disk)().size should equal(14)
  //  }

  //  "rotationalEigenspace" should "produce a 4 dimensional space for eigenvalue -1 for Haagerup" in {
  //    val disk = positiveDisk(8, 0)
  //    //    val lowWeightSpace = HaagerupGPA.lowWeightSpace(disk)()
  //    //    HaagerupGPA.rotationalEigenspace(disk, -1)(lowWeightSpace).size should equal (4) 
  //  }
  //
  //  "identityTangle" should "do nothing on n boxes" in {
  //    val disk = positiveDisk(1, 1)
  //    val identityTangle = HaagerupGPA.operad.identity(disk)
  //    val basis: Seq[GPAElement[F]] = HaagerupGPA.basis(disk)
  //    for (x <- basis) {
  //      HaagerupGPA.act(identityTangle)(Seq(x)) should equal(x)
  //    }
  //  }
  //
  //  "traceOfStrand" should "produce the correct dimension for a labelled strand" in {
  //    val disk = positiveDisk(1, 1)
  //    val strand = HaagerupGPA.operad.identity(disk)
  //
  //  }

}

