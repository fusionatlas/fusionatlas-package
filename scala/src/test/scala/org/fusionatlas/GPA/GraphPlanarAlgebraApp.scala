package org.fusionatlas.GPA

import net.tqft.toolkit.algebra.polynomials.Polynomial
import net.tqft.toolkit.algebra.Fraction
import org.fusionatlas.graphs.PairOfBigraphsWithDuals

object GraphPlanarAlgebraApp extends App {

  val A1GPA = {
    val p: Polynomial[Fraction[Int]] = Polynomial(1 -> Fraction(1, 1))
    val dimensions = (List(List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(1, 1)))), List(List(Polynomial(0 -> Fraction(1, 1))), List(Polynomial(0 -> Fraction(1, 1)))))
    val G = PairOfBigraphsWithDuals("bwd1duals1", "bwd1duals1")
    val combinatorics: FusionCombinatorics = (G, p, dimensions)

    new LinearGraphPlanarAlgebra(combinatorics)
  }

  def positiveDisk(up: Int, down: Int): ChiralDisk = {
    Disk[ChiralStrand, (Int, Int)]((0, 0), strands = for (i <- 0 until (up + down) / 2; l <- Seq(Dextro(Vertex(0, 1, 0, 0), 0), Dextro(Vertex(1, 0, 0, 0), 0))) yield l, up, down)
  }

  val disk = positiveDisk(1, 1)
  val basis = A1GPA.basis(disk)
  require(basis.size == 1, "basis must have size 1")
  println(basis)
  val cap = A1GPA.act(A1GPA.operad.capAt(disk, 0))
  println(cap(Seq(basis.head)).coefficients)

}