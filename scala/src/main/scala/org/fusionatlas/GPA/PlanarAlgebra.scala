package org.fusionatlas.GPA

import net.tqft.toolkit.algebra.Module
import net.tqft.toolkit.algebra.Field
import net.tqft.toolkit.algebra.Ring
import net.tqft.toolkit.algebra.matrices.Matrix
import net.tqft.toolkit.algebra.Algebra

trait PlanarAlgebra[S <: StrandType[S, L], L, M <: PlanarAlgebraElement[S, L]] extends OperadAlgebra[Disk[S, L], PlanarTangle[S, L], M] {
  def operad: PlanarOperad[S, L]
  override def colour(element: M) = {
    element.disk
  }
}

/* For a GraphPlanarAlgebra, K = GraphCycle */
trait AdditivePlanarAlgebra[S <: StrandType[S, L], L, R, K, M <: PlanarAlgebraModuleElement[S, L, R, K]] extends PlanarAlgebra[S, L, M] {
  def ring: Ring[R]
  def module(disk: Disk[S, L]): Module[R, M]
  def basis(disk: Disk[S, L]): Seq[M]

  class Idempotent(val element: M, checked: Boolean = true) {
    if (checked) {
      // TODO require that it really is an idempotent
    }
    val disk = element.disk
    require(disk.up == disk.down)
    require(disk.strandsAcrossTop == disk.strandsAcrossBottom.map(_.dual))
    def flavour = disk.strandsAcrossTop
    def width = flavour.size
    def dual = new Idempotent(act(operad.rotatePi(disk))(Seq(element)), checked = false)
  }

  def caps(m: M): List[M] = for (
    i <- (0 until m.disk.size).toList
  ) yield act(operad.capAt(m.disk, i))(Seq(m))

  def rotationalEigenspace(disk: Disk[S, L], root: R)(subspace: Seq[M] = basis(disk)): List[M] = ???

  /**
   * @return a basis for the invariant space of the tensor product of the idempotents
   */
  def invariants(idempotents: List[Idempotent])(subspace: Seq[M]): List[M] = ???
  /**
   * @param from is the idempotent at the bottom,
   * @param to is the idempotent at the top
   */
  def Hom(from: Idempotent, to: Idempotent)(subspace: Seq[M] /* TODO default argument */ ): List[M] = {
    for (m <- invariants(List(from, to.dual))(subspace)) yield {
      act(operad.turnDownTopRightCorner(m.disk, from.width))(Seq(m))
    }
  }
  /**
   * @return the lattice of idempotents with boundary 'disk', in a given 'subspace'
   */
  // TODO we should present the lattice structure, rather than just give a list of the idempotents
  def idempotents(disk: Disk[S, L])(subspace: Seq[M] = basis(disk)): List[Idempotent] = {
    require(disk.strandsAcrossTop == disk.strandsAcrossBottom.map(_.dual))

    ???
  }
  def minimalIdempotents(disk: Disk[S, L])(subspace: Seq[M] = basis(disk)): List[Idempotent] = ???

  // TODO many intermediate steps, before we get to this!
  def principalGraph: Any = ???
}

trait LinearPlanarAlgebra[S <: StrandType[S, L], L, F, K, M <: PlanarAlgebraModuleElement[S, L, F, K]] extends AdditivePlanarAlgebra[S, L, F, K, M] {
  final override val ring: Algebra[F, F] = Algebra.fromRing(field) // This is a bit awkward; but at some point I decided that fields weren't algebras...
  implicit def field: Field[F]

  def coefficients(m: M): Seq[F]

  def lowWeightSpace(disk: Disk[S, L])(subspace: Seq[M] = basis(disk)): Seq[M] = {
    val capEquations = Matrix.from((for (e <- subspace) yield for (c <- caps(e); x <- coefficients(c)) yield x)).transpose
    println(capEquations)
    val ns = capEquations.nullSpace
    println(ns)
    val mod = module(disk)
    for(v <- ns) yield {
      mod.sum(for((x, m) <- v.zip(subspace)) yield mod.scalarMultiply(x, m))
    }
  }

}

trait TwoSidedPlanarAlgebra[S <: StrandType[S, L], L, M <: PlanarAlgebraElement[Chiral[S, L], (L, L)]] extends PlanarAlgebra[Chiral[S, L], (L, L), M]

trait AdditiveTwoSidedPlanarAlgebra[S <: StrandType[S, L], L, R, K, M <: PlanarAlgebraModuleElement[Chiral[S, L], (L, L), R, K]] extends AdditivePlanarAlgebra[Chiral[S, L], (L, L), R, K, M] with TwoSidedPlanarAlgebra[S, L, M]

trait LinearTwoSidedPlanarAlgebra[S <: StrandType[S, L], L, R, K, M <: PlanarAlgebraModuleElement[Chiral[S, L], (L, L), R, K]] extends LinearPlanarAlgebra[Chiral[S, L], (L, L), R, K, M] with TwoSidedPlanarAlgebra[S, L, M]


