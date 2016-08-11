package org.fusionatlas.GPA

trait PlanarAlgebraElement[S <: StrandType[S, L], L] {
  def disk: Disk[S, L]
}
trait PlanarAlgebraModuleElement[S <: StrandType[S, L], L, A, K] extends PlanarAlgebraElement[S, L]

case class GPAElement[A](disk: ChiralDisk,  val coefficients: Map[Path, A]) extends PlanarAlgebraModuleElement[ChiralStrand, (Int, Int), A, Path]
