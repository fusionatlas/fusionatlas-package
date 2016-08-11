package org.fusionatlas.GPA

trait StrandType[S <: StrandType[S, L], L] { self: S =>
  def dual: S
  def leftLabel: L
  def rightLabel: L
}
trait SelfDualStrandType[S <: SelfDualStrandType[S, L], L] extends StrandType[S, L] { self: S =>
  final override def dual = this
  def label: L
  final override def leftLabel = label
  final override def rightLabel = label
}

sealed trait NoLabel extends SelfDualStrandType[NoLabel, Unit]
object NoLabel extends NoLabel {
  override def toString = "*"
  override def label = ()
}

sealed trait Chiral[S <: StrandType[S, L], L] extends StrandType[Chiral[S, L], (L, L)] {
  def strand: S
}
case class Laevo[S <: StrandType[S, L], L](override val strand: S, val otherLabel: L) extends Chiral[S, L] {
  override def dual = Laevo(strand.dual, otherLabel)
  override def leftLabel = (strand.leftLabel, otherLabel)
  override def rightLabel = (strand.rightLabel, otherLabel)
}
case class Dextro[S <: StrandType[S, L], L](override val strand: S, val otherLabel: L) extends Chiral[S, L] {
  override def dual = Dextro(strand.dual, otherLabel)
  override def leftLabel = (otherLabel, strand.leftLabel)
  override def rightLabel = (otherLabel, strand.rightLabel)
}