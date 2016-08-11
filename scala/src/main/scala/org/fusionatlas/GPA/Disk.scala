package org.fusionatlas.GPA

case class Disk[S <: StrandType[S, L], L](val basepointLabel: L, val strands: Seq[S], up: Int, down: Int) {
  def size = strands.size
  def strandsAcrossTop = strands.take(up)
  def strandsAcrossBottom = strands.takeRight(down).reverse
}

object UnlabeledDisk {
  def apply(up: Int, down: Int) = Disk[NoLabel, Unit]((), Seq.fill(up + down)(NoLabel), up, down)
}