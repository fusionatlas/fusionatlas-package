package org.fusionatlas.GPA

case class Spaghetti[S <: StrandType[S, _]](val label: S, val initialDisk: Int, val initialPoint: Int, val finalDisk: Int, val finalPoint: Int, val initialTangentDirection: Int, val curvature: Int) {
  import net.tqft.toolkit.arithmetic.Mod._
  def finalTangentDirection = if (curvature % 2 == 0) initialTangentDirection else -initialTangentDirection
  def renumberDisk(oldLabel: Int, newLabel: Int) = copy(initialDisk = if (initialDisk == oldLabel) newLabel else initialDisk, finalDisk = if (finalDisk == oldLabel) newLabel else finalDisk)
  def incrementLabelsBy(k: Int, ifAbove: Option[Int] = None) = {
    val threshold = ifAbove.getOrElse(-1)
    copy(initialDisk = if (initialDisk > threshold) initialDisk + k else initialDisk, finalDisk = if (finalDisk > threshold) finalDisk + k else finalDisk)
  }
  def reverse = {
    Spaghetti(label.dual, finalDisk, finalPoint, initialDisk, initialPoint, -finalTangentDirection, -curvature)
  }
  def composable(other: Spaghetti[S]) = {
    label == other.label && finalDisk == other.initialDisk && finalPoint == other.initialPoint && finalTangentDirection == other.initialTangentDirection
  }
  def compose(other: Spaghetti[S]): Spaghetti[S] = {
    require(composable(other))
    Spaghetti(label, initialDisk, initialPoint, other.finalDisk, other.finalPoint, initialTangentDirection, curvature + other.curvature)
  }
  override def equals(other: Any) = {
    def sameContents(other: Spaghetti[_]) = {
      label == other.label && initialDisk == other.initialDisk && initialPoint == other.initialPoint && finalDisk == other.finalDisk && finalPoint == other.finalPoint && initialTangentDirection == other.initialTangentDirection && curvature == other.curvature
    }

    other match {
      case other: Spaghetti[_] => {
        sameContents(other) || sameContents(other.reverse)
      }
      case _ => false
    }
  }

}

object Loop {
  def apply[S <: StrandType[S, _]](label: S, curvature: Int): Loop[S] = {
    curvature match {
      case 2 => Loop(label)
      case -2 => Loop(label.dual)
      case _ => throw new IllegalArgumentException
    }
  }
}

case class Loop[S](val label: S)

trait PlanarTangle[S <: StrandType[S, L], L] {
  /*
   * disks contains the outer disk as the first entry, then all the inner disks
   */
  def disks: List[Disk[S, L]]
  def spaghetti: Seq[Spaghetti[S]]
  def loops: Seq[Loop[S]]

  def outerDisk = disks.head
  def innerDisks = disks.tail

  def matchedPoint(disk: Int, point: Int) = {
    spaghetti.find(s => s.initialDisk == disk && s.initialPoint == point).map(t => (t.finalDisk, t.finalPoint)).orElse(spaghetti.find(s => s.finalDisk == disk && s.finalPoint == point).map(t => (t.initialDisk, t.initialPoint))).get
  }

  override def equals(other: Any) = {
    other match {
      case other: PlanarTangle[_, _] => {
        import net.tqft.toolkit.collections.Tally._
        disks == other.disks && spaghetti.toSet == other.spaghetti.toSet && loops.tally.toSet == other.loops.tally.toSet
      }
      case _ => false
    }
  }
}

/**
 * A ConnectedPlanarTangle can only represent planar tangles in which all the inner disks are connected to the outer disk (possible via other inner disks).
 * TODO? enforce this
 */
case class ConnectedPlanarTangle[S <: StrandType[S, L],L](
    val disks: List[Disk[S, L]],
    val spaghetti: Seq[Spaghetti[S]],
    val loops: Seq[Loop[S]]
    ) extends PlanarTangle[S, L] {
//  require(disks.map(_.size).sum == spaghetti.size * 2)
//  for (
//    (t, i) <- outerDisk.strands zipWithIndex;
//    dir = if (i < outerDisk.up) -1 else 1
//  ) {
//    require(spaghetti.find(
//      s => s.label == t.dual && s.initialDisk == 0 && s.initialPoint == i && s.initialTangentDirection == dir ||
//        s.label == t && s.finalDisk == 0 && s.finalPoint == i && s.finalTangentDirection == -dir).nonEmpty)
//  }
//  for (
//    (disk,n)  <- innerDisks zipWithIndex;
//    (t, i) <- disk.strands zipWithIndex;
//    dir = if (i < disk.up) 1 else -1
//  ) {
//    require(spaghetti.find(
//      s => s.label == t && s.initialDisk == n + 1 && s.initialPoint == i && s.initialTangentDirection == dir ||
//        s.label == t.dual && s.finalDisk == n + 1 && s.finalPoint == i && s.finalTangentDirection == -dir).nonEmpty)
//  }
  
  override def toString = "ConnectedPlanarTangle(outerDisk = " + outerDisk + ", innerDisks = " + innerDisks + ", spaghetti = " + spaghetti.map(_.toString).sorted + ", loops = " + loops + ")"
}
