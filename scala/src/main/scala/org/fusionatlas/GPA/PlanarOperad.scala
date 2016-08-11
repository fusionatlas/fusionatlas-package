package org.fusionatlas.GPA

trait TypedSurfaceGraph[V, E, F] {
  trait BoundaryComponent
  case class Loop(e: E) extends BoundaryComponent
  case class EmptyBoundary(v: V) extends BoundaryComponent
  case class Circle(boundary: Seq[(V, E)]) extends BoundaryComponent

  def vertices: Seq[V]
  def edges: Seq[E]
  def faces: Seq[F]

  def facesAdjacentTo(v: V): Seq[F]
  def edgesAdjacentTo(v: V): Seq[E]

  def boundaryComponents(f: F): Seq[BoundaryComponent]

  def faceOnLeft(e: E): F
  def faceOnRight(e: E): F
  def source(e: E): V
  def target(e: E): V
}

trait TypedSurfaceGraphOperad[V, E, F] extends Operad[V, TypedSurfaceGraph[V, E, F]] {
  private final type G = TypedSurfaceGraph[V, E, F]

  override def target(t: G) = t.vertices.head
  override def source(t: G) = t.vertices.tail

  def renamer(g: TypedSurfaceGraph[Either[V, V], Seq[Either[E, E]], Either[F, F]]): G

  override def composeAt(k: Int)(outer: G, inner: G): G = {
    val result: TypedSurfaceGraph[Either[V, V], Seq[Either[E, E]], Either[F, F]] = {

      val edgeChains: Seq[Seq[Either[E, E]]] = ???

      new TypedSurfaceGraph[Either[V, V], Seq[Either[E, E]], Either[F, F]] {
        override val vertices: Seq[Either[V, V]] = {
          import net.tqft.toolkit.collections.Deleted._
          outer.vertices.deleted(k).map(Left(_)) ++ inner.vertices.tail.map(Right(_))
        }
        override val edges: Seq[Seq[Either[E, E]]] = {
          // build edge chains
          ???
        }

        override val faces = ???
        
        def boundaryComponents(f: Either[F, F]): Seq[BoundaryComponent] = ???
        def edgesAdjacentTo(v: Either[V, V]): Seq[Seq[Either[E, E]]] = ???
        def faceOnLeft(e: Seq[Either[E, E]]): Either[F, F] = ???
        def faceOnRight(e: Seq[Either[E, E]]): Either[F, F] = ???
        def facesAdjacentTo(v: Either[V, V]): Seq[Either[F, F]] = ???
        def source(ec: Seq[Either[E, E]]): Either[V, V] = {
          ec.head match {
            case Left(e) => Left(outer.source(e)) 
            case Right(e) => Right(inner.source(e)) 
          }
        }
        def target(ec: Seq[Either[E, E]]): Either[V, V] = {
          ec.last match {
            case Left(e) => Left(outer.target(e)) 
            case Right(e) => Right(inner.target(e)) 
          }
        }
      }

    }

    renamer(result)
  }
}

trait SurfaceGraph extends TypedSurfaceGraph[Int, Int, Int]

trait PlanarTangle2[S, L] {
  def graph: SurfaceGraph
  def edgeLabel(e: Int): S
  def faceLabel(e: Int): L
}

trait PlanarOperad[S <: StrandType[S, L], L] extends Operad[Disk[S, L], PlanarTangle[S, L]] {
  def oriented: Boolean

  def target(t: PlanarTangle[S, L]) = t.outerDisk
  def source(t: PlanarTangle[S, L]) = t.innerDisks
  def composeAt(k: Int)(outer: PlanarTangle[S, L], inner: PlanarTangle[S, L]) = {
    require(outer.innerDisks(k) == inner.outerDisk)
    val outerDisk = outer.outerDisk
    val innerDisks = outer.innerDisks.take(k) ++ inner.innerDisks ++ outer.innerDisks.drop(k + 1)
    require(innerDisks.size == outer.innerDisks.size + inner.innerDisks.size - 1)

    val renumberedOuterSpaghetti = outer.spaghetti.map(_.incrementLabelsBy(inner.innerDisks.size - 1, ifAbove = Some(k + 1)))
    val renumberedInnerSpaghetti = inner.spaghetti.map(_.incrementLabelsBy(k))
    // involvedSpaghetti includes all spaghetti which crosses the composition circle, with the inner disc k+1 and the outer disc 0 renamed to -1
    // remoteSpaghetti includes all the spaghetti which does not cross the composition circle
    val (involvedOuterSpaghetti, remoteOuterSpaghetti) = renumberedOuterSpaghetti.partition(s => s.initialDisk == k + 1 || s.finalDisk == k + 1)
    val (involvedInnerSpaghetti, remoteInnerSpaghetti) = renumberedInnerSpaghetti.partition(s => s.initialDisk == k || s.finalDisk == k)
    val involvedSpaghetti = involvedOuterSpaghetti.map(_.renumberDisk(k + 1, -1)) ++ involvedInnerSpaghetti.map(_.renumberDisk(k, -1))
    val remoteSpaghetti = remoteOuterSpaghetti ++ remoteInnerSpaghetti

    val max = outer.innerDisks.size + inner.innerDisks.size - 1
    for (s <- remoteOuterSpaghetti) {
      require(s.initialDisk <= max)
      require(s.finalDisk <= max)
    }
    for (s <- remoteInnerSpaghetti) {
      require(s.initialDisk <= max)
      require(s.finalDisk <= max)
    }

    def reduceSpaghetti(involvedSpaghetti: List[Spaghetti[S]], resolvedSpaghetti: List[Spaghetti[S]], loops: List[Loop[S]]): (List[Spaghetti[S]], List[Spaghetti[S]], List[Loop[S]]) = {
      import net.tqft.toolkit.collections.DeleteOne._
      import net.tqft.toolkit.arithmetic.Mod._

      involvedSpaghetti match {
        case Nil => (Nil, resolvedSpaghetti, loops)
        case head :: tail => {
          head match {
            // spaghetti that's been resolved by now
            case Spaghetti(_, initialDisk, _, finalDisk, _, _, _) if initialDisk != -1 && finalDisk != -1 => reduceSpaghetti(tail, head :: resolvedSpaghetti, loops)
            // loops
            case Spaghetti(label, -1, initialPoint, -1, finalPoint, _, curvature) if initialPoint == finalPoint => reduceSpaghetti(tail, resolvedSpaghetti, Loop[S](label, curvature) :: loops)
            // spaghetti ending on the composition circle
            case Spaghetti(_, _, _, -1, _, _, _) => {
              val next = tail.find(s => head.composable(s) || !oriented && head.composable(s.reverse)).get
              reduceSpaghetti(head.compose(if (head.composable(next)) next else next.reverse) :: (tail deleteOne next).toList, resolvedSpaghetti, loops)
            }
            // spaghetti beginning on the composition circle
            case Spaghetti(_, -1, _, _, _, _, _) => {
              val previous = tail.find(s => s.composable(head) || !oriented && s.reverse.composable(head)).get
              reduceSpaghetti((if (previous.composable(head)) previous else previous.reverse).compose(head) :: (tail deleteOne previous).toList, resolvedSpaghetti, loops)
            }
          }
        }
      }
    }

    val (Nil, resolvedSpaghetti, newLoops) = reduceSpaghetti(involvedSpaghetti.toList, Nil, Nil)

    new ConnectedPlanarTangle[S, L](outerDisk +: innerDisks, remoteSpaghetti ++ resolvedSpaghetti, newLoops ++ outer.loops ++ inner.loops)
  }

  def empty(label: L) = ConnectedPlanarTangle[S, L](List(Disk[S, L](label, Nil, 0, 0)), Nil, Nil)

  def capAt(boundary: Disk[S, L], k: Int) = {
    require(boundary.size >= 2)
    import net.tqft.toolkit.arithmetic.Mod._
    val k0 = k mod boundary.size
    val k1 = k0 + 1 mod boundary.size
    require(boundary.strands(k0) == boundary.strands(k1).dual)
    val indices = if (k0 == boundary.size - 1) {
      (1 to boundary.size - 2).toList
    } else {
      (0 until k0).toList ::: (k0 + 2 until boundary.size).toList
    }
    require(indices.size == boundary.size - 2)
    val (outerUp, outerDown, curvature, outerBasepointLabel) =
      (k0, k1) match {
        case (k0, 0) if k0 == boundary.up - 1 => {
          // loops all the way round the bottom
          require(boundary.down == 0)
          (boundary.up - 2, 0, 3, boundary.strands.head.rightLabel)
        }
        case (k0, 0) if k0 == boundary.size - 1 && boundary.up == 0 => {
          // loops all the way round the top
          (0, boundary.down - 2, 3, boundary.strands.head.rightLabel)
        }
        case (_, 0) => {
          // on the left side
          (boundary.up - 1, boundary.down - 1, 2, boundary.strands(1).leftLabel)
        }
        case (k0, k1) if k1 < boundary.up => {
          // on the top
          (boundary.up - 2, boundary.down, 1, boundary.basepointLabel)
        }
        case (k0, k1) if k1 == boundary.up => {
          // on the right side
          (boundary.up - 1, boundary.down - 1, 2, boundary.basepointLabel)
        }
        case (k0, k1) => {
          // on the bottom
          (boundary.up, boundary.down - 2, 1, boundary.basepointLabel)
        }
      }
    val outerDisk = Disk[S, L](outerBasepointLabel, indices.map(boundary.strands(_)), outerUp, outerDown)
    val innerDisk = boundary
    val cap = Spaghetti(boundary.strands(k0), 1, k0, 1, k1, if (k0 < boundary.up) 1 else -1, curvature)
    val radial = (indices.zipWithIndex map {
      case (initialPoint, finalPoint) => Spaghetti(innerDisk.strands(initialPoint), 1, initialPoint, 0, finalPoint, if (initialPoint < boundary.up) 1 else -1, 0)
    })
    ConnectedPlanarTangle[S, L](disks = outerDisk :: innerDisk :: Nil, spaghetti = cap :: radial, loops = Nil)
  }

  def turnUpBottomRightCorner(boundary: Disk[S, L], k: Int = 1): PlanarTangle[S, L] = {
    require(k >= 0)
    require(boundary.down >= k)
    k match {
      case 0 => identity(boundary)
      case 1 => {
        // the real work
        val spaghetti = for (
          (s, i) <- boundary.strands.zipWithIndex;
          initialTangentDirection = if (i < boundary.up) 1 else -1;
          curvature = if (i == boundary.up) -1 else 0
        ) yield {
          Spaghetti(s, 1, i, 0, i, initialTangentDirection, curvature)
        }
        ConnectedPlanarTangle(disks = boundary.copy[S, L](up = boundary.up + 1, down = boundary.down - 1) :: boundary :: Nil, spaghetti, loops = Nil)
      }
      case _ => {
        val inner = turnUpBottomRightCorner(boundary, 1)
        compose(turnUpBottomRightCorner(inner.outerDisk, k - 1), List(inner))
      }
    }
  }
  def turnUpBottomLeftCorner(boundary: Disk[S, L], k: Int = 1): PlanarTangle[S, L] = {
    import net.tqft.toolkit.collections.Rotate._
    import net.tqft.toolkit.arithmetic.Mod._

    require(k >= 0)
    require(boundary.down >= k)
    k match {
      case 0 => identity(boundary)
      case 1 => {
        // the real work
        val spaghetti = for (
          (s, i) <- boundary.strands.zipWithIndex;
          j = (i + 1) mod boundary.size;
          initialTangentDirection = if (i < boundary.up) 1 else -1;
          curvature = if (i == boundary.size - 1) 1 else 0
        ) yield {
          Spaghetti(s, 1, i, 0, j, initialTangentDirection, curvature)
        }
        ConnectedPlanarTangle(disks = Disk[S, L](???, strands = boundary.strands.rotateRight(1), up = boundary.up + 1, down = boundary.down - 1) :: boundary :: Nil, spaghetti, loops = Nil)
      }
      case _ => {
        val inner = turnUpBottomLeftCorner(boundary, 1)
        compose(turnUpBottomLeftCorner(inner.outerDisk, k - 1), List(inner))
      }
    }
  }
  def turnDownTopRightCorner(boundary: Disk[S, L], k: Int = 1): PlanarTangle[S, L] = {
    require(k >= 0)
    require(boundary.up >= k)
    k match {
      case 0 => identity(boundary)
      case 1 => {
        // the real work
        val spaghetti = for (
          (s, i) <- boundary.strands.zipWithIndex;
          initialTangentDirection = if (i < boundary.up) 1 else -1;
          curvature = if (i == boundary.up - 1) 1 else 0
        ) yield {
          Spaghetti(s, 1, i, 0, i, initialTangentDirection, curvature)
        }
        ConnectedPlanarTangle(disks = boundary.copy[S, L](up = boundary.up - 1, down = boundary.down + 1) :: boundary :: Nil, spaghetti, loops = Nil)
      }
      case _ => {
        val inner = turnDownTopRightCorner(boundary, 1)
        compose(turnDownTopRightCorner(inner.outerDisk, k - 1), List(inner))
      }
    }
  }
  def turnDownTopLeftCorner(boundary: Disk[S, L], k: Int = 1): PlanarTangle[S, L] = {
    import net.tqft.toolkit.collections.Rotate._
    import net.tqft.toolkit.arithmetic.Mod._

    require(k >= 0)
    require(boundary.up >= k)
    k match {
      case 0 => identity(boundary)
      case 1 => {
        // the real work
        val spaghetti = for (
          (s, i) <- boundary.strands.zipWithIndex;
          j = (i - 1) mod boundary.size;
          initialTangentDirection = if (i < boundary.up) 1 else -1;
          curvature = if (i == 0) -1 else 0
        ) yield {
          Spaghetti(s, 1, i, 0, j, initialTangentDirection, curvature)
        }
        ConnectedPlanarTangle(disks = Disk[S, L](???, strands = boundary.strands.rotateLeft(1), up = boundary.up - 1, down = boundary.down + 1) :: boundary :: Nil, spaghetti, loops = Nil)
      }
      case _ => {
        val inner = turnDownTopLeftCorner(boundary, 1)
        compose(turnDownTopLeftCorner(inner.outerDisk, k - 1), List(inner))
      }
    }
  }

  def rotateLeft(boundary: Disk[S, L], k: Int = 1) = rotateRight(boundary, -k)
  def rotateRight(boundary: Disk[S, L], k: Int = 1): PlanarTangle[S, L] = {
    if (boundary.size == 0) {
      identity(boundary)
    } else {
      k match {
        case 0 => identity(boundary)
        case 1 => {
          if (boundary.up > 0) {
            val first = turnDownTopRightCorner(boundary)
            val second = turnUpBottomLeftCorner(first.outerDisk)
            compose(second, List(first))
          } else {
            val first = turnUpBottomLeftCorner(boundary)
            val second = turnDownTopRightCorner(first.outerDisk)
            compose(second, List(first))
          }
        }
        case -1 => {
          if (boundary.up > 0) {
            val first = turnDownTopLeftCorner(boundary)
            val second = turnUpBottomRightCorner(first.outerDisk)
            compose(second, List(first))
          } else {
            val first = turnUpBottomRightCorner(boundary)
            val second = turnDownTopLeftCorner(first.outerDisk)
            compose(second, List(first))
          }
        }
        case _ => {
          val first = rotateRight(boundary, math.signum(k))
          val second = rotateRight(first.outerDisk, k - math.signum(k))
          compose(second, List(first))
        }
      }
    }
  }

  def rotatePi(boundary: Disk[S, L], k: Int = 1): PlanarTangle[S, L] = {
    if (boundary.size == 0) {
      identity(boundary)
    } else {
      k match {
        case 0 => identity(boundary)
        case 1 => {
          val first = turnDownTopRightCorner(boundary, boundary.up)
          val second = turnUpBottomLeftCorner(first.outerDisk, boundary.down)
          compose(second, List(first))
        }
        case -1 => {
          val first = turnDownTopLeftCorner(boundary, boundary.up)
          val second = turnUpBottomRightCorner(first.outerDisk, boundary.down)
          compose(second, List(first))
        }
        case _ => {
          val first = rotatePi(boundary, math.signum(k))
          val second = rotatePi(first.outerDisk, k - math.signum(k))
          compose(second, List(first))
        }

      }
    }
  }

  def multiply(lower: Disk[S, L], upper: Disk[S, L]): PlanarTangle[S, L] = {
    require(lower.strandsAcrossTop == upper.strandsAcrossBottom.map(_.dual))
    val outerDisk = Disk[S, L](lower.basepointLabel, upper.strandsAcrossTop ++ lower.strandsAcrossBottom.reverse, upper.up, lower.down)
    val upperSpaghetti = for (i <- (0 until upper.up).toList) yield {
      Spaghetti(upper.strands(upper.up - 1 - i), 2, upper.up - 1 - i, 0, upper.up - 1 - i, 1, 0)
    }
    val middleSpaghetti = for (i <- (0 until upper.down).toList) yield {
      Spaghetti(upper.strands(i + upper.up).dual, 1, lower.up - i - 1, 2, upper.up + i, 1, 0)
    }
    val lowerSpaghetti = for (i <- (0 until lower.down).toList) yield {
      Spaghetti(lower.strands(i + lower.up).dual, 0, upper.up + i, 1, lower.up + i, 1, 0)
    }
    ConnectedPlanarTangle(outerDisk :: lower :: upper :: Nil, upperSpaghetti ::: middleSpaghetti ::: lowerSpaghetti, loops = Nil)
  }
  def multiply(basepointLabel: L, disks: Disk[S, L]*): PlanarTangle[S, L] = {
    disks.size match {
      case 0 => empty(basepointLabel)
      case 1 => identity(disks.head)
      case 2 => multiply(disks(0), disks(1))
      case _ => {
        val inner = multiply(basepointLabel, disks.tail: _*)
        compose(multiply(basepointLabel, disks.head, inner.outerDisk), identity(disks.head), inner)
      }
    }
  }

  def addStrandOnLeft(boundary: Disk[S, L], s: S): PlanarTangle[S, L] = {
    val outerDisk = Disk[S, L](s.leftLabel, s +: boundary.strands :+ s.dual, boundary.up + 1, boundary.down + 1)
    val spaghetti = Spaghetti(s, 0, boundary.size + 1, 0, 0, 1, 0) +: (for (
      (s, i) <- boundary.strands.zipWithIndex;
      initialTangentDirection = if (i < boundary.up) 1 else -1
    ) yield {
      Spaghetti(s, 1, i, 0, i + 1, initialTangentDirection, 0)
    })
    ConnectedPlanarTangle(outerDisk :: boundary :: Nil, spaghetti, loops = Nil)
  }
  def addStrandsOnLeft(boundary: Disk[S, L], labels: Seq[S]): PlanarTangle[S, L] = {
    labels match {
      case Nil => identity(boundary)
      case s :: Nil => addStrandOnLeft(boundary, s)
      case h :: t => {
        val inner = addStrandsOnLeft(boundary, t)
        compose(addStrandOnLeft(inner.outerDisk, h), List(inner))
      }
    }
  }
  def addStrandOnRight(boundary: Disk[S, L], s: S) = {
    val first = rotatePi(boundary, 1)
    val second = addStrandOnLeft(first.outerDisk, s.dual)
    val third = rotatePi(second.outerDisk, -1)
    compose(third, List(compose(second, List(first))))
  }
  def addStrandsOnRight(boundary: Disk[S, L], labels: Seq[S]): PlanarTangle[S, L] = {
    labels match {
      case Nil => identity(boundary)
      case s :: Nil => addStrandOnRight(boundary, s)
      case h :: t => {
        val inner = addStrandOnRight(boundary, h)
        compose(addStrandsOnRight(inner.outerDisk, t), List(inner))
      }
    }
  }
  def tensor(factor1: Disk[S, L], factor2: Disk[S, L]) = {
    val strandsOnRight = addStrandsOnRight(factor1, factor2.strandsAcrossBottom.map(_.dual))
    val strandsOnLeft = addStrandsOnLeft(factor2, factor1.strandsAcrossTop)
    compose(multiply(strandsOnRight.outerDisk, strandsOnLeft.outerDisk), List(strandsOnRight, strandsOnLeft))
  }

  // Positive k shifts the lower factor to the right 
  // Negative k shifts the upper factor to the right
  def multiplyWithOffset(k: Int, lower: Disk[S, L], upper: Disk[S, L]) = {
    val offsetLower: PlanarTangle[S, L] = if (k > 0) {
      addStrandsOnLeft(lower, upper.strandsAcrossBottom.map(_.dual).take(k))
    } else {
      identity(lower)
    }
    val offsetUpper: PlanarTangle[S, L] = if (k < 0) {
      addStrandsOnLeft(upper, lower.strandsAcrossTop.take(-k))
    } else {
      identity(upper)
    }
    val padLower: PlanarTangle[S, L] = if (offsetLower.outerDisk.up < offsetUpper.outerDisk.down) {
      addStrandsOnRight(offsetLower.outerDisk, offsetUpper.outerDisk.strandsAcrossBottom.takeRight(offsetUpper.outerDisk.down - offsetLower.outerDisk.up))
    } else {
      identity(offsetLower.outerDisk)
    }
    val padUpper: PlanarTangle[S, L] = if (offsetUpper.outerDisk.down < offsetLower.outerDisk.up) {
      addStrandsOnRight(offsetUpper.outerDisk, offsetLower.outerDisk.strandsAcrossTop.takeRight(offsetLower.outerDisk.up - offsetUpper.outerDisk.down))
    } else {
      identity(offsetUpper.outerDisk)
    }
    compose(multiply(padLower.outerDisk, padUpper.outerDisk), List(compose(padLower, List(offsetLower)), compose(padUpper, List(offsetUpper))))
  }

  override def identity(boundary: Disk[S, L]): PlanarTangle[S, L] = {
    val spaghetti = for (
      (s, i) <- boundary.strands.zipWithIndex;
      initialTangentDirection = if (i < boundary.up) 1 else -1
    ) yield {
      Spaghetti(s, 1, i, 0, i, initialTangentDirection, 0)
    }
    ConnectedPlanarTangle(boundary :: boundary :: Nil, spaghetti, loops = Nil)
  }

}

final class UnorientedPlanarOperad[S <: SelfDualStrandType[S, L], L] extends PlanarOperad[S, L] {
  final override val oriented = false
}

final class OrientedPlanarOperad[S <: StrandType[S, L], L] extends PlanarOperad[S, L] {
  final override val oriented = true
}
