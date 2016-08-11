package org.fusionatlas.graphs

import org.fusionatlas.permutations.Involutions
import org.fusionatlas.permutations.Involutions._
import net.tqft.toolkit.mathematica.ShortMathematicaExpression
import net.tqft.toolkit.permutations.Permutations._

// FIXME insanity: involutions are 1-indexed, not 0-indexed
class BigraphWithDuals(val bigraph: Bigraph, val involutions: List[Involution]) extends Object with ShortMathematicaExpression with java.io.Serializable {
  require(involutions.head.size == bigraph.rankAtDepth(if (graphDepth % 2 == 0) graphDepth else graphDepth - 1))
  require(involutions.head.sorted == (1 to involutions.head.size).toList)

  def graphDepth = bigraph.graphDepth
  def rankAtDepth(depth: Int) = bigraph.rankAtDepth(depth)
  def totalRank = bigraph.totalRank
  def tightLoops = bigraph.tightLoops

  def involutionAtDepth(depth: Int): IndexedSeq[Int] = {
    require(depth % 2 == 0 && depth >= 0 && depth <= graphDepth + 1)
    if (depth == graphDepth + 1) {
      IndexedSeq.empty
    } else {
      involutions((graphDepth - (graphDepth % 2) - depth) / 2)
    }
  }

  def truncate = new BigraphWithDuals(bigraph.truncate, if (graphDepth % 2 == 0) involutions.tail else involutions)
  def translate(k: Int = 2): BigraphWithDuals = {
    require(k >= 0)
    require(k % 2 == 0)
    k match {
      case 0 => this
      case 2 => new BigraphWithDuals(bigraph.translate(2), involutions ::: List(IndexedSeq(1)))
      case k => translate(2).translate(k - 2)
    }
  }

  override def equals(x: Any) = {
    x match {
      case x: BigraphWithDuals => x.bigraph == bigraph && x.involutions == involutions
      case _ => false
    }
  }
  override def hashCode = {
    bigraph.hashCode + 13 * involutions.hashCode
  }
  override def toString() = {
    "bwd" + bigraph.toString.drop(3) + "duals" + (involutions.reverse.map { _.mkString("x") }).mkString("v")
  }

  override def toMathematicaInputString() = {
    "GraphFromString[\"" + toString + "\"]"
  }

  lazy val involutionFixedPoints = {
    involutions map { i => i.zipWithIndex count { case (t, i) => t - i == 1 } }
  }

  def rankZeroExtension = {
    require(graphDepth % 2 == 1)
    new BigraphWithDuals(bigraph.rankZeroExtension, IndexedSeq() :: involutions)
  }

  def permuteAtDepth(depth: Int, p: Permutation): BigraphWithDuals = {
    import net.tqft.toolkit.permutations.Permutations._

    val newInvolutions = if (depth % 2 == 0 && depth <= graphDepth) {
      val oldInvolution = involutionAtDepth(depth)
      val newInvolution = (p permute ((oldInvolution map { x => x - 1 }) permute p.inverse)) map { x => x + 1 }
      involutions.updated((graphDepth - (graphDepth % 2) - depth) / 2, newInvolution)
    } else {
      involutions
    }
    val newBigraph = bigraph.permuteAtDepth(depth, p)
    new BigraphWithDuals(newBigraph, newInvolutions)
  }

  def evenPart: FusionGraph = null // FIXME
}

object BigraphWithDuals {
  def apply(s: String): BigraphWithDuals = {
    require(s.startsWith("bwd"))
    s.split("duals").toList match {
      case List(g, d) => new BigraphWithDuals(Bigraph("gbg" + g.drop(3)), involutionsFromString(d))
      case _ => throw new UnsupportedOperationException
    }
  }

  def involutionsFromString(s: String): List[Involution] = {
    s.split('v').toList.reverse map { i => i.split('x').toIndexedSeq map { c => c.toInt } }
  }

  val ordering: Ordering[BigraphWithDuals] = new Ordering[BigraphWithDuals] {
    def compare(x: BigraphWithDuals, y: BigraphWithDuals): Int = {
      val d1 = Bigraph.ordering.compare(x.bigraph, y.bigraph)
      if (d1 != 0) return d1

      import Ordering.Implicits._
      implicitly[Ordering[Seq[Seq[Int]]]].compare(x.involutions, y.involutions)
    }
  }

  val isomorphismOrdering: Ordering[BigraphWithDuals] = new Ordering[BigraphWithDuals] {
    def compare(x: BigraphWithDuals, y: BigraphWithDuals): Int = {
      val d1 = Bigraph.isomorphismOrdering.compare(x.bigraph, y.bigraph)
      if (d1 != 0) return d1

      import Ordering.Implicits._
      implicitly[Ordering[Seq[Int]]].compare(x.involutionFixedPoints, y.involutionFixedPoints)
    }
  }

}