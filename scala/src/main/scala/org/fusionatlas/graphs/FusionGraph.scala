package org.fusionatlas.graphs

import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.algebra.enumeration.Odometer
import org.fusionatlas.matrices._
import net.tqft.toolkit.permutations._
import net.tqft.toolkit.permutations.Permutations._

sealed trait AbstractFusionGraph extends AbstractGraph with Logging {
  // ACHTUNG: these matrices are in the *reverse* order from what you might expect. The *deepest* comes first.

  val inclusions: List[RectangularMatrix]
  val dualData: List[Permutation]
  val sameDepth: List[SymmetricMatrix]

  val graphDepth: Int = inclusions.size

  def listOfCrossings = inclusions map { _.crossings }

  private def rankAtDepthFromInclusions(d: Int) = {
    if (d < 0) {
      0
    } else if (d == 0) {
      1
    } else if (d == graphDepth) {
      inclusions.head.numberOfTargets
    } else {
      inclusions(graphDepth - d - 1).numberOfSources
    }
  }

  private def rankAtDepthFromSameDepth(d: Int) = {
    sameDepth(graphDepth - d).size
  }

  def rankAtDepth(d: Int) = {
    val r1 = (0 to graphDepth) map { rankAtDepthFromInclusions(_) }
    val r2 = (0 to graphDepth) map { rankAtDepthFromSameDepth(_) }
    assert(rankAtDepthFromInclusions(d) == rankAtDepthFromSameDepth(d))
    rankAtDepthFromInclusions(d)
  }

  def listOfRanks = (0 to graphDepth).reverse map { rankAtDepth _ }

  def dual(vertex: GraphVertex): GraphVertex = {
    (vertex._1, dualData(graphDepth - vertex._1)(vertex._2))
  }

  // TODO, eventually, try memoizing this.
  override def neighbours(v: GraphVertex): List[GraphVertex] = {
    val (d, k) = v

    val upNeighbours = if (d != graphDepth) {
      inclusions(graphDepth - d - 1).targets(k) map { (d + 1, _) }
    } else {
      Nil
    }

    val downNeighbours = if (d != 0) {
      inclusions(graphDepth - d).sources(k) map { (d - 1, _) }
    } else {
      Nil
    }

    val sameNeighbours = sameDepth(graphDepth - d).neighbours(k) map { (d, _) }

    (upNeighbours ::: downNeighbours ::: sameNeighbours).sorted
  }

  def leftNeighbours(vertex: GraphVertex): List[GraphVertex] = {
    neighbours(dual(vertex)) map dual
  }
  def rightNeighbours(vertex: GraphVertex): List[GraphVertex] = {
    neighbours(vertex)
  }

  def partialAssociativityTest(): Boolean = {
    partialAssociativityTest(graphDepth) && partialAssociativityTest(graphDepth - 1)
  }
  def partialAssociativityTest(depth: Int): Boolean = {
    (for (k <- 1 to rankAtDepth(depth)) yield partialAssociativityTest((depth, k))) reduceLeft (_ && _)
  }
  def partialAssociativityTest(vertex: GraphVertex): Boolean = {
    import Ordering.Implicits._

    // the filter statement here ensures that we don't check associativity between two vertices at the maximal depth
    val s1 = leftNeighbours(vertex) flatMap rightNeighbours filter { _._1 + vertex._1 < 2 * graphDepth } sortWith { _ < _ }
    val s2 = rightNeighbours(vertex) flatMap leftNeighbours filter { _._1 + vertex._1 < 2 * graphDepth } sortWith { _ < _ }
    s1 == s2
  }
  def fullAssociativityTest(): Boolean = {
    fullAssociativityTest(graphDepth)
  }
  def fullAssociativityTest(depth: Int): Boolean = {
    (for (k <- 1 to rankAtDepth(depth)) yield fullAssociativityTest((depth, k))) reduceLeft (_ && _)
  }
  def fullAssociativityTest(vertex: GraphVertex): Boolean = {
    import Ordering.Implicits._

    val s1 = leftNeighbours(vertex) flatMap rightNeighbours sortWith { _ < _ }
    val s2 = rightNeighbours(vertex) flatMap leftNeighbours sortWith { _ < _ }
    s1 == s2
  }

  //This function should calculate dim(Hom(X\otimes A\otimes X,B)) given A,B
  def sizeOfConnection(vertex1: GraphVertex, vertex2: GraphVertex): Int = {
    val coincidences = (leftNeighbours(vertex1) flatMap rightNeighbours) filter (_ == vertex2)
    coincidences.size
  }

  lazy val FPEigenvalueLowerBounds: Stream[Double] = {
    import scala.math._ // so we can use sqrt and pow

    def applyAdjacencyMatrix(v: GraphVector): GraphVector = {
      val result = for (d <- (0 to graphDepth).toList) yield {
        for (k <- (1 to rankAtDepth(d)).toList) yield {
          (neighbours(d, k) map { case (d1, k1) => v(d1)(k1 - 1) }).foldLeft(0.0)(_ + _)
        }
      }
      result
    }

    val initialGuess: GraphVector = {
      for (d <- (0 to graphDepth).toList) yield {
        List.fill(rankAtDepth(d))(1.0)
      }
    }

    val EvenFPEigenvectorEstimates: Stream[GraphVector] = Stream.iterate(initialGuess)(applyAdjacencyMatrix _)
    def norm(v: GraphVector) = sqrt(v.flatten map { pow(_, 2) } reduceLeft { _ + _ })
    (EvenFPEigenvectorEstimates zip EvenFPEigenvectorEstimates.tail) map { case (v1, v2) => norm(v2) / norm(v1) }
  }

  override def toString(): String = {
    import net.tqft.toolkit.collections.FlexibleTranspose._
    List(dualData.reverse.map(_.map(_ + 1).mkString("x")), sameDepth.reverse, inclusions.reverse).flexibleTranspose.flatten.mkString("fg", "v", "")
  }

}

sealed trait FusionGraph extends AbstractFusionGraph with PersistentGraph[FusionGraph] {

  def translate(k: Int = 1): FusionGraph = k match {
    case 0 => this
    case k if k > 0 => FusionGraph("fg0v0v1v" + toString.drop(2)).translate(k - 1)
  }
  def truncate(k: Int = -1): FusionGraph = k match {
    case 0 => this
    case k if k < 0 => FusionGraph(inclusions.tail, sameDepth.tail, dualData.tail).truncate(k + 1)
    case k if k > 0 => graphDepth match {
      case d if d <= k => this
      case d if d > k => truncate(-1).truncate(k)
    }
  }

  override def findIsomorphisms(other: FusionGraph): Iterator[List[Permutation]] = {
    if (FusionGraph.isomorphismOrdering.compare(this, other) != 0) return Iterator.empty
    if (graphDepth == 0) {
      Seq(List(Permutations.identity(1))).iterator
    } else {
      if (dualData.head != other.dualData.head) {
        Iterator.empty
      } else {
        val shorterIsomorphisms = truncate(-1).findIsomorphisms(other.truncate(-1))
        trace("Looking for isomorphisms between " + this + " and " + other)
        shorterIsomorphisms flatMap { l: List[Permutation] =>
          {
            trace(". looking for ways to extend the isomorphism " + l)
            val p = l.head
            val next: Iterator[Permutation] = {
              val inclusionPermutations = Permutations.mapping(inclusions.head.permuteColumns(p).asArray, other.inclusions.head.asArray)
              val pairPermutation = dualData.head
              val dualDataPermutations = inclusionPermutations.filter(q => (q permute pairPermutation) == (pairPermutation permute q))
              val allowedPermutations = dualDataPermutations.filter(q => sameDepth.head.permuteRowsAndColumns(q).asArray == other.sameDepth.head.asArray)
              allowedPermutations
            }
            next map { _ :: l }
          }
        }
      }
    }
  }

}

sealed trait IncompleteFusionGraph extends AbstractFusionGraph

object FusionGraph {
  def apply(s: String): FusionGraph = {
    require(s.startsWith("fg"))

    def notBrokenSplit(s: String, c: Char): List[String] = {
      if (s.last == c) {
        notBrokenSplit(s.init, c) ::: List("")
      } else {
        s.split(c).toList
      }
    }

    if (s == "fg1v0") {
      return FusionGraph(List(), List(new SymmetricMatrix(1)), List(IndexedSeq(0)))
    }

    import net.tqft.toolkit.collections.Stripes._
    val chunks = notBrokenSplit(s.drop(2), 'v').stripes(3)
    val dualData = chunks(0).reverse map { _.split("x").filter(_.nonEmpty).map(_.toInt - 1): IndexedSeq[Int] }

    // have to jump through some hoops here to correctly handle the case with zero vertices at the maximal depth.
    val mostSameDepth = chunks(1).init.reverse map { m => new SymmetricMatrix(m) }
    val mostInclusions = chunks(2).init.reverse map { m => new RectangularMatrix(m) }

    val lastSameDepth = if (chunks(2).last == "") {
      new SymmetricMatrix(0)
    } else {
      new SymmetricMatrix(chunks(1).last)
    }
    val lastInclusion = if (chunks(2).last == "") {
      new RectangularMatrix(mostInclusions.head.numberOfTargets)
    } else {
      new RectangularMatrix(chunks(2).last)
    }

    val sameDepth = lastSameDepth :: mostSameDepth
    val inclusions = lastInclusion :: mostInclusions

    FusionGraph(inclusions, sameDepth, dualData)
  }
  def unapply(s: String): Option[FusionGraph] = try {
    Some(apply(s))
  } catch {
    case _: Exception => None
  }

  def apply(inclusions: List[RectangularMatrix], sameDepth: List[SymmetricMatrix], dualData: List[Permutation]): FusionGraph = new FusionGraphImpl(inclusions, sameDepth, dualData)

  // We turn an IncompleteFusionGraph into a FusionGraph when we've finished adding edges from one depth to the next
  // and are ready to start adding constant depth edges.
  def apply(graph: IncompleteFusionGraph): FusionGraph = {
    // sanity check:
    // make sure all the new vertices are actually connected
    require(graph.inclusions.head.connected_?)
    new FusionGraphImpl(graph.inclusions, graph.sameDepth, graph.dualData)
  }

  implicit val ordering: Ordering[FusionGraph] = new Ordering[FusionGraph] {
    def compare(x: FusionGraph, y: FusionGraph): Int = {
      val d1 = isomorphismOrdering.compare(x, y)
      if (d1 != 0) return d1

      import Ordering.Implicits._

      val d2 = implicitly[Ordering[Seq[Int]]].compare(x.listOfCrossings, y.listOfCrossings)
      if (d2 != 0) return d2

      return 0

    }
  }

  implicit val isomorphismOrdering: Ordering[FusionGraph] = new Ordering[FusionGraph] {
    def compare(x: FusionGraph, y: FusionGraph): Int = {
      import Ordering.Implicits._

      val d0 = x.graphDepth - y.graphDepth
      if (d0 != 0) return d0

      val d1 = implicitly[Ordering[Seq[Int]]].compare(x.listOfRanks, y.listOfRanks)
      if (d1 != 0) return d1

      return 0
    }
  }

  implicit object Odometer extends Odometer[FusionGraph] {
    private val odometer: Odometer[SymmetricMatrix] = implicitly
    override def reset(o: FusionGraph) = FusionGraph(o.inclusions, odometer.reset(o.sameDepth.head) :: o.sameDepth.tail, o.dualData)
    override def increment(o: FusionGraph) = {
      FusionGraph(o.inclusions, odometer.increment(o.sameDepth.head) :: o.sameDepth.tail, o.dualData)
    }
    override def carry(o: FusionGraph) = {
      odometer.carry(o.sameDepth.head).map({ m: SymmetricMatrix => FusionGraph(o.inclusions, m :: o.sameDepth.tail, o.dualData) })
    }

  }

  private class FusionGraphImpl(val inclusions: List[RectangularMatrix], val sameDepth: List[SymmetricMatrix], val dualData: List[Permutation]) extends FusionGraph {
    assert(inclusions.size == sameDepth.size - 1)
    assert(sameDepth.size == dualData.size)
    assert(inclusions.isEmpty || inclusions.head.numberOfTargets != 0 || sameDepth.head.size == 0)
  }

}

object IncompleteFusionGraph {
  def apply(s: String): IncompleteFusionGraph = {
    assert(s.endsWith("vvv"))
    IncompleteFusionGraph(FusionGraph(s.substring(0, s.size - 3)))
  }
  def apply(graph: FusionGraph): IncompleteFusionGraph = {
    IncompleteFusionGraph(graph, new RectangularMatrix(graph.rankAtMaximalDepth), IndexedSeq.empty)
  }
  def apply(graph: FusionGraph, inclusion: RectangularMatrix, dualData: Permutation): IncompleteFusionGraph = {
    IncompleteFusionGraph(inclusion :: graph.inclusions, (new SymmetricMatrix(inclusion.numberOfTargets)) :: graph.sameDepth, dualData :: graph.dualData)
  }
  def apply(inclusions: List[RectangularMatrix], sameDepth: List[SymmetricMatrix], dualData: List[Permutation]): IncompleteFusionGraph = {
    new IncompleteFusionGraphImpl(inclusions, sameDepth, dualData)
  }

  implicit object Odometer extends Odometer[IncompleteFusionGraph] {
    private val odometer: Odometer[RectangularMatrix] = implicitly
    override def reset(o: IncompleteFusionGraph) = IncompleteFusionGraph(odometer.reset(o.inclusions.head) :: o.inclusions.tail, o.sameDepth, o.dualData)
    override def increment(o: IncompleteFusionGraph) = {
      IncompleteFusionGraph(odometer.increment(o.inclusions.head) :: o.inclusions.tail, o.sameDepth, o.dualData)
    }
    override def carry(o: IncompleteFusionGraph) = {
      odometer.carry(o.inclusions.head).map({ m: RectangularMatrix => IncompleteFusionGraph(m :: o.inclusions.tail, o.sameDepth, o.dualData) })
    }

  }

  private class IncompleteFusionGraphImpl(
    val inclusions: List[RectangularMatrix],
    val sameDepth: List[SymmetricMatrix],
    val dualData: List[Permutation]) extends IncompleteFusionGraph {

    assert(inclusions.size == sameDepth.size - 1)
    assert(sameDepth.size == dualData.size)
    assert(inclusions.head.numberOfTargets != 0 || sameDepth.head.size == 0)
  }
}