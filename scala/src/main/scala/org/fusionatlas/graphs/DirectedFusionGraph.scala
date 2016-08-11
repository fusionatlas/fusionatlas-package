package org.fusionatlas.graphs

import net.tqft.toolkit.Logging
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.algebra.enumeration.Odometer
import org.fusionatlas.matrices._
import net.tqft.toolkit.permutations._
import net.tqft.toolkit.permutations.Permutations._

sealed trait AbstractDirectedFusionGraph extends AbstractGraph with Logging {
  // ACHTUNG: these matrices are in the *reverse* order from what you might expect. The *deepest* comes first.

  // Note: inclusions is now nextDepth
  val nextDepth: List[RectangularMatrix]
  val lastDepth: List[RectangularMatrix]
  val sameDepth: List[RectangularMatrix]
  val dualData: List[Permutation]

  assert(nextDepth.size == sameDepth.size - 1)
  assert(lastDepth.size == sameDepth.size - 1)
  assert(sameDepth.size == dualData.size)

  assert(lastDepth.isEmpty || lastDepth.head.numberOfTargets == nextDepth.head.numberOfSources)
  assert(lastDepth.isEmpty || lastDepth.head.numberOfSources == nextDepth.head.numberOfTargets)
  assert(lastDepth.isEmpty || nextDepth.head.numberOfTargets == sameDepth.head.numberOfSources)
  assert(sameDepth.head.numberOfSources == sameDepth.head.numberOfTargets)

  val graphDepth: Int = nextDepth.size

  def listOfCrossings = nextDepth map { _.crossings }

  private def rankAtDepthFromInclusions(d: Int) = {
    if (d < 0) {
      0
    } else if (d == 0) {
      1
    } else if (d == graphDepth) {
      nextDepth.head.numberOfTargets
    } else {
      nextDepth(graphDepth - d - 1).numberOfSources
    }
  }

  private def rankAtDepthFromSameDepth(d: Int) = {
    // sameDepth is no longer a symmetric matrix, so it has no method "size"
    sameDepth(graphDepth - d).numberOfSources
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
  /* this is implicitly: --- \tensor X */
  override def neighbours(v: GraphVertex): List[GraphVertex] = {
    val (d, k) = v

    val upNeighbours =
      if (d != graphDepth) {
        nextDepth(graphDepth - 1 - d).targets(k) map { (d + 1, _) }
      } else {
        Nil
      }

    val downNeighbours =
      if (d != 0) {
        lastDepth(graphDepth - d).targets(k) map { (d - 1, _) }
      } else {
        Nil
      }

    val sameNeighbours = sameDepth(graphDepth - d).targets(k) map { (d, _) }

    (upNeighbours ::: downNeighbours ::: sameNeighbours).sorted
  }

  def neighboursXdual(v: GraphVertex): List[GraphVertex] = {
    val (d, k) = v

    val upNeighbours =
      if (d != graphDepth) {
        lastDepth(graphDepth - 1 - d).sources(k) map { (d + 1, _) }
      } else {
        Nil
      }

    val downNeighbours =
      if (d != 0) {
        nextDepth(graphDepth - d).sources(k) map { (d - 1, _) }
      } else {
        Nil
      }

    val sameNeighbours = sameDepth(graphDepth - d).sources(k) map { (d, _) }

    (upNeighbours ::: downNeighbours ::: sameNeighbours).sorted
  }

  def leftNeighboursX(vertex: GraphVertex): List[GraphVertex] = {
    neighboursXdual(dual(vertex)) map dual
  }
  def rightNeighboursX(vertex: GraphVertex): List[GraphVertex] = {
    neighbours(vertex)
  }
  def leftNeighboursXdual(vertex: GraphVertex): List[GraphVertex] = {
    neighbours(dual(vertex)) map dual
  }
  def rightNeighboursXdual(vertex: GraphVertex): List[GraphVertex] = {
    neighboursXdual(vertex)
  }

  // FIXME write the correct associativity tests

  def partialAssociativityTest(): Boolean = {
    partialAssociativityTest(graphDepth) && partialAssociativityTest(graphDepth - 1)
  }
  def partialAssociativityTest(depth: Int): Boolean = {
    (for (k <- 1 to rankAtDepth(depth)) yield partialAssociativityTest((depth, k))) reduceLeft (_ && _)
  }
  def partialAssociativityTest(vertex: GraphVertex): Boolean = {
    import Ordering.Implicits._

    // the filter statement here ensures that we don't check associativity between two vertices at the maximal depth
    val s1 = leftNeighboursX(vertex) flatMap rightNeighboursX filter { _._1 + vertex._1 < 2 * graphDepth } sortWith { _ < _ }
    val s2 = rightNeighboursX(vertex) flatMap leftNeighboursX filter { _._1 + vertex._1 < 2 * graphDepth } sortWith { _ < _ }
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

    val s1 = leftNeighboursX(vertex) flatMap rightNeighboursX sortWith { _ < _ }
    val s2 = rightNeighboursX(vertex) flatMap leftNeighboursX sortWith { _ < _ }
    s1 == s2
  }

  override def toString(): String = {
    import net.tqft.toolkit.collections.FlexibleTranspose._
    (List(dualData.reverse map (_.map(_ + 1).mkString("[", ",", "]")), sameDepth.reverse map (_.niceToString), nextDepth.reverse map (_.niceToString), lastDepth.reverse map (_.niceToString)).flexibleTranspose.flatten).mkString("dfg ", ";", "")
  }

  def evenPart: FusionGraph = {
    // the even generator is X (x) X^* - 1
    def tensorWithEvenGenerator(v: GraphVertex) = {
      import net.tqft.toolkit.collections.DeleteOne._
      (rightNeighboursX(v) flatMap { rightNeighboursXdual(_) } deleteOne (v)).sorted
    }

    def verticesAtNextDepth(verticesAtLastDepth: List[GraphVertex], verticesAtSameDepth: List[GraphVertex]): List[GraphVertex] = {
      val unsorted = ((verticesAtSameDepth flatMap (tensorWithEvenGenerator)).toSet -- verticesAtLastDepth -- verticesAtSameDepth).toList
      /*
       * Now, put the dual pairs first, and the self dual vertices last.
       * Also, we order pairs accordings to the smaller element.
       */
      (((unsorted.map({ v => Set(v, dual(v)) }).distinct).sortBy({ s => (-s.size, s.min) })).map({ s => s.toList.sorted })).flatten
    }

    val verticesAtDepth = {
      lazy val _verticesAtDepth: Stream[List[GraphVertex]] =
        Stream.cons(List((0, 1)), Stream.cons(tensorWithEvenGenerator((0, 1)).toList,
          (_verticesAtDepth zip _verticesAtDepth.tail) map { case (l, s) => verticesAtNextDepth(l, s) }))

      _verticesAtDepth.takeWhile(_.nonEmpty).toList
    }

    def multiplicity(v1: GraphVertex, v2: GraphVertex) = {
      tensorWithEvenGenerator(v1).count(_ == v2)
    }

    val inclusions = for (d <- ((verticesAtDepth.size - 2) to 0 by -1).toList) yield {
      new RectangularMatrix(for (v1 <- verticesAtDepth(d + 1)) yield {
        for (v2 <- verticesAtDepth(d)) yield {
          multiplicity(v2, v1)
        }
      })
    }

    val sameDepth = for (d <- ((verticesAtDepth.size - 1) to 0 by -1).toList) yield {
      new SymmetricMatrix(for (v1 <- verticesAtDepth(d)) yield {
        for (v2 <- verticesAtDepth(d)) yield {
          multiplicity(v1, v2)
        }
      })
    }

    val dualData = for (d <- ((verticesAtDepth.size - 1) to 0 by -1).toList) yield {
      verticesAtDepth(d).map(v => verticesAtDepth(d).indexOf(dual(v))).toIndexedSeq
    }

    FusionGraph(inclusions, sameDepth, dualData)

  }

  def toPairOfBigraphsWithDuals: PairOfBigraphsWithDuals = {
    // FIXME
    throw new UnsupportedOperationException
  }

  def FPEigenvalueLowerBounds = {
    evenPart.FPEigenvalueLowerBounds map { x => scala.math.sqrt(x + 1) }
  }

}

// dfg's increment sameDepth
sealed trait DirectedFusionGraph extends AbstractDirectedFusionGraph with PersistentGraph[DirectedFusionGraph] {
  def truncate(k: Int = -1): DirectedFusionGraph = k match {
    case 0 => this
    case k if k < 0 => DirectedFusionGraph(nextDepth.tail, sameDepth.tail, lastDepth.tail, dualData.tail).truncate(k + 1)
    case k if k > 0 => graphDepth match {
      case d if d <= k => this
      case d if d > k => truncate(-1).truncate(k)
    }
  }

  def findIsomorphisms(other: DirectedFusionGraph): Iterator[List[Permutation]] = {
    import net.tqft.toolkit.permutations.Permutations._

    if (DirectedFusionGraph.isomorphismOrdering.compare(this, other) != 0) return Iterator.empty
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
              val nextDepthPermutations = Permutations.mapping(nextDepth.head.permuteColumns(p).asArray, other.nextDepth.head.asArray)
              val pairPermutation = dualData.head
              val dualDataPermutations = nextDepthPermutations.filter(q => (q permute pairPermutation) == (pairPermutation permute q))
              val allowedPermutations = dualDataPermutations.filter(q => {
                sameDepth.head.permuteRows(q).permuteColumns(q).asArray == other.sameDepth.head.asArray &&
                  lastDepth.head.permuteRows(p).permuteColumns(q).asArray == other.lastDepth.head.asArray
              })
              allowedPermutations
            }
            next map { _ :: l }
          }
        }
      }
    }
  }

}

// these incomplete dfg's increment nextDepth
// We just pass through all the Odometer methods to the head of nextDepth
// Note that this just modifies *the first row*.
sealed trait NextDirectedFusionGraph extends AbstractDirectedFusionGraph

// these incomplete dfg's increment lastDepth
// We pass through all the Odometer methods to the head of lastDepth, but we wrap this with RectangularMatrix.entireMatrixOdometer,
// which takes care of modifying all rows, not just the first row (which is the default behvaviour of RectangularMatrix).
sealed trait LastDirectedFusionGraph extends AbstractDirectedFusionGraph

object DirectedFusionGraph {
  def apply(s: String): DirectedFusionGraph = {
    require(s.startsWith("dfg"))

    def notBrokenSplit(s: String, c: Char): List[String] = {
      if (s.last == c) {
        notBrokenSplit(s.init, c) ::: List("")
      } else {
        s.split(c).toList
      }
    }

    // TODO: fix strings!
    import net.tqft.toolkit.collections.Stripes._
    val chunks = notBrokenSplit(s.drop(4), ';').stripes(4)
    val dualData = chunks(0).reverse.map({ p => p.ensuring(_.startsWith("[")).stripPrefix("[").ensuring(_.endsWith("]")).stripSuffix("]").split(",").map(_.toInt - 1): IndexedSeq[Int] })

    // have to jump through some hoops here to correctly handle the case with zero vertices at the maximal depth.
    val mostSameDepth = chunks(1).init.reverse map { m => new RectangularMatrix(m) }
    val mostInclusions = chunks(2).init.reverse map { m => new RectangularMatrix(m) }
    val mostLastDepths = chunks(3).init.reverse map { m => new RectangularMatrix(m) }

    val lastSameDepth = if (chunks(2).last == "") {
      new RectangularMatrix(0)
    } else {
      new RectangularMatrix(chunks(1).last)
    }
    val lastInclusion = if (chunks(2).last == "") {
      new RectangularMatrix(mostInclusions.head.numberOfTargets)
    } else {
      new RectangularMatrix(chunks(2).last)
    }
    val lastLastDepth = if (chunks(3).last == "") {
      new RectangularMatrix(mostLastDepths.head.numberOfTargets)
    } else {
      new RectangularMatrix(chunks(3).last)
    }

    val sameDepth = lastSameDepth :: mostSameDepth
    val nextDepth = lastInclusion :: mostInclusions
    val lastDepth = lastLastDepth :: mostLastDepths

    DirectedFusionGraph(nextDepth, sameDepth, lastDepth, dualData)
  }
  def unapply(s: String): Option[DirectedFusionGraph] = try {
    Some(apply(s))
  } catch {
    case _: Exception => None
  }

  def apply(nextDepth: List[RectangularMatrix], sameDepth: List[RectangularMatrix], lastDepth: List[RectangularMatrix], dualData: List[Permutation]): DirectedFusionGraph = {
    new DirectedFusionGraphImpl(nextDepth, sameDepth, lastDepth, dualData)
  }

  // We turn an LastDirectedFusionGraph into a DirectedFusionGraph when we've finished adding edges up and edges down,
  // and are ready to start adding constant depth edges.
  def apply(graph: LastDirectedFusionGraph): DirectedFusionGraph = {
    require(graph.nextDepth.head.connected_?)
    new DirectedFusionGraphImpl(graph.nextDepth, graph.sameDepth, graph.lastDepth, graph.dualData)
  }

  implicit val ordering: Ordering[DirectedFusionGraph] = new Ordering[DirectedFusionGraph] {
    def compare(x: DirectedFusionGraph, y: DirectedFusionGraph): Int = {
      val d1 = isomorphismOrdering.compare(x, y)
      if (d1 != 0) return d1

      import Ordering.Implicits._

      val d2 = implicitly[Ordering[Seq[Int]]].compare(x.listOfCrossings, y.listOfCrossings)
      if (d2 != 0) return d2

      return 0

    }
  }

  implicit val isomorphismOrdering: Ordering[DirectedFusionGraph] = new Ordering[DirectedFusionGraph] {
    def compare(x: DirectedFusionGraph, y: DirectedFusionGraph): Int = {
      import Ordering.Implicits._

      val d0 = x.graphDepth - y.graphDepth
      if (d0 != 0) return d0

      val d1 = implicitly[Ordering[Seq[Int]]].compare(x.listOfRanks, y.listOfRanks)
      if (d1 != 0) return d1

      return 0
    }
  }

  implicit object Odometer extends Odometer[DirectedFusionGraph] {
    private val odometer: Odometer[RectangularMatrix] = implicitly

    override def reset(o: DirectedFusionGraph) = DirectedFusionGraph(o.nextDepth, odometer.reset(o.sameDepth.head) :: o.sameDepth.tail, o.lastDepth, o.dualData)
    override def increment(o: DirectedFusionGraph) = {
      DirectedFusionGraph(o.nextDepth, odometer.increment(o.sameDepth.head) :: o.sameDepth.tail, o.lastDepth, o.dualData)
    }
    //TODO : is this right?
    override def carry(o: DirectedFusionGraph) = {
      odometer.carry(o.sameDepth.head) map { m: RectangularMatrix => DirectedFusionGraph(o.sameDepth, m :: o.sameDepth.tail, o.lastDepth, o.dualData) }
    }

  }

  private class DirectedFusionGraphImpl(
    val nextDepth: List[RectangularMatrix],
    val sameDepth: List[RectangularMatrix],
    val lastDepth: List[RectangularMatrix],
    val dualData: List[Permutation]) extends DirectedFusionGraph

}

object NextDirectedFusionGraph {
  def apply(graph: DirectedFusionGraph): NextDirectedFusionGraph = {
    NextDirectedFusionGraph(graph, new RectangularMatrix(graph.rankAtMaximalDepth), IndexedSeq.empty)
  }
  def apply(graph: DirectedFusionGraph, nextDepth: RectangularMatrix, duals: Permutation): NextDirectedFusionGraph = {
    NextDirectedFusionGraph(nextDepth :: graph.nextDepth, (new RectangularMatrix(nextDepth.numberOfTargets, nextDepth.numberOfTargets)) :: graph.sameDepth, (new RectangularMatrix(nextDepth.numberOfTargets, nextDepth.numberOfSources)) :: graph.lastDepth, duals :: graph.dualData)
  }
  def apply(nextDepth: List[RectangularMatrix], sameDepth: List[RectangularMatrix], lastDepth: List[RectangularMatrix], dualData: List[Permutation]): NextDirectedFusionGraph = {
    new NextDirectedFusionGraphImpl(nextDepth, sameDepth, lastDepth, dualData)
  }

  private class NextDirectedFusionGraphImpl(
    val nextDepth: List[RectangularMatrix],
    val sameDepth: List[RectangularMatrix],
    val lastDepth: List[RectangularMatrix],
    val dualData: List[Permutation]) extends NextDirectedFusionGraph

  implicit object Odometer extends Odometer[NextDirectedFusionGraph] {
    private val odometer: Odometer[RectangularMatrix] = implicitly

    override def reset(o: NextDirectedFusionGraph) = NextDirectedFusionGraph(odometer.reset(o.nextDepth.head) :: o.nextDepth.tail, o.sameDepth, o.lastDepth, o.dualData)
    override def increment(o: NextDirectedFusionGraph) = {
      NextDirectedFusionGraph(odometer.increment(o.nextDepth.head) :: o.nextDepth.tail, o.sameDepth, o.lastDepth, o.dualData)
    }
    override def carry(o: NextDirectedFusionGraph) = {
      odometer.carry(o.nextDepth.head) map { m: RectangularMatrix => NextDirectedFusionGraph(m :: o.nextDepth.tail, o.sameDepth, o.lastDepth, o.dualData) }
    }

  }
}

object LastDirectedFusionGraph {
  def apply(graph: NextDirectedFusionGraph): LastDirectedFusionGraph = {
    LastDirectedFusionGraph(graph.nextDepth, graph.sameDepth, graph.lastDepth, graph.dualData)
  }
  def apply(graph: NextDirectedFusionGraph, lastDepth: RectangularMatrix): LastDirectedFusionGraph = {
    LastDirectedFusionGraph(graph.nextDepth, graph.sameDepth, lastDepth :: graph.lastDepth.tail, graph.dualData)
  }
  def apply(nextDepth: List[RectangularMatrix], sameDepth: List[RectangularMatrix], lastDepth: List[RectangularMatrix], dualData: List[Permutation]): LastDirectedFusionGraph = {
    new LastDirectedFusionGraphImpl(nextDepth, sameDepth, lastDepth, dualData)
  }

  private class LastDirectedFusionGraphImpl(
    val nextDepth: List[RectangularMatrix],
    val sameDepth: List[RectangularMatrix],
    val lastDepth: List[RectangularMatrix],
    val dualData: List[Permutation]) extends LastDirectedFusionGraph

  implicit object Odometer extends Odometer[LastDirectedFusionGraph] {
    override def reset(o: LastDirectedFusionGraph) = LastDirectedFusionGraph(o.nextDepth, o.sameDepth, RectangularMatrix.entireMatrixOdometer.reset(o.lastDepth.head) :: o.lastDepth.tail, o.dualData)
    override def increment(o: LastDirectedFusionGraph) = {
      LastDirectedFusionGraph(o.nextDepth, o.sameDepth, RectangularMatrix.entireMatrixOdometer.increment(o.lastDepth.head) :: o.lastDepth.tail, o.dualData)
    }
    override def carry(o: LastDirectedFusionGraph) = {
      RectangularMatrix.entireMatrixOdometer.carry(o.nextDepth.head) map { m: RectangularMatrix =>
        LastDirectedFusionGraph(o.nextDepth, o.sameDepth, m :: o.lastDepth.tail, o.dualData)
      }
    }

  }
}