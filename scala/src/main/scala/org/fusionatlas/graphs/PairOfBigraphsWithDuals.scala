package org.fusionatlas.graphs

import net.tqft.toolkit.Logging
import net.tqft.toolkit.mathematica.ShortMathematicaExpression
import net.tqft.toolkit.permutations._
import net.tqft.toolkit.permutations.Permutations._
import org.fusionatlas.matrices.RectangularMatrix
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.functions.Memo
import net.tqft.toolkit.collections.CachingIterable
import net.tqft.toolkit.collections.SynchronizedIterable
import net.tqft.toolkit.arithmetic.BinomialCoefficient
import net.tqft.toolkit.arithmetic.Factorial
import net.tqft.toolkit.collections.RemoveDuplicates

class PairOfBigraphsWithDuals(val gg0: BigraphWithDuals, val gg1: BigraphWithDuals) extends PersistentGraph[PairOfBigraphsWithDuals] with ShortMathematicaExpression {
  import PairOfBigraphsWithDuals.{ trace, debug, info, warn, error, fatal } // access to logging statements

  require(scala.math.abs(gg0.graphDepth - gg1.graphDepth) <= 1)

  override def equals(x: Any) = {
    x match {
      case x: PairOfBigraphsWithDuals => x.gg0 == gg0 && x.gg1 == gg1
      case _ => false
    }
  }
  override def hashCode = {
    gg0.hashCode + 13 * gg1.hashCode
  }

  //  override def toString = "PairOfBigraphsWithDuals(\"" + gg0 + "\", \"" + gg1 + "\")"
  override def toString = "(" + gg0 + ", " + gg1 + ")"

  override def toMathematicaInputString = "{" + gg0.toMathematicaInputString + ", " + gg1.toMathematicaInputString + "}"

  val graphDepth = gg0.graphDepth max gg1.graphDepth
  def totalRank = gg0.totalRank + gg1.totalRank

  def isFPEigenvaluePossiblyBelow(d: Double) = gg0.bigraph.isFPEigenvaluePossiblyBelow(d)

  private lazy val truncateByOne = {
    val (h0, h1) = (gg0.graphDepth - gg1.graphDepth) match {
      case -1 => (gg0, gg1.truncate)
      case 1 => (gg0.truncate, gg1)
      case 0 => (gg0.truncate, gg1.truncate)
    }
    new PairOfBigraphsWithDuals(h0, h1)
  }

  def truncate(k: Int = -1): PairOfBigraphsWithDuals = k match {
    case 0 => this
    case -1 => truncateByOne
    case k if k < 0 => truncate(-1).truncate(k + 1)
    case k if k > 0 => graphDepth match {
      case d if d <= k => this
      case d if d > k => truncateByOne.truncate(k)
    }
  }

  def translate(k: Int = 2): PairOfBigraphsWithDuals = {
    new PairOfBigraphsWithDuals(gg0.translate(k), gg1.translate(k))
  }

  def switch = new PairOfBigraphsWithDuals(gg1, gg0)

  def sorted = {
    if (BigraphWithDuals.ordering.compare(gg0, gg1) <= 0) {
      this
    } else {
      switch
    }
  }

  def permuteToReduceCrossings(deep: Boolean = false): PairOfBigraphsWithDuals = {
    def permutations(n: Int, m: Int): Iterator[Permutation] = if (Factorial(n) > m) Iterator((0 until n).toIndexedSeq) ++ Permutations.randomPermutationsOf(n).take(m - 1) else Permutations.of(n)

    def minimizeCrossingsAtDepth(g: PairOfBigraphsWithDuals, k: Int): PairOfBigraphsWithDuals = {
      val permutedGraphs: Iterator[PairOfBigraphsWithDuals] = if (k % 2 == 0) {
        for (p1 <- permutations(g.gg0.rankAtDepth(k), 10); p2 <- permutations(g.gg1.rankAtDepth(k), 10)) yield new PairOfBigraphsWithDuals(g.gg0.permuteAtDepth(k, p1), g.gg1.permuteAtDepth(k, p2))
      } else {
        for (p <- permutations(g.gg0.rankAtDepth(k), 100)) yield new PairOfBigraphsWithDuals(g.gg0.permuteAtDepth(k, p), g.gg1.permuteAtDepth(k, p))
      }
      import net.tqft.toolkit.collections.Iterators._
      permutedGraphs findMinimum ({ h => (h.gg0.bigraph.inclusions.reverse.slice(k - 1, k + 1) ::: h.gg1.bigraph.inclusions.reverse.slice(k - 1, k + 1)).map(_.crossings).reduceLeft(_ + _) }, lowerBound = Some(0))
    }
    val result = if (deep) {
      (0 to graphDepth).foldLeft(this)(minimizeCrossingsAtDepth _)
    } else {
      minimizeCrossingsAtDepth(this, graphDepth)
    }
    result
  }

  private def permuteColumns(matrix: RectangularMatrix, q: Permutation): List[IndexedSeq[Int]] = {
    matrix.asArray map { row => q permute row }
  }
  private def permuteRows(matrix: RectangularMatrix, q: Permutation): IndexedSeq[List[Int]] = {
    q permute matrix.asArray
  }

  // as usual, the returned Lists start at the deepest depth!
  override def findIsomorphisms(otherPair: PairOfBigraphsWithDuals): Iterator[List[(Permutation, Permutation)]] = {
    PairOfBigraphsWithDuals.findIsomorphismsCounter = PairOfBigraphsWithDuals.findIsomorphismsCounter + 1

    //		if(graphDepth != otherPair.graphDepth) return Iterable()
    if (PairOfBigraphsWithDuals.isomorphismOrdering.compare(this, otherPair) != 0) return Iterator.empty
    if (graphDepth == 0) {
      Seq(List((Permutations.identity(1), Permutations.identity(1)))).iterator
    } else {
      val shorterIsomorphisms = PairOfBigraphsWithDuals.findIsomorphismsCached(truncate(), otherPair.truncate()).iterator
      //       trace("Looking for isomorphisms between " + this + " and " + otherPair)
      val result = shorterIsomorphisms flatMap { p: List[Pair[Permutation, Permutation]] =>
        {
          //          trace(". looking for ways to extend the isomorphism " + p)

          val lastPair = p.head

          val nextPairs: Iterator[Pair[Permutation, Permutation]] = if (graphDepth % 2 == 0) {
            // If we're at an even depth, we can have two different permutations.
            def involutionPredicate(h1: BigraphWithDuals, h2: BigraphWithDuals): (Permutation => Boolean) = { q: Permutation =>
              (q permute ((h1.involutions.head) map { x => x - 1 } permute q.inverse)) == (h2.involutions.head map { x => x - 1 })
            }

            //            trace(".. looking for permutations for the first graph")
            val p1 = if (gg0.graphDepth < graphDepth) {
              Seq(Permutations.identity(0)).iterator
            } else {
              val t1 = Permutations.mappingCached(
                permuteColumns(gg0.bigraph.inclusions.head, lastPair._1), otherPair.gg0.bigraph.inclusions.head.asArray)
              //              trace(".. found permutations that match the inclusions matrices: " + t1)
              val t2 = t1 filter involutionPredicate(gg0, otherPair.gg0)
              //              trace(".. of those, these also match the duality involution: " + t2)
              t2
            }
            //            trace(".. looking for permutations for the second graph")
            val p2 = if (gg1.graphDepth < graphDepth) {
              Seq(Permutations.identity(0))
            } else {
              //              trace("... comparing matrices: " + permuteColumns(gg1.bigraph.inclusions.head, lastPair._2) + " and " + otherPair.gg1.bigraph.inclusions.head.asArray)
              val t1 = Permutations.mappingCached(
                permuteColumns(gg1.bigraph.inclusions.head, lastPair._2), otherPair.gg1.bigraph.inclusions.head.asArray)
              //              trace(".. found permutations that match the inclusions matrices: " + t1)
              val t2 = t1 filter involutionPredicate(gg1, otherPair.gg1)
              //              trace(".. of those, these also match the duality involution: " + t2)
              t2.toStream
            }
            for (q1 <- p1; q2 <- p2) yield { (q1, q2) }
          } else {
            // If we're at an odd depth, we want a single permutation, repeated twice.
            //            trace(".. looking for permutations for both graphs")
            val p1 = Permutations.mappingCached(
              permuteColumns(gg0.bigraph.inclusions.head, lastPair._1),
              otherPair.gg0.bigraph.inclusions.head.asArray) filter { q: Permutation =>
                (q permute permuteColumns(gg1.bigraph.inclusions.head, lastPair._2)) == otherPair.gg1.bigraph.inclusions.head.asArray
              }
            //            trace(".. found permutations: " + p1)
            for (q1 <- p1) yield { (q1, q1) }
          }
          nextPairs map { _ :: p }
        }
      }
      result
    }
  }

  override def isIsomorphicTo(otherPair: PairOfBigraphsWithDuals) = (findIsomorphisms(otherPair).nonEmpty) || (findIsomorphisms(otherPair.switch).nonEmpty)

  def automorphisms = findIsomorphisms(this)
  // TODO produce automorphism group orbits at the maximal depth -- and perhaps cut down on the odometer?

  def graphVertices = for ((graphNumber, graph) <- List((0, gg0), (1, gg1))) yield {
    for (depth <- (0 to graph.graphDepth).toList) yield {
      for (index <- (1 to graph.rankAtDepth(depth)).toList) yield {
        GraphVertex(this, graphNumber, depth, index)
      }
    }
  }
  def allVertices = graphVertices.flatten.flatten.toSet

  lazy val danglingQuadruplePoints = {
    def quadruplePointQ(v: GraphVertex) = v.rightNeighbours.size == 4
    def danglingQ(v: GraphVertex) = ((v.rightNeighbours map (_.rightNeighbours.size)) count (_ == 1)) == 2

    allVertices filter (_.depth <= graphDepth - 2) filter quadruplePointQ filter danglingQ filter (v => quadruplePointQ(v.dual)) filter (v => danglingQ(v.dual))
  }

  // this method tries to use combinatorial data to decide which vertices must have the same dimensions
  // there's more it could do, but very few cases of the triple point obstruction rely on this. 
  lazy val verticesByDimension: Set[Set[GraphVertex]] = {
    val mostVertices = (graphVertices map { l: List[List[GraphVertex]] => l.init }).flatten.flatten.toSet
    val deepestVertices = (graphVertices map { l: List[List[GraphVertex]] => l.last }).flatten.toSet

    val dualPairs = allVertices map { v: GraphVertex => Set(v, v.dual) }
    val deepestClumps = deepestVertices map { v: GraphVertex => Set(v, v.dual) }

    def clump(clumps: Set[Set[GraphVertex]])(v: GraphVertex) = clumps.find(_ contains v).get

    import Ordering.Implicits._
    def newClumps(clumps: Set[Set[GraphVertex]]): Set[Set[GraphVertex]] = {
      mostVertices.groupBy(v => (v.rightNeighbours map { w => clump(clumps)(w) }).sortBy(_.toSeq.sorted)).values.toSet ++ deepestClumps
    }
    def combineClumps(clumps1: Set[Set[GraphVertex]], clumps2: Set[Set[GraphVertex]]) = {
      clumps1 map { set => set flatMap { clump(clumps2)(_) } }
    }
    def updateClumps(clumps: Set[Set[GraphVertex]]) = combineClumps(clumps, newClumps(clumps))

    val stream = Stream.iterate(dualPairs)(updateClumps _)
    ((stream zip stream.tail) find { case (a, b) if a == b => true; case _ => false }).get._1
  }

  //Check if two vertices are in the same clump from verticesByDimension
  def combinatorialSameDimensionsQ(vertex1: GraphVertex, vertex2: GraphVertex): Boolean = {
    //see if vertex1 and vertex2 in same clump
    ((verticesByDimension find (_ contains vertex1)).get) contains vertex2
  }

  // This function should calculate dim(Hom(X\otimes A\otimes X,B)) given A,B
  def sizeOfConnection(vertex1: GraphVertex, vertex2: GraphVertex): Int = {
    val coincidences = (vertex1.leftNeighbours flatMap { _.rightNeighbours }) filter (_ == vertex2)
    coincidences.size
  }

  private def associativityTest(v: GraphVertex) = {
    val s1 = ((v.leftNeighbours) flatMap { _.rightNeighbours }) filter (_.depth == v.depth) sortWith { _ < _ }
    val s2 = ((v.rightNeighbours) flatMap { _.leftNeighbours }) filter (_.depth == v.depth) sortWith { _ < _ }
    s1 == s2
  }
  private def upwardsAssociativityTest(v: GraphVertex) = {
    val s1 = ((((v.leftNeighbours) filter (_.depth == v.depth + 1)) flatMap { _.rightNeighbours }) filter (_.depth == v.depth + 2)) sortWith { _ < _ }
    val s2 = ((((v.rightNeighbours) filter (_.depth == v.depth + 1)) flatMap { _.leftNeighbours }) filter (_.depth == v.depth + 2)) sortWith { _ < _ }
    s1 == s2
  }
  private def vertexPairAssociativityTest(v1: GraphVertex, v2: GraphVertex) = {
    val v2l = v2.leftNeighbourMultiplicities.toMap
    val v2r = v2.rightNeighbourMultiplicities.toMap
    ((for ((k, v) <- v1.leftNeighbourMultiplicities) yield v * v2r.getOrElse(k, 0)).sum) == ((for ((k, v) <- v1.rightNeighbourMultiplicities) yield v * v2l.getOrElse(k, 0)).sum)
  }

  def leftExtensionAssociativityTest: Boolean = {
    require(gg0.graphDepth == gg1.graphDepth + 1)
    if (gg0.graphDepth % 2 == 0) {
      for (
        i <- 1 to gg1.bigraph.rankAtDepth(gg1.graphDepth - 1);
        v = GraphVertex(this, 1, gg1.graphDepth - 1, i)
      ) {
        if (!upwardsAssociativityTest(v)) return false
      }
      true
    } else {
      true
    }
  }
  def rightExtensionAssociativityTest = switch.leftExtensionAssociativityTest

  def leftExtensionOddOddMultiplicities: List[List[Int]] = {
    require(gg0.graphDepth == gg1.graphDepth + 1)
    val (depth1, depth2) = if (gg0.graphDepth % 2 == 0) { (gg1.graphDepth, gg1.graphDepth) } else { (gg0.graphDepth - 2, gg0.graphDepth) }
    for (
      i <- (0 until gg0.bigraph.rankAtDepth(depth1)).toList;
      v = graphVertices(0)(depth1)(i)
    ) yield {
      import net.tqft.toolkit.collections.Tally._
      val c = v.rightNeighbours.flatMap(_.leftNeighbours).tally.toMap
      for (
        j <- (0 until gg1.bigraph.rankAtDepth(depth2)).toList;
        w = graphVertices(1)(depth2)(j)
      ) yield {
        c.getOrElse(w, 0)
      }
    }
  }
  def rightExtensionOddOddMultiplicities = switch.leftExtensionOddOddMultiplicities

  def partialOcneanuTest: Boolean = {
    PairOfBigraphsWithDuals.associativityTestCounter = PairOfBigraphsWithDuals.associativityTestCounter + 1

    def partialOcneanuTest1: Boolean = {
      // check pairs of vertices at depth-2 and depth,
      for (
        i <- 1 to gg0.bigraph.rankAtDepth(graphDepth - 2);
        v = GraphVertex(this, 0, graphDepth - 2, i)
      ) {
        if (!upwardsAssociativityTest(v)) return false
      }
      for (
        i <- 1 to gg1.bigraph.rankAtDepth(graphDepth - 2);
        v = GraphVertex(this, 1, graphDepth - 2, i)
      ) {
        if (!upwardsAssociativityTest(v)) return false
      }
      true
    }
    def partialOcneanuTest2: Boolean = {
      // ... and pairs of vertices at depth-1
      for (
        i <- 1 to gg0.bigraph.rankAtDepth(graphDepth - 1);
        v = GraphVertex(this, 0, graphDepth - 1, i)
      ) {
        if (!associativityTest(v)) return false
      }
      for (
        i <- 1 to gg1.bigraph.rankAtDepth(graphDepth - 1);
        v = GraphVertex(this, 1, graphDepth - 1, i)
      ) {
        if (!associativityTest(v)) return false
      }
      true
    }

    return partialOcneanuTest1 && partialOcneanuTest2
  }

  // check pairs of vertices at depth.
  def fullOcneanuTest: Boolean = {
    if (!partialOcneanuTest) return false
    if (gg0.graphDepth == graphDepth) {
      for (
        i <- 1 to gg0.bigraph.rankAtDepth(graphDepth);
        v = GraphVertex(this, 0, graphDepth, i)
      ) {
        if (!associativityTest(v)) return false
      }
    }
    if (gg1.graphDepth == graphDepth) {
      for (
        i <- 1 to gg1.bigraph.rankAtDepth(graphDepth);
        v = GraphVertex(this, 1, graphDepth, i)
      ) {
        if (!associativityTest(v)) return false
      }
    }
    true
  }
}

object PairOfBigraphsWithDuals extends Logging {
  var associativityTestCounter = 0
  var findIsomorphismsCounter = 0

  def apply(s1: String, s2: String): PairOfBigraphsWithDuals = {
    new PairOfBigraphsWithDuals(BigraphWithDuals(s1), BigraphWithDuals(s2))
  }
  def apply(s: String): PairOfBigraphsWithDuals = {
    //    val pattern = """PairOfBigraphsWithDuals\("(bwd[0-9vpx]*duals[0-9vx]*)", "(bwd[0-9vpx]*duals[0-9vx]*)"\)""".r
    val pattern = """\((bwd[0-9vpx]*duals[0-9vx]*), (bwd[0-9vpx]*duals[0-9vx]*)\)""".r

    s match {
      case pattern(g1, g2) => PairOfBigraphsWithDuals(g1, g2)
      case _ => throw new IllegalArgumentException("Malformed string for PairOfBigraphsWithDuals: " + s)
    }

  }
  def unapply(s: String): Option[PairOfBigraphsWithDuals] = try {
    Some(apply(s))
  } catch {
    case _: Exception => None
  }

//  lazy val findIsomorphismsCached = { p: (PairOfBigraphsWithDuals, PairOfBigraphsWithDuals) => (p._1.toString, p._2.toString) } andThen Memo.softly { s: (String, String) => NonStrictIterable.from(PairOfBigraphsWithDuals(s._1).findIsomorphisms(PairOfBigraphsWithDuals(s._2)).toList) }
  lazy val findIsomorphismsCached = Memo.softly { p: (PairOfBigraphsWithDuals, PairOfBigraphsWithDuals) => p._1.findIsomorphisms(p._2).toList }

  def removeDuplicates0(graphs: Iterable[PairOfBigraphsWithDuals]) = {
    import RemoveDuplicates._

    def isomorphic(p1: PairOfBigraphsWithDuals, p2: PairOfBigraphsWithDuals) = p1.isIsomorphicTo(p2)
    graphs.toSeq.removeDuplicatesAndSort(isomorphic, PairOfBigraphsWithDuals.ordering)
  }
  def removeDuplicates(graphs: Iterable[PairOfBigraphsWithDuals]) = {
    import net.tqft.toolkit.collections.GroupBy._
    import net.tqft.toolkit.collections.Tally._
    import Ordering.Implicits._
    import net.tqft.toolkit.orderings.LexicographicOrdering.mapOrdering

    var counter = 0

    import net.tqft.toolkit.functions.Memo
    val isomorphic = { (p1: PairOfBigraphsWithDuals, p2: PairOfBigraphsWithDuals) => counter = counter + 1; if (counter % 100000 == 0) { info("checked " + counter + " isomorphisms, currently looking at " + p1 + ", " + p2); }; p1.isIsomorphicTo(p2) }

    // 315414586
    //  39898008
    //  18886306
    def graphInvariant(g: BigraphWithDuals) = {
      val i = g.bigraph.inclusions.head
      val ia = i.asArray
      val p = (i * (i.transpose)).asArray
      (
        g.bigraph.rankAtMaximalDepth,
        g.involutionFixedPoints.head,
        ia.map(_.tally).tally.toMap,
        ia.transpose.map(_.tally).tally.toMap,
        p.map(_.tally).tally.toMap,
        p.transpose.map(_.tally).tally.toMap)
    }

    val pairInvariant = { p: PairOfBigraphsWithDuals => List(graphInvariant(p.gg0), graphInvariant(p.gg1)).sorted }

    //    val groups = graphs.par.map({g => (g,pairInvariant(g))}).seq.groupBy(_._2).values.toList.map(_.map(_._1)).par
    //    info("chopped into groups of size: " + groups.map(_.size).toList)
    //    info("at most ~" + groups.map(x => x.size * x.size / 2).sum + " isomorphisms to check...")
    //    val result = groups.flatMap(_.lazilyChooseRepresentatives(isomorphic, pairInvariant).toList).toList

    // FIXME we used to have a parallel version of this! (parallelChooseRepresentatives)
    val result = graphs.chooseEquivalenceClassRepresentatives(isomorphic, pairInvariant).toList

    info("checked " + counter + " isomorphisms")

    result
  }

  implicit val ordering: Ordering[PairOfBigraphsWithDuals] = new Ordering[PairOfBigraphsWithDuals] {
    def compare(x: PairOfBigraphsWithDuals, y: PairOfBigraphsWithDuals): Int = {
      val d1 = BigraphWithDuals.ordering.compare(x.gg0, y.gg0)
      if (d1 != 0) return d1

      val d2 = BigraphWithDuals.ordering.compare(x.gg1, y.gg1)
      if (d2 != 0) return d2

      return 0
    }
  }

  implicit val isomorphismOrdering: Ordering[PairOfBigraphsWithDuals] = new Ordering[PairOfBigraphsWithDuals] {
    def compare(x: PairOfBigraphsWithDuals, y: PairOfBigraphsWithDuals): Int = {
      val d1 = BigraphWithDuals.isomorphismOrdering.compare(x.gg0, y.gg0)
      if (d1 != 0) return d1

      val d2 = BigraphWithDuals.isomorphismOrdering.compare(x.gg1, y.gg1)
      if (d2 != 0) return d2

      return 0
    }
  }

}



