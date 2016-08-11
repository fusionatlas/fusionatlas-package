package org.fusionatlas.graphs

trait AbstractGraph {
  import AbstractGraph.{ trace, debug, info, warn, error, fatal }

  val graphDepth: Int
  def rankAtDepth(d: Int): Int
  def totalRank = (0 to graphDepth) map { rankAtDepth _ } reduceLeft { _ + _ }
  def rankAtMaximalDepth = rankAtDepth(graphDepth)

  type GraphVertex = (Int, Int)
  type GraphVector = Seq[Seq[Double]]

  def neighbours(v: GraphVertex): List[GraphVertex]
  def neighbours2(v: GraphVertex): List[GraphVertex] = neighbours(v) flatMap { neighbours(_) }

  def neighboursMultiplicities(v: GraphVertex): Seq[(GraphVertex, Int)] = {
    import net.tqft.toolkit.collections.Tally._
    neighbours(v).tally.toSeq
  }
  def neighbours2Multiplicities(v: GraphVertex): Seq[(GraphVertex, Int)] = {
    import net.tqft.toolkit.collections.Tally._
    neighbours2(v).tally.toSeq
  }

  def FPEigenvalueLowerBounds: Stream[Double]

  def isFPEigenvaluePossiblyBelow(d: Double): Boolean = {
    	if(scala.util.Random.nextDouble < 0.001)	info("checking eigenvalue: " + this)

    val bounds = FPEigenvalueLowerBounds

    val fixedPoint = (bounds zip bounds.tail).find({ case (b1, b2) => b2 > d || b2-b1 < (d-b2)/4 }).get._2
    fixedPoint < d

//    val m1 = 10; // any less than 5 and it seems that we say "true" too often
//    val m2 = 15; // even at 10 there are occasional unnecessary "true"s
//    
//    for (bound <- bounds.take(m1); if bound > d) {
//      //			info("over: " + bound)
//      return false
//    }
//
//    val geometricBounds: Stream[Double] = {
//      val triples = bounds zip bounds.tail zip bounds.tail.tail
//      triples map { case ((a, b), c) => (a * c - b * b) / (a - 2 * b + c) }
//    }
//
//    for ((bound, estimate) <- (bounds.drop(m1) zip geometricBounds.drop(m1 - 2)).take(m2 - m1)) {
//      if (bound > d) {
//        //				info("over: " + bound)
//        return false
//      }
//      if (estimate < d) {
//        //				info("geometric estimate says it looks okay: " + (bound, estimate))
//        return true
//      }
//    }
//    //		info("got bored")
//    return true;
  }

}

// this exists solely for the sake of slightly more descriptive Logging messages
object AbstractGraph extends Object with net.tqft.toolkit.Logging