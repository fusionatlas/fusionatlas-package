package org.fusionatlas.graphs

import org.fusionatlas.matrices._
import net.tqft.toolkit.algebra.enumeration.Odometer
import net.tqft.toolkit.mathematica.ShortMathematicaExpression
import net.tqft.toolkit.arithmetic.BinomialCoefficient
import net.tqft.toolkit.functions.Memo
import net.tqft.toolkit.permutations.Permutations._

sealed trait Bigraph extends AbstractGraph with ShortMathematicaExpression with java.io.Serializable {

  // ACHTUNG: these matrices are in the *reverse* order from what you might expect. The *deepest* comes first.
  val inclusions: List[RectangularMatrix]

  def inclusionAtDepth(k: Int) = inclusions(graphDepth - k - 1)

  val graphDepth: Int = inclusions.size

  def tightLoops = {
    (inclusions reduceLeft { _ * _ }).normSquared
  }

  lazy val loops = {
    val loopsOfLength0 = List(((0, 1), 1))
    def collectMultiplicities[A](multiplicities: List[(A, Int)]): List[(A, Int)] = {
      (multiplicities.groupBy(_._1) mapValues { _.foldLeft(0)(_ + _._2) }).toList
    }

    val n2m = Memo(neighbours2Multiplicities _)

    Iterable.iterate(loopsOfLength0, graphDepth + 1)(
      { l: List[(GraphVertex, Int)] =>
        collectMultiplicities(l flatMap { case (v, k) => n2m(v) map { case (w, m) => (w, m * k) } })
      }).toList map (_.collect({ case ((0, 1), k) => k }).apply(0))
  }
  lazy val annularMultiplicities: List[Long] = {
    val loops0 = loops

    1L :: 0L :: (for (r <- (2 to graphDepth).toList) yield {
      (for (n <- (0 to r).toList) yield {
        (if ((r - n) % 2 == 0) 1 else -1) * 2 * r * BinomialCoefficient(r + n, r - n) / (r + n) * loops0(n)
      }).foldLeft(0L)(_ + _)
    })
  }

  def annularMultiplicitiesIfAvailable = if (rankAtDepth(0) == 1) {
    Some(annularMultiplicities)
  } else {
    None
  }

  def rankAtDepthZeroOption: Option[Int]

  def rankAtDepth(d: Int) = {
    if (d < 0) {
      0
    } else if (d == 0) {
      rankAtDepthZeroOption.getOrElse(inclusions.last.numberOfSources)
    } else if (d == graphDepth) {
      inclusions.head.numberOfTargets
    } else if (d > graphDepth) {
      0
    } else {
      inclusions(graphDepth - d - 1).numberOfSources
    }
  }
  lazy val listOfRanks = (0 to graphDepth).reverse map { rankAtDepth _ }
  lazy val listOfCrossings = inclusions map { _.crossings }

  def depthOfBranchPoint = {
    inclusions.reverse.indexWhere(m => !(m == RectangularMatrix.oneByOneIdentity))
  }

  def truncate = Bigraph(inclusions.tail, Some(inclusions.last.numberOfSources))
  def translate(k: Int = 1): Bigraph = {
    require(k >= 0)
    k match {
      case 0 => this
      case 1 => Bigraph(inclusions ::: List(RectangularMatrix.oneByOneIdentity))
      case k => translate(1).translate(k - 1)
    }
  }

  def permuteAtDepth(k: Int, p: Permutation): Bigraph = {
    val ri = inclusions.reverse

    Bigraph((ri.take(k - 1) ::: ri.lift(k - 1).map(_.permuteRows(p)).toList ::: ri.lift(k).map(_.permuteColumns(p)).toList ::: ri.drop(k + 1)).reverse)
  }

  import scala.math._ // so we can use sqrt and pow

  def FPEigenvalueLowerBounds: Stream[Double] = {
    Bigraph.eigenvalueLowerBoundCounter = Bigraph.eigenvalueLowerBoundCounter + 1

    val neighbours2Cache = {
      for (d <- (0 to graphDepth).toList) yield {
        for (k <- (1 to rankAtDepth(d)).toList) yield {
          neighbours2(d, k)
        }
      }
    }

    def applySquaredAdjacencyMatrix(v: GraphVector): GraphVector = {
      for (d <- 0 to graphDepth by 2) yield {
        for (n2 <- neighbours2Cache(d)) yield {
          (for ((d1, k1) <- n2) yield v(d1 / 2)(k1 - 1)).sum
        }
      }
    }

    val EvenFPEigenvectorEstimates: Stream[(Double, GraphVector)] = Stream.iterate((1.0, currentGuess))({
      case (_, v) => {
        val a = applySquaredAdjacencyMatrix(v)
        val n2 = graphNorm(a)
        val v2 = divideBy(a, n2)
        (sqrt(n2), v2)
      }
    })
    EvenFPEigenvectorEstimates.tail map { _._1 }
  }

  private def graphNorm(v: GraphVector) = sqrt(v.flatten map { pow(_, 2) } reduce { _ + _ })
  private def normalize(v: GraphVector): GraphVector = divideBy(v, graphNorm(v))
  private def divideBy(v: GraphVector, n: Double): GraphVector = {
    v map { r => r map { x => x / n } }
  }

  private var currentGuess: GraphVector = normalize(for (d <- 0 to graphDepth by 2) yield {
    List.fill(rankAtDepth(d))(1.0)
  })

  override def toString(): String = {
    inclusions.reverse.mkString("gbg", "v", "")
  }

  override def toMathematicaInputString() = {
    "GraphFromString[\"" + toString + "\"]"
  }

  def rankZeroExtension: Bigraph
}

object Bigraph {
  var eigenvalueLowerBoundCounter = 0

  def apply(s: String): Bigraph = {
    require(s.startsWith("gbg"))
    if (s.length() == 3) {
      Bigraph(Nil, Some(1))
    } else {
      Bigraph(s.drop(3).split('v').toList.reverse map { m => new RectangularMatrix(m) })
    }
  }

  def apply(g: Bigraph, m: RectangularMatrix): Bigraph = new BigraphImpl(m :: g.inclusions, None)
  def apply(inclusions: List[RectangularMatrix], rankAtDepthZero: Option[Int] = None): Bigraph = new BigraphImpl(inclusions, rankAtDepthZero)

  private class BigraphImpl(override val inclusions: List[RectangularMatrix], val rankAtDepthZeroOption: Option[Int]) extends Bigraph {
    require(inclusions.isEmpty || rankAtDepthZeroOption.isEmpty || inclusions.last.numberOfSources == rankAtDepthZeroOption.get)

    override def equals(x: Any) = {
      x match {
        case x: BigraphImpl => x.inclusions == inclusions
        case _ => false
      }
    }
    override def hashCode = inclusions.hashCode

    override def neighbours(v: GraphVertex) = {
      if (v._1 > graphDepth - 3) {
        neighboursCache(v._1 - graphDepth + 2)(v._2 - 1)
      } else {
        neighbours_(v)
      }
    }
    //		override def neighbours(v: GraphVertex) = neighbours_(v)

    private val neighboursCache = {
      for (d <- ((graphDepth - 2) to graphDepth).toList) yield {
        for (k <- (1 to rankAtDepth(d)).toList) yield {
          neighbours_(d, k)
        }
      }
    }

    private def neighbours_(v: GraphVertex): List[GraphVertex] = {
      if (graphDepth == 0) return List()
      v match {
        case (`graphDepth`, k) => inclusions.head.sources(k) map { (graphDepth - 1, _) }
        case (0, k) => {
          inclusions.last.targets(k) map { (1, _) }
        }
        case (d, k) => (inclusions(graphDepth - d).sources(k) map { (d - 1, _) }) ::: (inclusions(graphDepth - d - 1).targets(k) map { (d + 1, _) })
      }
    }

    //		private lazy val neighbours2Cache = {
    //			for(d <- 0 to graphDepth toList) yield {
    //				for(k <- 1 to rankAtDepth(d) toList) yield {
    //					neighbours2_(d,k)
    //				}
    //			}
    //		}

    //		override def neighbours2(v: GraphVertex) = neighbours2Cache(v._1)(v._2 - 1)
    override def neighbours2(v: GraphVertex) = neighbours2_(v)

    def neighbours2_(v: GraphVertex): List[GraphVertex] = {
      neighbours(v) flatMap { neighbours _ }
    }

    override def rankZeroExtension = {
      Bigraph(this, new RectangularMatrix(rankAtMaximalDepth))
    }
  }

  val isomorphismOrdering: Ordering[Bigraph] = new Ordering[Bigraph] {
    def compare(x: Bigraph, y: Bigraph): Int = {
      import Ordering.Implicits._

      val d0 = x.graphDepth - y.graphDepth
      if (d0 != 0) return d0

      val d1 = implicitly[Ordering[Seq[Int]]].compare(x.listOfRanks, y.listOfRanks)
      if (d1 != 0) return d1

      val d2 = implicitly[Ordering[Seq[Long]]].compare(x.annularMultiplicities, y.annularMultiplicities)
      if (d2 != 0) return d2

      return 0
    }
  }

  val ordering: Ordering[Bigraph] = new Ordering[Bigraph] {
    def compare(x: Bigraph, y: Bigraph): Int = {
      val d1 = isomorphismOrdering.compare(x, y)
      if (d1 != 0) return d1

      import Ordering.Implicits._
      val d2 = implicitly[Ordering[List[Int]]].compare(x.listOfCrossings, y.listOfCrossings)
      if (d2 != 0) return d2

      return 0
    }
  }

  implicit object Odometer extends Odometer[Bigraph] {
    private val odometer: Odometer[RectangularMatrix] = implicitly

    override def reset(o: Bigraph) = {
      val result = Bigraph(odometer.reset(o.inclusions.head) :: o.inclusions.tail)
      result.currentGuess = o.currentGuess
      result
    }
    override def increment(o: Bigraph) = {
      val result = Bigraph(odometer.increment(o.inclusions.head) :: o.inclusions.tail)
      result.currentGuess = o.currentGuess
      result
    }
    override def carry(o: Bigraph) = odometer.carry(o.inclusions.head).map({ m =>
      {
        val result = Bigraph(m :: o.inclusions.tail)
        result.currentGuess = o.currentGuess
        result
      }
    })

  }

}

