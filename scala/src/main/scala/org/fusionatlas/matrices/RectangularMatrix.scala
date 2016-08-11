package org.fusionatlas.matrices

import net.tqft.toolkit.algebra.enumeration._
import net.tqft.toolkit.permutations.Permutations._

object RectangularMatrix {
  val oneByOneIdentity = new RectangularMatrix(List(List(1)))

  object entireMatrixOdometer extends Odometer[RectangularMatrix] {
//    require(rm.numberOfSources > 0 && rm.numberOfTargets > 0)

    val matrixOdometer: Odometer[List[List[Int]]] = implicitly
    
    def reset(rm: RectangularMatrix) = {
      new RectangularMatrix(matrixOdometer.reset(rm.m))
    }

    def increment(rm: RectangularMatrix) = {
      new RectangularMatrix(matrixOdometer.increment(rm.m))
    }

    def carry(rm: RectangularMatrix) = {
      matrixOdometer.carry(rm.m).map({ p =>
        new RectangularMatrix(p)
      })
    }

  }
  
  implicit object Odometer extends Odometer[RectangularMatrix] {
     
    val rowOdometer: Odometer[List[Int]] = implicitly
   override def reset(rm: RectangularMatrix) = {
    new RectangularMatrix(rowOdometer.reset(rm.m.head) :: rm.m.tail)
  }

  override def increment(rm: RectangularMatrix) = {
    new RectangularMatrix(rowOdometer.increment(rm.m.head) :: rm.m.tail)
  }

  override def carry(rm: RectangularMatrix) = {
    rowOdometer.carry(rm.m.head) map {
      p => new RectangularMatrix(p :: rm.m.tail)
    }
  }
  }

}

class RectangularMatrix(val numberOfSources: Int, private val m: List[List[Int]]) extends java.io.Serializable {
  def this(m: List[List[Int]]) = this(m.head.size, m)
  def this(l: List[Int], a: RectangularMatrix) = this(l.size, l +: a.m)
  // this creates a matrix with zero targets
  def this(numberOfSources: Int) = this(numberOfSources, List())
  def this(numberOfSources: Int, numberOfTargets: Int) = this(numberOfSources, List.fill(numberOfTargets)(List.fill(numberOfSources)(0)))

  def this(m: String) = this(
    if (m(0) == '[') {
      m.drop(2).dropRight(2).split("\\],\\[").toList map { s => s.split(',').toList map { c => c.toInt } }
    } else {
      m.split('p').toList map { s => s.split('x').toList map { c => c.toInt } }
    })

  /*
	 * I don't really care about the implementation in terms of a List[List[Int]]. Feel free to change.
	 */

  val numberOfTargets = m.size

  def targets(source: Int) = targetsCache(source - 1)
  val targetsCache: List[List[Int]] = for (i <- (1 to numberOfSources).toList) yield targets_(i)
  def targets_(source: Int): List[Int] = {
    (m.zipWithIndex) flatMap { case (row, target) => List.fill(row(source - 1))(target + 1) }
  }
  def sources(source: Int) = sourcesCache(source - 1)
  val sourcesCache: List[List[Int]] = for (i <- (1 to numberOfTargets).toList) yield sources_(i)
  def sources_(target: Int): List[Int] = {
    (m(target - 1).zipWithIndex) flatMap { case (m, source) => List.fill(m)(source + 1) }
  }

  def connected_?(): Boolean = {
    for (i <- 1 to numberOfTargets) {
      if (sources(i).isEmpty) return false
    }
    return true
  }

  def permuteColumns(q: Permutation) = {
    new RectangularMatrix(asArray map { row => (q permute row).toList })
  }
  def permuteRows(q: Permutation) = {
    new RectangularMatrix((q permute asArray).toList)
  }

  lazy val crossings = {
    var total = 0
    for (
      s0 <- 0 to (numberOfSources - 1); t0 <- 0 to (numberOfTargets - 1);
      s1 <- 0 to (s0 - 1); t1 <- (t0 + 1) to (numberOfTargets - 1)
    ) {
      total = total + m(t0)(s0) * m(t1)(s1)
    }
    total
  }

  def asArray: List[List[Int]] = m
  def normSquared = m.flatten.foldLeft(0)({ (s, x) => s + x * x })

   //	override def reset() = { m.head.reset :: m.tail }

  override def toString() = {
    (m map { l => l.mkString("x") }).mkString("p")
  }
  def niceToString() = {
    (m map { l => l.mkString("[", ",", "]") }).mkString("[", ",", "]")
  }

  def *(r: RectangularMatrix) = {
    require(r.numberOfTargets == numberOfSources)
    val result = new RectangularMatrix(r.numberOfSources,
      for (row <- m) yield for (j <- (0 to r.numberOfSources - 1).toList) yield {
        ((row zip r.m) map { case (x, rr) => x * rr(j) }).foldLeft(0)(_ + _)
      })
    require(result.numberOfTargets == numberOfTargets)
    result
  }

  def +(r: RectangularMatrix) = {
    require(numberOfSources == r.numberOfSources)
    require(numberOfTargets == r.numberOfTargets)
    new RectangularMatrix(numberOfSources,
      (m zip r.m) map { case (x, y) => (x zip y) map { case (a, b) => a + b } })
  }

  def transpose = {
    if (numberOfTargets == 0) {
      new RectangularMatrix(numberOfTargets)
    } else {
      new RectangularMatrix(numberOfTargets, m.transpose)
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: RectangularMatrix => {
        that.numberOfSources == numberOfSources
        that.m == m
      }
      case _ => false
    }
  }

}



