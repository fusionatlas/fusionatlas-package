package org.fusionatlas.matrices

import net.tqft.toolkit.algebra.enumeration.Odometer
import net.tqft.toolkit.permutations.Permutations._

object SymmetricMatrix {
  implicit object Odometer extends Odometer[SymmetricMatrix] {

    private val listOdometer: Odometer[List[List[Int]]] = implicitly

    override def increment(o: SymmetricMatrix): SymmetricMatrix = {
      new SymmetricMatrix(listOdometer.increment(o.m))
    }
    override def carry(o: SymmetricMatrix): Option[SymmetricMatrix] = {
      listOdometer.carry(o.m).map({ new SymmetricMatrix(_) })
    }
    override def reset(o: SymmetricMatrix): SymmetricMatrix = {
      new SymmetricMatrix(o.size)
    }
  }
}

class SymmetricMatrix(private val m0: List[List[Int]] /* either a square matrix *or* just the lower-triangular entries*/ ) {
  // this constructor give an n*n matrix of zeroes:
  def this(n: Int) = this((1 to n).toList map { k => List.fill(k)(0) })
  // this constructor parses a string:
  def this(m: String) = this(m.split('p').toList map { s => s.split('x').toList.filter(_.nonEmpty) map { c => c.toInt } })

  private val m = if (m0.size <= 1 || m0(0).size == m0(1).size) {
    // we've been given a rectangular matrix; extract just the lower-triangular entries
    for ((row, index) <- m0.zipWithIndex) yield {
      row.take(index + 1)
    }
  } else {
    // make sure we've actually been given a lower-triangular shaped matrix
    for ((row, index) <- m0.zipWithIndex) {
      assert(row.size == index + 1)
    }
    m0
  }

  /* 1-indexed! */
  def neighbours(i: Int): List[Int] = {
    ((m(i - 1).zipWithIndex) flatMap { case (e, k) => List.fill(e)(k + 1) }) :::
      ((m.drop(i).map({ _(i - 1) }).zipWithIndex) flatMap { case (e, k) => List.fill(e)(k + i + 1) })
  }

  def size = m.size

  def permuteRowsAndColumns(q: Permutation) = {
    new SymmetricMatrix(q.permute(asArray map { row => (q permute row).toList }).toList)
  }

  override def toString() = {
    (m map { l => l.mkString("x") }).mkString("p")
  }

  def asArray: List[List[Int]] = m.zipWithIndex map { case (row, index) => row ::: (m.drop(index + 1) map { _(index) }) }
}