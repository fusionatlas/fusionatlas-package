package org.fusionatlas.GPA

trait OperadAlgebra[C, O, A] {
  def colour(a: A): C
  def act(o: O): Seq[A] => A
}

object OperadAlgebra {
  case class PartialEvaluation[O, A](o: O, as: Map[Int, A])

  /**
   * withConstants takes an operad O and an algebra A over it, and returns a triple (O', A', f),
   * where O' is the operad O extended by constants of A, A' is A as an algebra over O', and f promotes elements of A to operations in O'.
   */
  def withConstants[C, O, A](operad: Operad[C, O], algebra: OperadAlgebra[C, O, A]): (Operad[C, PartialEvaluation[O, A]], OperadAlgebra[C, PartialEvaluation[O, A], A], A => PartialEvaluation[O, A]) = {
    val newOperad = new Operad[C, PartialEvaluation[O, A]] {
      override def source(pe: PartialEvaluation[O, A]): Seq[C] = operad.source(pe.o).zipWithIndex.filterNot(p => pe.as.keySet.contains(p._2)).map(_._1)
      override def target(pe: PartialEvaluation[O, A]) = operad.target(pe.o)
      override def identity(c: C) = PartialEvaluation(operad.identity(c), Map())
      override def composeAt(k: Int)(o: PartialEvaluation[O, A], x: PartialEvaluation[O, A]) = {
        PartialEvaluation(operad.composeAt(k)(o.o, x.o), (o.as.map { case (m, a) if m < k => (m, a); case (m, a) if m > k => (m + operad.valence(x.o), a) }) ++ (x.as.map { case (m, a) => (m + k, a) }).toMap)
      }
    }
    val newAlgebra = new OperadAlgebra[C, PartialEvaluation[O, A], A] {
      override def colour(a: A) = algebra.colour(a)
      override def act(o: PartialEvaluation[O, A]) = { as: Seq[A] =>
        val combinedMap = (((0 until operad.valence(o.o)) filterNot o.as.keySet.contains) zip as).toMap ++ o.as
        val arguments = (0 until operad.valence(o.o)).toList.map(combinedMap(_))
        algebra.act(o.o)(arguments)
      }
    }
    def constantAsOperation(a: A) = PartialEvaluation(operad.identity(algebra.colour(a)), Map(0 -> a))
    (newOperad, newAlgebra, constantAsOperation _)
  }
}
