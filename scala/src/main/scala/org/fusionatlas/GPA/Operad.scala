package org.fusionatlas.GPA

trait Operad[C, O] {
  def source(o: O): Seq[C]
  def target(o: O): C
  def identity(c: C): O
  def valence(o: O) = source(o).size
  def compose(o: O, os: List[O]): O = {
    (os zip (os.map(valence(_)).foldLeft(List(0))({
      (l: List[Int], k: Int) => l.head + k :: l
    })).reverse).foldLeft(o)({
      (o1: O, p: (O, Int)) => composeAt(p._2)(o1, p._1)
    })
  }
  def compose(o: O, os: O*): O = compose(o, os.toList)
  def composeAt(k: Int)(o: O, x: O): O
}
