package org.fusionatlas.enumerators

import org.fusionatlas.graphs.PairOfBigraphsWithDuals

object ImmediatePairClassifierApp extends App {
  val Some((vines, weeds)) = new ImmediatePairClassifier(2.23607, Nil).extend(PairOfBigraphsWithDuals("bwd1v1v1v1p1duals1v1v2x1", "bwd1v1v1v1p1duals1v1v2x1"))
  for(w <- weeds) {
    println(w.toString)
  }
}