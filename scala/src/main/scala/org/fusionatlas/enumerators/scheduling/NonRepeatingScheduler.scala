package org.fusionatlas.enumerators.scheduling

import org.fusionatlas.enumerators.Classifier
import org.fusionatlas.graphs.PersistentGraph
import scala.collection.JavaConversions

trait NonRepeatingScheduler[G <: PersistentGraph[G]] extends GraphScheduler[G] {
  import JavaConversions._
  val scheduled: scala.collection.mutable.Map[G, Unit] = new com.google.common.collect.MapMaker().makeMap[G, Unit]
  def propose(h: G) = {
    if (scheduled.containsKey(h)) {
      false
    } else {
      scheduled += (h -> ())
      true
    }
  }
  def complete(h: G) = scheduled -= h
}


