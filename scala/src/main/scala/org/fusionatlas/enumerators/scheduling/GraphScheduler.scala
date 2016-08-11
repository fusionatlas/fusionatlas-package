package org.fusionatlas.enumerators.scheduling

import org.fusionatlas.graphs.PersistentGraph
import org.fusionatlas.enumerators.Classifier
trait GraphScheduler[G <: PersistentGraph[G]] {
  def schedule(h: G)
}