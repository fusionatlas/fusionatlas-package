package org.fusionatlas.enumerators.scheduling

import org.fusionatlas.enumerators.Classifier
import org.fusionatlas.graphs.PersistentGraph

trait ImmediateScheduler[G <: PersistentGraph[G]] extends GraphScheduler[G] { this: Classifier[G] =>
  override def schedule(h: G) {
    enumerator.extend(d, h)
  }
}

