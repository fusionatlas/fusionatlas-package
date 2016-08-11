package org.fusionatlas.enumerators

import net.tqft.toolkit.Logging

trait Enumerator[G] {
  def extend(d: Double, g: G, maximumGlobalDimensionIncrease: Option[Double] = None): (List[G], List[G])
}
