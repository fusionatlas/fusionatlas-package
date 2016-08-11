package org.fusionatlas.graphs.obstructions
import org.fusionatlas.graphs.PersistentGraph

trait Obstructions[G <: PersistentGraph[G]] {

  /** findObstructions takes a graph, and optionally returns a reason for ruling it out, along with lists of new vines and weeds */
  def findObstructions(f: G): Option[(String, List[G], List[(Double, G)])] = {
    obstructions.foldLeft(onIgnoredList(f))((o, t) => o.orElse(t(f)))
  }

  private def onIgnoredList(g: G): Option[(String, List[G], List[(Double, G)])] = {
    ignoredWeeds.find { weed => g.truncate(weed.graphDepth).isIsomorphicTo(weed) } map { h =>
      ("matches graph on ignore list: " + h, Nil, Nil)
    }
  }

  def ignoredWeeds: List[G]
  def d: Double
  def obstructions: List[G => Option[(String, List[G], List[(Double, G)])]]

}