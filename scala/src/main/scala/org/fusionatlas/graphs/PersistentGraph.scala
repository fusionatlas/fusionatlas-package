package org.fusionatlas.graphs

trait PersistentGraph[G <: PersistentGraph[G]] {
  def graphDepth: Int
  def truncate(k: Int): G
  def totalRank: Int
  def isFPEigenvaluePossiblyBelow(d: Double): Boolean
  def priority: Int = (totalRank - graphDepth) / 10
  def findIsomorphisms(other: G): Iterator[_]
  def isIsomorphicTo(other: G): Boolean = findIsomorphisms(other).nonEmpty
}
