package org.fusionatlas.enumerators
import net.tqft.toolkit.PrintLogging
import org.fusionatlas.graphs.obstructions.Obstructions
import org.fusionatlas.graphs.obstructions.PairObstructions
import org.fusionatlas.graphs.PersistentGraph
import org.fusionatlas.enumerators.queues.JobQueue
import scala.collection.mutable.ListBuffer
import org.fusionatlas.graphs.PairOfBigraphsWithDuals

abstract class Classifier2[G <: PersistentGraph[G]](override val d: Double, override val ignoredWeeds: List[G]) extends PrintLogging with Obstructions[G] {

  protected def enumerator: CachingEnumerator[G]
  
// ignored weeds are meant to be handled by Obstructions
//  def removeIgnoredWeeds(graphs: List[G]): List[G] = graphs.filter(graph => ignoredWeeds.find(ignored => ignored.isIsomorphicTo(graph.truncate(ignored.graphDepth))).isEmpty)
//  def removeIgnoredWeeds(vinesAndWeeds: (List[G], List[G])): (List[G], List[G]) = (vinesAndWeeds._1, removeIgnoredWeeds(vinesAndWeeds._2))
  
  def extend(g: G) = enumerator.lookup(d, g)
  def obstruct(g: G) = findObstructions(g)
  def enqueue(g: G)
}

trait JobQueueClassifier[G <: PersistentGraph[G]] { self: Classifier2[G] =>
  private val queue = JobQueue()
  def enqueue(g: G) = queue.enqueue((d, g))

}

class PairClassifier2(d: Double, ignoredWeeds: List[PairOfBigraphsWithDuals]) extends Classifier2[PairOfBigraphsWithDuals](d, ignoredWeeds) with JobQueueClassifier[PairOfBigraphsWithDuals] with PairObstructions {
  override val enumerator = DatabasePairEnumerator
}

class ImmediatePairClassifier(d: Double, ignoredWeeds: List[PairOfBigraphsWithDuals]) extends Classifier2[PairOfBigraphsWithDuals](d, ignoredWeeds) with PairObstructions {
  override val enumerator = DatabasePairEnumerator
  override def extend(g: PairOfBigraphsWithDuals) = Some(enumerator.extend(d, g))
  override def enqueue(g: PairOfBigraphsWithDuals) { }
}

class GlobalDimensionLimitClassifier(d: Double, maximumGlobalDimensionIncrease: Double, ignoredWeeds: List[PairOfBigraphsWithDuals]) extends ImmediatePairClassifier(d, ignoredWeeds) {
  override def extend(g: PairOfBigraphsWithDuals) = Some(enumerator.extend(d, g, Some(maximumGlobalDimensionIncrease)))
}