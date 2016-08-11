package org.fusionatlas.enumerators

import org.fusionatlas.graphs.obstructions.Obstructions
import org.fusionatlas.enumerators.scheduling.GraphScheduler
import scala.collection.mutable.ListBuffer
import net.tqft.toolkit.PrintLogging
import org.fusionatlas.graphs.PersistentGraph
import net.tqft.toolkit.ConcurrentTreeBuilder
import net.tqft.toolkit.arithmetic.MinMax
import net.tqft.toolkit.Throttle

abstract class Classifier[G <: PersistentGraph[G]](val d: Double, roots: List[G], val ignoredWeeds: List[G]) extends PrintLogging with GraphScheduler[G] with Obstructions[G] {

  val enumerator: CachingEnumerator[G]

  private val discarded = new ListBuffer[(G, String)]

  def applyObstructionsOnce(vinesAndWeeds: (List[G], List[G])) = {
    val (vines, weeds) = vinesAndWeeds
    val (keep, discard) = weeds map { w => (w, findObstructions(w)) } partition { _._2 == None }
    discarded ++= discard collect { case (g, Some((s, vines, weeds))) => (g, s) }
    val newVines = discard flatMap { _._2.get._2 }
    val newWeeds = discard flatMap { _._2.get._3 } map (_._2) // if the obstructions specify a different norm for one of the new weeds, we completely ignore that here.
    (vines ++ newVines, (keep map (_._1)) ++ newWeeds)
  }

  def applyObstructions(vinesAndWeeds: (List[G], List[G])) = {
    import net.tqft.toolkit.functions.FixedPoint._
    (applyObstructionsOnce _) fixedPoint (vinesAndWeeds)
  }

  val treeBuilder = ConcurrentTreeBuilder[G, List[G]]({ g =>
    enumerator.lookup(d, g) map (applyObstructions _)
  })(roots)

  import MinMax._
  def update(condition: G => Boolean = { g: G => true }) { treeBuilder.update(condition) }
  def updateToRank(k: Int = (weeds() map { _.totalRank }).minOption.getOrElse(0)) = update({ g: G => g.totalRank <= k })
  def updateToDepth(k: Int = (weeds() map { _.graphDepth }).minOption.getOrElse(0)) = update({ g: G => g.graphDepth <= k })
  def updateToFPEigenvalue(e: Double) = update({ g: G => g.isFPEigenvaluePossiblyBelow(e) })

  def completed = treeBuilder.completed()
  
  def weeds(condition: G => Boolean = { g: G => true }) = treeBuilder.incomplete(condition)
  def weedsToRank(k: Int = (weeds() map { _.totalRank }).minOption.getOrElse(0)) = weeds({ g: G => g.totalRank <= k })
  def weedsToDepth(k: Int = (weeds() map { _.graphDepth }).minOption.getOrElse(0)) = weeds({ g: G => g.graphDepth <= k })
  def weedsToFPEigenvalue(e: Double) = weeds({ g: G => g.isFPEigenvaluePossiblyBelow(e) })
  def hay = discarded.toList
  
  private val reportedVines = new ListBuffer[G]
  def newVines = {
    val result = vines().filterNot(reportedVines.contains).toList
    reportedVines ++= result
    result
  }
  
  def delayedToDepth(k: Int) = { treeBuilder.delayed({ g: G => g.graphDepth <= k }) }
  
  def vines(condition: G => Boolean = { g: G => true }) = treeBuilder.data(condition).map(_._2).flatten
  def vinesToRank(k: Int) = vines({ g: G => g.totalRank <= k })
  def vinesToDepth(k: Int) = vines({ g: G => g.graphDepth <= k })
  def vinesToFPEigenvalue(e: Double) = vines({ g: G => g.isFPEigenvaluePossiblyBelow(e) })
  
  def schedulePending(condition: G => Boolean = { g: G => true }) { treeBuilder.delayed(condition) foreach schedule }
  def schedulePendingToRank(k: Int) = schedulePending({ g: G => g.totalRank <= k })
  def schedulePendingToDepth(k: Int) = schedulePending({ g: G => g.graphDepth <= k })
  def schedulePendingToFPEigenvalue(e: Double) = schedulePending({ g: G => g.isFPEigenvaluePossiblyBelow(e) })

}

