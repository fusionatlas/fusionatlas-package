package org.fusionatlas.graphs.obstructions

import org.fusionatlas.graphs.PairOfBigraphsWithDuals
import org.fusionatlas.enumerators.Classifier
import org.fusionatlas.permutations._
import org.fusionatlas.permutations.Involutions
import org.fusionatlas.graphs.GraphVertex

trait PairObstructions extends Obstructions[PairOfBigraphsWithDuals] {

  val obstructions = List(triplePoint _, paper2 _, paper1QuadruplePoints _, paper1duality _, paper3 _, quadraticTangles522 _, stability _ /* , Haagerup3ST _*/ )

  def Haagerup3ST(p: PairOfBigraphsWithDuals): Option[(String, List[PairOfBigraphsWithDuals], List[(Double, PairOfBigraphsWithDuals)])] = {
    val Haagerup = PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p0x1duals1v1v1x2v2x1", "bwd1v1v1v1p1v1x0p1x0duals1v1v1x2")

    for (g <- List(p.gg0, p.gg1); b = g.bigraph; if (b.toString.startsWith("gbg1v1v1v1p1v1x0p0x1v"))) {
      if (b.neighbours((5, 1)).intersect(b.neighbours((5, 2))).isEmpty) {
        return Some(("Scott's GPA obstruction says that if you're 3ST, look like Haagerup to depth 5, and have no loops by depth 6, then you're Haagerup.",
          List(Haagerup),
          List((d, p.translate(2)))))
      }
    }
    None
  }

  def paper2(p: PairOfBigraphsWithDuals): Option[(String, List[PairOfBigraphsWithDuals], List[(Double, PairOfBigraphsWithDuals)])] = {
    val BadSeed = ("B", PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v0x0x1x0p0x1x0x0v1x0p0x1v0x1p1x0duals1v1v1x2v1x3x2x4v1x2", "bwd1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p0x1v1x0p0x1v1x0p0x1duals1v1v1x2v1x2v2x1"))
    val FSM = ("F", PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0p0x1v1x0p1x0p0x1v0x1x0p0x0x1duals1v1v1x2v1x2v2x1", "bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1p0x1v1x0x0x0p0x0x1x0p0x1x0x0v1x0x0p0x1x0p0x1x0p0x0x1duals1v1v1x2v1x3x2x4v2x1x3x4"))
    val Crab = ("C", PairOfBigraphsWithDuals("bwd1v1v1v1p1v1x0p1x0v1x0v1p1duals1v1v1x2v1", "bwd1v1v1v1p1v1x0p0x1v1x0p1x0p0x1v1x0x0p0x0x1duals1v1v1x2v1x3x2"))

    val depth = p.gg0.bigraph.depthOfBranchPoint
    if (depth < 3 || depth % 2 == 0) {
      None
    } else {
      (for (
        (name, graph) <- List(BadSeed, FSM, Crab) if (p.isIsomorphicTo(graph.translate(depth - 3)))
      ) yield ("The graph " + name + " was ruled out in paper 2", Nil, Nil)).headOption
    }
  }

  def paper1duality(p: PairOfBigraphsWithDuals): Option[(String, List[PairOfBigraphsWithDuals], List[(Double, PairOfBigraphsWithDuals)])] = {
    val depth = p.gg0.bigraph.depthOfBranchPoint
    if (depth % 2 == 0 || depth == -1) {
      None
    } else {
      if ((p.gg0.bigraph.inclusionAtDepth(depth).asArray.flatten ++ p.gg1.bigraph.inclusionAtDepth(depth).asArray.flatten).exists(_ > 1)) {
        None
      } else {
        Involutions.countFixedPoints(p.gg0.involutionAtDepth(depth + 1)) == Involutions.countFixedPoints(p.gg1.involutionAtDepth(depth + 1)) match {
          case true => None
          case false => Some(("The number of self-dual objects immediately past the branch point must match up. (See Lemma 3.6 in Part 1.)", Nil, Nil))
        }
      }
    }
  }

  def paper1QuadruplePoints(p: PairOfBigraphsWithDuals): Option[(String, List[PairOfBigraphsWithDuals], List[(Double, PairOfBigraphsWithDuals)])] = {
    val quadruplePointWeeds = List(
      (Some(2.40486),
        PairOfBigraphsWithDuals("bwd1v1v1v1p1p1v1x0x0p0x1x0v1x0duals1v1v1x2x3v1", "bwd1v1v1v1p1p1v1x0x0p0x1x0v1x0duals1v1v1x2x3v1"),
        List(PairOfBigraphsWithDuals("bwd1v1v1v1v1v1p1p1v1x0x0p0x1x0v1x0duals1v1v1v1x2x3v1", "bwd1v1v1v1v1v1p1p1v1x0x0p0x1x0v1x0duals1v1v1v1x2x3v1")),
        List(),
        "Theorem 6.9 (and noncyclomicity of the index for the next few Beraha numbers)"),
      (None,
        PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0duals1v1v1", "bwd1v1v1p1p1v1x0x0duals1v1v1"),
        List(),
        List(),
        "Theorem 3.7 (Theorem 5.2.2 from Quadratic Tangles)"),
      (Some(2.24687),
        PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0p0x1x0v1x0duals1v1v1x2", "bwd1v1v1p1p1v1x0x0p0x1x0v1x0duals1v1v1x2"),
        List(),
        List((d, PairOfBigraphsWithDuals("bwd1v1v1v1v1p1p1v1x0x0p0x1x0v1x0duals1v1v1v1x2", "bwd1v1v1v1v1p1p1v1x0x0p0x1x0v1x0duals1v1v1v1x2"))),
        "Theorem 6.10"),
      (Some(2.23774),
        PairOfBigraphsWithDuals("bwd1v1v1v1v1p1p1v1x0x0p0x1x0v1x0v1v1v1duals1v1v1v1x2v1v1", "bwd1v1v1v1v1p1p1v1x0x0p0x1x0v1x0v1v1v1duals1v1v1v1x2v1v1"),
        List(PairOfBigraphsWithDuals("bwd1v1v1v1v1v1v1p1p1v1x0x0p0x1x0v1x0v1v1v1duals1v1v1v1v1x2v1v1", "bwd1v1v1v1v1v1v1p1p1v1x0x0p0x1x0v1x0v1v1v1duals1v1v1v1v1x2v1v1")),
        List(),
        "Theorem 6.10 (and a slight improvement of the proof thereof!)"),
      (Some(2.24114),
        PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0p0x1x0v1x0p0x1duals1v1v1x2", "bwd1v1v1p1p1v1x0x0p0x1x0v1x0p0x1duals1v1v1x2"),
        List(PairOfBigraphsWithDuals("bwd1v1v1v1v1p1p1v1x0x0p0x1x0v1x0p0x1duals1v1v1v1x2", "bwd1v1v1v1v1p1p1v1x0x0p0x1x0v1x0p0x1duals1v1v1v1x2")),
        List(),
        "Theorem 6.11"))

    // it's important that you don't translate these graphs!

    (for (
      (cutoff, graph, vines, weeds, reference) <- quadruplePointWeeds if cutoff.isEmpty || d <= cutoff.get if (p.isIsomorphicTo(graph))
    ) yield ("The graph " + graph + " was ruled out in part 1 (" + reference + ")" + (if (cutoff.isEmpty) "" else (" below norm " + cutoff.get)), vines, weeds)).headOption

  }

  def paper3(p: PairOfBigraphsWithDuals): Option[(String, List[PairOfBigraphsWithDuals], List[(Double, PairOfBigraphsWithDuals)])] = {
    val depth = p.gg0.bigraph.depthOfBranchPoint
    if (depth < 0 || depth % 2 == 0) {
      None
    } else {
      val Q = PairOfBigraphsWithDuals("bwd1v1p1p1v1x0x0duals1v1x3x2", "bwd1v1p1p1v1x0x0duals1v1x3x2")
      val Qp = PairOfBigraphsWithDuals("bwd1v1p1p1v1x0x0duals1v1x2x3", "bwd1v1p1p1v1x0x0duals1v1x2x3")
      val g3311 = PairOfBigraphsWithDuals("bwd1v1v1v1p1p1v1x0x0v1duals1v1v1x2x3v1", "bwd1v1v1v1p1p1v1x0x0v1duals1v1v1x2x3v1")
      val weedsWithVines = List((Q, Nil), (Qp, List(g3311)))
      (for (
        (weed, vines) <- weedsWithVines;
        translate = weed.translate(depth - 1);
        if p.isIsomorphicTo(translate)
      ) yield ("The graph " + p + " was ruled out in part 3", vines, Nil)).headOption
    }
  }

  def quadraticTangles522(p: PairOfBigraphsWithDuals): Option[(String, List[PairOfBigraphsWithDuals], List[(Double, PairOfBigraphsWithDuals)])] = {
    val depth = p.gg0.bigraph.depthOfBranchPoint
    if (depth < 0 || depth % 2 == 1) {
      None
    } else {
      val T = PairOfBigraphsWithDuals("bwd1v1v1p1p1v1x0x0duals1v1", "bwd1v1v1p1p1v1x0x0duals1v1")
      if (p.isIsomorphicTo(T.translate(depth - 2))) {
        Some(("The graph " + p + " was ruled out in Theorem 5.2.2 of Quadratic Tangles", Nil, Nil))
      } else {
        None
      }
    }

  }

  def triplePoint(p: PairOfBigraphsWithDuals): Option[(String, List[PairOfBigraphsWithDuals], List[(Double, PairOfBigraphsWithDuals)])] = {
    // we can ignore d; the triple point obstructions always works above 2.  
    if (d <= 2) return None

    for(s <- TriplePointObstruction(p)) yield (s, Nil, Nil)    
  }

  
  def stability(p: PairOfBigraphsWithDuals): Option[(String, List[PairOfBigraphsWithDuals], List[(Double, PairOfBigraphsWithDuals)])] = {

    def rowAndColumnSums(k: Int): List[Int] = {
      val m0 = p.gg0.bigraph.inclusionAtDepth(k).asArray
      val m1 = p.gg1.bigraph.inclusionAtDepth(k).asArray
      for (m <- List(m0, m0.transpose, m1, m1.transpose); r <- m) yield r.sum
    }

    // this means looking between depths k and k+1
    def stableAtDepth(k: Int): Boolean = {
      rowAndColumnSums(k).forall(x => x == 0 || x == 1)
    }

    val stabilities = for (k <- (p.gg0.bigraph.depthOfBranchPoint + 1) to (p.gg0.graphDepth - 1)) yield stableAtDepth(k)

    if (stabilities.containsSlice(Seq(true, false))) {
      Some(("Once principal graphs become stable, they must stay stable." /* + stabilities.toString + (for (k <- (p.gg0.bigraph.depthOfBranchPoint + 1) to (p.gg0.graphDepth - 1)) yield rowAndColumnSums(k))*/ , Nil, Nil))
    } else {
      None
    }
  }

}