package org.fusionatlas.graphs

import net.tqft.toolkit.mathematica.ShortMathematicaExpression

sealed trait Direction
case object Left extends Direction
case object Right extends Direction

case class GraphVertex(G: PairOfBigraphsWithDuals, graph: Int, depth: Int, index: Int) extends Ordered[GraphVertex] with ShortMathematicaExpression {
  require(index > 0)
  def bwd = {
    graph match {
      case 0 => G.gg0
      case 1 => G.gg1
    }
  }
  def dual = {
    if (depth % 2 == 0) {
      GraphVertex(G, graph, depth, (bwd.involutionAtDepth(depth))(index - 1))
    } else {
      GraphVertex(G, 1 - graph, depth, index)
    }
  }
  def rightNeighbours = bwd.bigraph.neighbours(depth, index) map { case (d, i) => GraphVertex(G, graph, d, i) }
  def leftNeighbours = dual.rightNeighbours map { _.dual }

  import net.tqft.toolkit.collections.Tally._
  def rightNeighbourMultiplicities = rightNeighbours.tally
  def leftNeighbourMultiplicities = leftNeighbours.tally

  def neighbours(direction: Direction) = {
    direction match {
      case Right => rightNeighbours
      case Left => leftNeighbours
    }
  }

  def neighbourMultiplicities(direction: Direction): Seq[(GraphVertex, Int)] = {
    (direction match {
      case Right => rightNeighbourMultiplicities
      case Left => leftNeighbourMultiplicities
    }).toSeq
  }
  def neighbourMultiplicities(direction: String): Seq[(GraphVertex, Int)] = {
    neighbourMultiplicities(direction match { case "L" => Left; case "R" => Right })
  }

  def edgesLeaving(direction: Direction) = {
    neighbourMultiplicities(direction) flatMap {
      case (target, multiplicity) => for (index <- 1 to multiplicity) yield GraphEdge(direction, this, target, index)
    }
  }

  def pathsFromStringDirections(over: Traversable[String]): List[GraphPath] = {
    paths(over.toList collect { case "L" => Left; case "R" => Right })
  }
  def paths(over: List[Direction]): List[GraphPath] = {
    def extendPath(path: GraphPath, over: List[Direction]): List[GraphPath] = {
      over match {
        case overHead :: overTail => path.finish.edgesLeaving(overHead).toList map { edge: GraphEdge => GraphPath(path.start, path.edges ::: List(edge)) } flatMap { extendPath(_, overTail) }
        case Nil => List(path)
      }
    }
    extendPath(GraphPath(this, List()), over)
  }
  def cycles(over: List[Direction]): List[GraphCycle] = {
    val paths1 = paths(over.take(over.size / 2)).groupBy(_.finish)
    val paths2 = paths(over.drop(over.size / 2).reverse).groupBy(_.finish)
    for (v <- paths1.keySet.toList; p1 <- paths1(v); p2 <- paths2.get(v).getOrElse(Nil)) yield {
      p1.concatenate(p2.reverse).asCycle
    }
  }

  def compare(other: GraphVertex) = {
    if (graph == other.graph) {
      if (depth == other.depth) {
        if (index == other.index) {
          0
        } else {
          index - other.index
        }
      } else {
        depth - other.depth
      }
    } else {
      graph - other.graph
    }
  }

  def toMathematicaInputString = {
    "Vertex[" + graph + ", " + ((graph + depth) % 2) + ", " + depth + ", " + index + "]"
  }
}

case class GraphEdge(direction: Direction, source: GraphVertex, target: GraphVertex, index: Int) {
  def reverse = GraphEdge(direction, target, source, index)
}

trait AbstractGraphPath[P <: AbstractGraphPath[P]] extends ShortMathematicaExpression {
  def start: GraphVertex
  def edges: List[GraphEdge]
  def finish = if (edges.isEmpty) start else edges.last.target

  def concatenate(p: P): P
  def reverse: P

  def toMathematicaInputString = {
    (start.toMathematicaInputString :: (edges flatMap { edge => List(edge.index.toString, edge.target.toMathematicaInputString) })) mkString ("{", ", ", "}")
  }
}

case class GraphPath(start: GraphVertex, edges: List[GraphEdge]) extends AbstractGraphPath[GraphPath] {
  require(edges.isEmpty || edges.head.source == start)
  def concatenate(p: GraphPath) = {
    require(p.start == finish)
    GraphPath(start, edges ::: p.edges)
  }
  def reverse = {
    GraphPath(finish, edges.reverse.map(e => e.copy(source = e.target, target = e.source)))
  }
  def asCycle = GraphCycle(start, edges)
}

case class GraphCycle(start: GraphVertex, edges: List[GraphEdge]) extends AbstractGraphPath[GraphCycle] {
  require(edges.isEmpty || edges.head.source == start)
  require(start == finish)
  def concatenate(p: GraphCycle) = {
    require(p.start == finish)
    GraphCycle(start, edges ::: p.edges)
  }
  def reverse = {
    GraphCycle(start, edges.reverse.map(e => e.copy(source = e.target, target = e.source)))
  }
}

