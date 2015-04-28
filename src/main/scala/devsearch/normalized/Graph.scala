package devsearch.normalized

trait Edge {
  type Node

  def from: Node
  def to: Node
}

trait GraphOps { self =>
  type Node

  type Edge <: devsearch.normalized.Edge {
    type Node = self.Node
  }

  type Graph <: devsearch.normalized.Graph {
    type Node = self.Node
    type Edge = self.Edge
  }

  def newEdge(edge: Edge, from: Node, to: Node): Edge

  def newGraph(nodes: List[Node], edges: Set[Edge]): Graph
}

trait Graph { self =>
  type Node
  type Edge <: devsearch.normalized.Edge { type Node = self.Node }

  def nodes: List[Node]
  def edges: Set[Edge]

  def next(node: Node): Set[Edge] = edges.filter(_.from == node)
  def prev(node: Node): Set[Edge] = edges.filter(_.to == node)

  def transitiveNext(node: Node): Set[Node] = {
    devsearch.ast.Operators.fixpoint { nodes: Set[Node] =>
      nodes.flatMap(next(_).map(_.to))
    }(Set(node))
  }

  def transitivePrev(node: Node): Set[Node] = {
    devsearch.ast.Operators.fixpoint { nodes: Set[Node] =>
      nodes.flatMap(prev(_).map(_.from))
    }(Set(node))
  }

  def map(f: Node => Node)(implicit ops: GraphOps {
    type Node = self.Node
    type Edge = self.Edge
  }): ops.Graph = {
    val newNodes = nodes.map(f)
    val nodeMapping = (nodes zip newNodes).toMap
    val newEdges = edges.map(e => ops.newEdge(e, nodeMapping(e.from), nodeMapping(e.to)))
    ops.newGraph(newNodes, newEdges)
  }
}
