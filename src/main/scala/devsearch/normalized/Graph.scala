package devsearch.normalized

/**
 * Directed graph edge
 *
 * Conceptually, a directed graph edge is simply a pair of nodes. We also
 * annotate this edge with a `reason` string that can be used both for
 * further graph analysis and display.
 */
trait Edge {
  type Node

  def reason: String
  def from: Node
  def to: Node
}

/**
 * Graph operations
 *
 * Much like the `CanBuildFrom` design pattern in Scala, we use `GraphOps` to
 * provide the graph with the necessary functions for functional graph transformations.
 *
 * To ensure the types make sense before and after transformation, `GraphOps` are
 * type-parametric in `Node`, `Edge` and `Graph`. These types are used in the creation
 * functions:
 * - [[newEdge]] which transforms an edge with two new Nodes
 * - [[newGraph]] which builds a same-typed graph from a set of nodes and edges
 */
trait GraphOps { self =>
  type Node

  type Edge <: devsearch.normalized.Edge {
    type Node = self.Node
  }

  type Graph <: devsearch.normalized.Graph {
    type Node = self.Node
    type Edge = self.Edge
  }

  /**
   * Builds a new `Edge` instance based on the meta-data of the `edge` parameter connecting
   * nodes `from` and `to`
   */
  def newEdge(edge: Edge, from: Node, to: Node): Edge

  /**
   * Builds a new `Graph` instance based on `nodes` and `edges`.
   *
   * Note that all `Edge` instances in `edges` must connect nodes contained in `nodes`!
   */
  def newGraph(nodes: List[Node], edges: Set[Edge]): Graph
}

/**
 * Generic directed graph base-type
 *
 * Provides basic graph functionalities such as
 * - [[nodes]], the set of nodes in this graph
 * - [[edges]], the set of edges in this graph
 * - [[firstNode]] and [[lastNode]], the respective first and last nodes of this graph
 * (we require these to exist in our use case)
 * - [[next]], [[prev]], [[transitiveNext]], [[transitivePrev]] and [[transitiveEdge]]
 * graph connectedness functions
 * - [[map]] that transforms the nodes in the graph
 * - some pretty-printing utilities such as [[printStructure]]
 */
trait Graph { self =>
  type Node
  type Edge <: devsearch.normalized.Edge { type Node = self.Node }

  /** The nodes of this graph */
  def nodes: List[Node]

  /** The edges of this graph */
  def edges: Set[Edge]

  /**
   * The first node of this graph. This node has a path to all other nodes
   *
   * Note that by convention, we use `nodes.head` as first node.
   */
  def firstNode: Node = nodes.head

  /**
   * The last node of this graph. All other nodes have a path to this node
   *
   * Note that by convention, we use `nodes.last` as last node.
   */
  def lastNode: Node = nodes.last

  /** Provides all outgoing edges from `node` */
  def next(node: Node): Set[Edge] = edges.filter(_.from == node)

  /** Provides all incoming edges to `node` */
  def prev(node: Node): Set[Edge] = edges.filter(_.to == node)

  /** Provides the set of reachable nodes from `node` */
  def transitiveNext(node: Node): Set[Node] = {
    devsearch.ast.Operators.fixpoint { nodes: Set[Node] =>
      nodes.flatMap(next(_).map(_.to))
    }(Set(node))
  }

  /** Provides the set of nodes that can reach `node` */
  def transitivePrev(node: Node): Set[Node] = {
    devsearch.ast.Operators.fixpoint { nodes: Set[Node] =>
      nodes.flatMap(prev(_).map(_.from))
    }(Set(node))
  }

  /** Determines whether `n1` has a path to `n2` */
  def transitiveEdge(n1: Node, n2: Node): Boolean = transitiveNext(n1)(n2)

  /**
   * Transformation on the current map that applies `f` to all contained nodes. In the process,
   * the function also updates all edges to point to the new nodes.
   *
   * Note that this function requires an implicit instance of `GraphOps` to build the resulting
   * graph and edges. See [[GraphOps]] for more information.
   */
  def map(f: Node => Node)(implicit ops: GraphOps {
    type Node = self.Node
    type Edge = self.Edge
  }): ops.Graph = {
    val newNodes = nodes.map(f)
    val nodeMapping = (nodes zip newNodes).toMap
    val newEdges = edges.map(e => ops.newEdge(e, nodeMapping(e.from), nodeMapping(e.to)))
    ops.newGraph(newNodes, newEdges)
  }

  /**
   * Pretty-print the graph edges using indices for node identifiers.
   *
   * The following graph:
   * ```
   *      A
   *  r1 / \ r2
   *    B   C
   *  r3 \ / r4
   *      D
   *   r5 |
   *      E
   * ```
   * would result in:
   * ```
   * (1,2,r1)
   * (1,3,r2)
   * (2,4,r3)
   * (3,4,r4)
   * (4,5,r5)
   * ```
   * where r1-r5 are the edge labels, see [[devsearch.normalized.Edge!.reason]].
   */
  def printStructure: String = {
    edges.map(e => (nodes.indexOf(e.from) + 1, nodes.indexOf(e.to) + 1, e.reason)).toList.sorted.mkString("\n")
  }
}
