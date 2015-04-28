package devsearch.normalized

import devsearch.ast

trait GuardedEdge extends Edge {
  type Node <: devsearch.normalized.Node
  def guard: Option[(Value, Boolean)]
}

trait Node {
  def statements: List[Statement]
  def locked: Boolean
}

trait ControlFlowGraphs { self =>

  type Node <: devsearch.normalized.Node
  def mkNode: Node

  class Graph (
    private var _nodes: List[Node] = List(),
    private var _edges: Set[Edge] = Set()
  ) extends devsearch.normalized.Graph {

    type Node = self.Node
    type Edge = self.Edge

    val firstNode = {
      val node = newNode
      _nodes :+= node
      node
    }

    val lastNode = {
      val node = newNode
      _nodes :+= node
      node
    }

    def nodes: List[Node] = _nodes
    def newNode: Node = {
      val node = mkNode
      _nodes = _nodes.init :+ node :+ _nodes.last
      node
    }

    private var nodeToNext = Map.empty[Node, Set[Edge]].withDefaultValue(Set.empty)
    private var nodeToPrev = Map.empty[Node, Set[Edge]].withDefaultValue(Set.empty)

    override def next(node: Node): Set[Edge] = nodeToNext(node)
    override def prev(node: Node): Set[Edge] = nodeToPrev(node)

    private var dirty = true

    def edges: Set[Edge] = _edges
    def connect(n1: Node, n2: Node, guard: Option[(Value, Boolean)] = None): Unit = if (!n1.locked && !n2.locked) {
      val edge = Edge(n1, guard, n2)
      _edges = _edges + edge
      nodeToNext += n1 -> (nodeToNext(n1) + edge)
      nodeToPrev += n2 -> (nodeToPrev(n2) + edge)
      dirty = true
    }

    private var _transitiveEdges = Set.empty[(Node, Node)]
    private var _transitiveNext  = Map.empty[Node, Set[Node]].withDefaultValue(Set.empty)
    private var _transitivePrev  = Map.empty[Node, Set[Node]].withDefaultValue(Set.empty)

    private def transitiveClosure(): Unit = {
      var changed = true
      while(changed) {
        val newEdges = _transitiveEdges.flatMap {
          case (from, to) => _transitiveNext(to).map((from, _))
        } -- _transitiveEdges

        if (newEdges.nonEmpty) {
          for ((from, to) <- newEdges) {
            _transitiveEdges += (from -> to)
            _transitiveNext  += (from -> (_transitiveNext(from) + to))
            _transitivePrev  += (to   -> (_transitivePrev(to) + from))
          }
        } else {
          changed = false
        }
      }
    }

    private var _immediateDominator = Map.empty[Node, Node]
    private var _dominatedChildren  = Map.empty[Node, Set[Node]]
    private var _dominanceFrontier  = Map.empty[Node, Set[Node]]

    private def dominatorTree(): Unit = {
      val idx = nodes.zipWithIndex.toMap

      val dfsParents = {
        var seen = Set.empty[Node]
        def rec(node: Node): Map[Int, Int] = {
          seen += node
          (for (e <- next(node) if !seen(e.to)) yield {
            rec(e.to) + (idx(e.to) -> idx(node))
          }).flatten.toMap
        }
        rec(firstNode)
      }

      val indexes = List.range(0, nodes.size)
      val init0 = indexes.map(i => i -> -1).toMap
      val initV = indexes.map(i => i -> i).toMap

      var semi:     Map[Int, Int]       = initV
      var idom:     Map[Int, Int]       = init0
      var ancestor: Map[Int, Int]       = init0
      var best:     Map[Int, Int]       = initV
      var bucket:   Map[Int, Set[Int]] = indexes.map(i => i -> Set.empty[Int]).toMap

      def link(v: Int, w: Int): Unit = ancestor += w -> v

      def eval(v: Int): Int = {
        if (ancestor(v) != v) compress(v)
        best(v)
      }

      def compress(v: Int): Unit = {
        val a = ancestor(v)
        if (ancestor(a) >= 0) {
          compress(a)
          if (semi(best(v)) > semi(best(a)))
            best += v -> a
          ancestor += v -> a
        }
      }

      for (nw <- nodes.tail.reverse) {
        val w = idx(nw)
        val p = dfsParents(w)
        for (nv <- prev(nw)) {
          val v = idx(nv.from)
          val u = eval(v)
          if (semi(w) > semi(u))
            semi += w -> semi(u)
        }

        bucket += semi(w) -> (bucket(semi(w)) + w)
        link(p, w)

        for (v <- bucket(p)) {
          val u = eval(v)
          idom += v -> (if (semi(u) < p) u else p)
        }
      }

      for (nw <- nodes.tail) {
        val w = idx(nw)
        if (idom(w) != semi(w))
          idom += w -> idom(idom(w))
      }

      val rev = idx.map(p => p._2 -> p._1)
      _immediateDominator = idom.map(p => rev(p._1) -> rev(p._2))
      _dominatedChildren  = _immediateDominator.toList.map(p => p._2 -> p._1).groupBy(_._1).mapValues(_.map(_._2).toSet)

      _dominanceFrontier = Map.empty[Node, Set[Node]].withDefaultValue(Set.empty)
      for (b <- nodes if prev(b).size >= 2; p <- prev(b)) {
        var runner = p.from
        while (runner != _immediateDominator(b)) {
          _dominanceFrontier += runner -> (_dominanceFrontier(runner) + b)
          runner = _immediateDominator(runner)
        }
      }
    }

    @inline
    private def withCache[T](t: => T): T = {
      if (dirty) {
        transitiveClosure()
        dominatorTree()
        dirty = false
      }
      t
    }

    override def transitiveNext(node: Node): Set[Node] = withCache(_transitiveNext(node))

    override def transitivePrev(node: Node): Set[Node] = withCache(_transitivePrev(node))

    def dominator(node: Node): Node = withCache(_immediateDominator(node))

    def dominated(node: Node): Set[Node] = withCache(_dominatedChildren(node))

    def frontier(node: Node): Set[Node] = withCache(_dominanceFrontier(node))
  }

  implicit object graphOps extends GraphOps {
    type Node = self.Node
    type Edge = self.Edge
    type Graph = self.Graph

    def newEdge(edge: Edge, from: Node, to: Node): Edge = Edge(from, edge.guard, to)
    def newGraph(nodes: List[Node], edges: Set[Edge]): Graph = new Graph(nodes, edges) {
      override val firstNode = nodes.head
      override val lastNode = nodes.last
    }
  }

  case class Edge(from: Node, guard: Option[(Value, Boolean)], to: Node) extends GuardedEdge {
    type Node = self.Node
  }
}

