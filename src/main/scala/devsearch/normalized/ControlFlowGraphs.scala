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
    private var _nodes: List[Node] = List(mkNode, mkNode),
    private var _edges: Set[Edge] = Set()
  ) extends devsearch.normalized.Graph {

    type Node = self.Node
    type Edge = self.Edge

    val firstNode = _nodes.head
    val lastNode = _nodes.last

    def nodes: List[Node] = _nodes
    def newNode: Node = {
      val node = mkNode
      _nodes = _nodes.init :+ node :+ _nodes.last
      node
    }

    private var nodeToNext: Map[Node, Set[Edge]] = _edges.groupBy(_.from).withDefaultValue(Set.empty)
    private var nodeToPrev: Map[Node, Set[Edge]] = _edges.groupBy(_.to).withDefaultValue(Set.empty)

    override def next(node: Node): Set[Edge] = nodeToNext(node)
    override def prev(node: Node): Set[Edge] = nodeToPrev(node)

    private var dirty = true

    def edges: Set[Edge] = _edges
    assert(edges.forall(e => nodes.contains(e.from) && nodes.contains(e.to)))

    def connect(n1: Node, n2: Node, reason: String, guard: Option[(Value, Boolean)] = None): Unit = if (!n1.locked) {
      assert(nodes.contains(n1) && nodes.contains(n2))

      val edge = Edge(n1, guard, n2, reason)
      _edges = _edges + edge
      nodeToNext += n1 -> (nodeToNext(n1) + edge)
      nodeToPrev += n2 -> (nodeToPrev(n2) + edge)
      dirty = true
    }

    @inline
    private def withCache[T](t: => T): T = {
      if (dirty) {
        dirty = false
        transitiveClosure()
        dominatorTree()
      }
      t
    }

    private var _transitiveEdges : Set[(Node, Node)]    = _
    private var _transitiveNext  : Map[Node, Set[Node]] = _
    private var _transitivePrev  : Map[Node, Set[Node]] = _

    private def transitiveClosure(): Unit = {
      _transitiveEdges = edges.map(e => e.from -> e.to)
      _transitiveNext  = _transitiveEdges.groupBy(_._1).mapValues(_.map(_._2)).withDefaultValue(Set.empty)
      _transitivePrev  = _transitiveEdges.groupBy(_._2).mapValues(_.map(_._1)).withDefaultValue(Set.empty)

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

      val accessible = transitiveNext(firstNode) + firstNode
      _nodes = _nodes.filter(accessible)
      _edges = _edges.filter(e => accessible(e.from) && accessible(e.to))

      nodeToNext = nodeToNext.filter(p => accessible(p._1)).mapValues(_.filter(e => accessible(e.to))).withDefaultValue(Set.empty)
      nodeToPrev = nodeToPrev.filter(p => accessible(p._1)).mapValues(_.filter(e => accessible(e.from))).withDefaultValue(Set.empty)

      _transitiveEdges = _transitiveEdges.filter(p => accessible(p._1) && accessible(p._2))
      _transitiveNext  = _transitiveNext.filter(p => accessible(p._1)).withDefaultValue(Set.empty)
      _transitivePrev  = _transitivePrev.filter(p => accessible(p._1)).mapValues(_.filter(accessible)).withDefaultValue(Set.empty)
    }

    override def transitiveNext(node: Node): Set[Node] = withCache(_transitiveNext(node))

    override def transitivePrev(node: Node): Set[Node] = withCache(_transitivePrev(node))

    def transitiveEdge(n1: Node, n2: Node): Boolean = withCache(_transitiveEdges((n1, n2)))

    private var _immediateDominator : Map[Node, Node]      = _
    private var _dominatedChildren  : Map[Node, Set[Node]] = _
    private var _dominanceFrontier  : Map[Node, Set[Node]] = _

    // Lengauer Tarjan algorithm for dominator tree
    private def dominatorTree(): Unit = {
      val (idx, parents) = {
        var idx = Map.empty[Node, Int]
        var counter = 0

        def rec(node: Node): Map[Int, Int] = {
          idx += node -> counter
          counter += 1

          (for (e <- next(node) if !idx.isDefinedAt(e.to)) yield {
            val dfs = rec(e.to) // make sure idx has been completed for e.to
            dfs + (idx(e.to) -> idx(node))
          }).flatten.toMap
        }

        val parents = rec(firstNode)
        (idx, parents)
      }

      val ordered = idx.toList.sortBy(_._2).map(_._1)

      val indexes = List.range(0, nodes.size)
      val init0 = indexes.map(i => i -> -1).toMap
      val initV = indexes.map(i => i -> i).toMap

      var semi     : Map[Int, Int]      = initV
      var idom     : Map[Int, Int]      = init0
      var ancestor : Map[Int, Int]      = init0
      var best     : Map[Int, Int]      = initV
      var bucket   : Map[Int, Set[Int]] = indexes.map(i => i -> Set.empty[Int]).toMap

      def link(v: Int, w: Int): Unit = ancestor += w -> v

      def eval(v: Int): Int = if (ancestor(v) < 0) v else {
        compress(v)
        best(v)
      }

      def compress(v: Int): Unit = {
        val a = ancestor(v)
        if (ancestor(a) >= 0) {
          compress(a)
          if (semi(best(v)) > semi(best(a)))
            best += v -> best(a)
          ancestor += v -> ancestor(a)
        }
      }

      for (nw <- ordered.tail.reverse) {
        val w = idx(nw)
        val p = parents(w)
        for (nv <- prev(nw)) {
          val v = idx(nv.from)
          val u = eval(v)
          if (semi(u) < semi(w))
            semi += w -> semi(u)
        }

        bucket += semi(w) -> (bucket(semi(w)) + w)
        link(p, w)

        for (v <- bucket(p)) {
          val u = eval(v)
          idom += v -> (if (semi(u) < semi(v)) u else p)
        }

        bucket += p -> Set.empty
      }

      for (nw <- ordered.tail) {
        val w = idx(nw)
        if (idom(w) != semi(w))
          idom += w -> idom(idom(w))
      }

      val rev = idx.map(p => p._2 -> p._1)
      _immediateDominator = (idom - 0).map(p => rev(p._1) -> rev(p._2))
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

    def dominator(node: Node): Node = withCache(_immediateDominator(node))

    def dominated(node: Node): Set[Node] = withCache(_dominatedChildren(node))

    def frontier(node: Node): Set[Node] = withCache(_dominanceFrontier(node))
  }

  implicit object graphOps extends GraphOps {
    type Node = self.Node
    type Edge = self.Edge
    type Graph = self.Graph

    def newEdge(edge: Edge, from: Node, to: Node): Edge = Edge(from, edge.guard, to, edge.reason)
    def newGraph(nodes: List[Node], edges: Set[Edge]): Graph = new Graph(nodes, edges)
  }

  case class Edge(from: Node, guard: Option[(Value, Boolean)], to: Node, reason: String) extends GuardedEdge {
    type Node = self.Node
  }
}

