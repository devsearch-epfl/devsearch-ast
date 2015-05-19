package devsearch.normalized

import devsearch.ast

/** Edge in the [[ControlFlowGraph]] that extends the base [[Edge]] definition
  * with a guard and bounds the `Node` type to a control-flow [[Node]].
  *
  * The edge guard consists in an option (guard is not required) of a `(Value, Boolean)`
  * pair where the `Value` element encodes the guard and the boolean specifies whether
  * the guard is positive or negated (for else branches).
  */
trait GuardedEdge extends Edge {
  type Node <: devsearch.normalized.Node
  def guard: Option[(Value, Boolean)]
}

/** Node in the [[ControlFlowGraph]].
  *
  * Provides a list of SSA [[Statement]] elements that encode block behavior and
  * defines a `locked: Boolean` member to determine whether the Node can be connected
  * to others in the graph or not (typically nodes get locked once it ends with a
  * [[Throw]] statement).
  */
trait Node {
  def statements: List[Statement]
  def locked: Boolean
}

/** [[Graph]] extension that provides control-flow and SSA helpers.
  *
  * The control-flow graph guarantees we have a control-flow [[Node]]
  * as graph node and provides four kinds of functionalities:
  *  1. Efficient graph properties, namely connectedness and transitive
  *     connectedness in methods `next`, `prev`, `transitiveNext`, `transitivePrev`
  *     and `transitiveEdge`.
  *  2. Dominator-tree computation. The dominator tree and dominance frontier
  *     are computed using the Lengauer-Tarjan algorithm and can be accessed through
  *     the [[dominator]], [[dominated]], [[transitiveDominated]] and [[frontier]]
  *     methods.
  *  3. Let-def sequence computation and access through the methods
  *     [[definingStatement]] and [[definingNode]] for identifiers. Furthermore, we
  *     also provide a [[dependencies]] method that tracks the dependency tree
  *     between local variables.
  *  4. Loop extraction that computes control-flow loops that can later be used
  *     to discover properties about control-flow.
  */
trait ControlFlowGraph { self: Graph =>
  type Node <: devsearch.normalized.Node

  /** Direct dominator of a node. Returns an option to deal with nodes that have
    * no dominator, namely the graph entry-point.
    */
  def dominator(node: Node): Option[Node]

  /** The set of nodes that are directly dominated by the argument node */
  def dominated(node: Node): Set[Node]

  /** The set of all nodes that the argument node dominates (transitive closure) */
  def transitiveDominated(node: Node): Set[Node]

  /** The dominance-frontier of the argument node */
  def frontier(node: Node): Set[Node]

  /** The assignment statement that defines the given local variable if it exists.
    *
    * @see [[Statement]] for more information about assignment
    */
  def definingStatement(id: Identifier): Option[Statement]

  /** The node in which the [[definingStatement]] of the given local variable resides */
  def definingNode(id: Identifier): Option[Node]

  /** The set of variables upon which the given local variable depends */
  def dependencies(id: Identifier): Set[Identifier]

  /** The set of all control-flow loops that can be found in this graph */
  def loops: Set[List[Node]]
}

/** Provides a concrete implementation of a [[ControlFlowGraph]] to all mixins */
trait ControlFlowGraphs { self =>

  type Node <: devsearch.normalized.Node
  def mkNode: Node

  class Graph (
    private var _nodes: List[Node] = List(mkNode, mkNode),
    private var _edges: Set[Edge] = Set()
  ) extends devsearch.normalized.Graph with ControlFlowGraph {

    type Node = self.Node
    type Edge = self.Edge

    override val firstNode = _nodes.head
    override val lastNode = _nodes.last

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
        computeFlow()
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
    override def transitiveEdge(n1: Node, n2: Node): Boolean = withCache(_transitiveEdges((n1, n2)))

    private var _immediateDominator  : Map[Node, Node]      = _
    private var _dominatedChildren   : Map[Node, Set[Node]] = _
    private var _transitiveDominated : Map[Node, Set[Node]] = _
    private var _dominanceFrontier   : Map[Node, Set[Node]] = _

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
      _dominatedChildren  = _immediateDominator.toList.map(p => p._2 -> p._1).groupBy(_._1).mapValues(_.map(_._2).toSet).withDefaultValue(Set.empty)

      var changed = true
      _transitiveDominated = _dominatedChildren
      while(changed) {
        val next = _transitiveDominated.map { case (k,v) => k -> v.flatMap(_transitiveDominated) }.withDefaultValue(Set.empty)
        changed = next != _transitiveDominated
        _transitiveDominated = next
      }

      _dominanceFrontier = Map.empty[Node, Set[Node]].withDefaultValue(Set.empty)
      for (b <- nodes if prev(b).size >= 2; p <- prev(b)) {
        var runner = p.from
        while (runner != _immediateDominator(b)) {
          _dominanceFrontier += runner -> (_dominanceFrontier(runner) + b)
          runner = _immediateDominator(runner)
        }
      }
    }

    def dominator(node: Node): Option[Node] = withCache(_immediateDominator.get(node))

    def dominated(node: Node): Set[Node] = withCache(_dominatedChildren(node))
    def transitiveDominated(node: Node): Set[Node] = withCache(_transitiveDominated(node))

    def frontier(node: Node): Set[Node] = withCache(_dominanceFrontier(node))

    private var _definitionStmt : Map[Identifier, Statement]       = _
    private var _definitionNode : Map[Identifier, Node]            = _
    private var _dependencies   : Map[Identifier, Set[Identifier]] = _
    private var _loops          : Set[List[Node]]                  = _

    private def computeFlow(): Unit = {
      _definitionStmt = Map.empty
      _definitionNode = Map.empty
      _dependencies   = Map.empty.withDefaultValue(Set.empty)

      for (node <- nodes) {
        val stmts = node.statements.flatMap(stmt => stmt match {
          case Assign(id, expr) => List(id -> stmt)
          case MultiAssign(ids, value) => ids.map(_ -> stmt)
          case FieldAssign(obj: Identifier, _, value) => List(obj -> stmt)
          case IndexAssign(arr: Identifier, idx, value) => List(arr -> stmt)
          case _ => Nil
        })

        _definitionStmt ++= stmts
        _definitionNode ++= stmts.map(p => p._1 -> node)
        
        for ((id, stmt) <- stmts;
             d <- stmt match {
               case Assign(_, expr) => expr.dependencies
               case MultiAssign(_, value) => value.dependencies
               case FieldAssign(_, _, value) => value.dependencies
               case IndexAssign(_, idx, value) => idx.dependencies ++ value.dependencies
               case _ => scala.sys.error("How can this be!?")
             }) _dependencies += d -> (_dependencies(d) + id)
      }

      def loops(): Set[List[Node]] = {
        val MAX_LOOP_COUNT = 5
        def rec(chain: List[Node], count: Int): Set[List[Node]] = {
          val first = chain.head
          val last = chain.last

          if (!transitiveEdge(last, first)) {
            Set.empty[List[Node]]
          } else if (first == last && chain.size > 1) {
            Set(chain.init)
          } else if (chain.init.contains(last)) {
            Set.empty[List[Node]]
          } else if (count >= MAX_LOOP_COUNT) {
            Set.empty[List[Node]]
          } else {
            next(last).flatMap(e => rec(chain :+ e.to, count + 1))
          }
        }

        val allChains = nodes.flatMap(n => rec(n :: Nil, 0))

        def filterChains(seen: Set[Node], chains: List[List[Node]], found: List[List[Node]]): Set[List[Node]] = chains match {
          case x :: xs =>
            val newSeen = seen ++ x
            val newChain = found.foldLeft(x)((acc, f) => if (acc.intersect(f).nonEmpty) (acc ++ f).distinct else acc)
            filterChains(newSeen, chains.filterNot(_.forall(newSeen)), found :+ newChain)
          case Nil => found.toSet
        }

        filterChains(Set.empty, allChains.toList.sortBy(_.size), Nil)
      }

      _loops = loops()
    }

    def definingStatement(id: Identifier): Option[Statement] = withCache(_definitionStmt.get(id))
    def definingNode(id: Identifier): Option[Node] = withCache(_definitionNode.get(id))
    def dependencies(id: Identifier): Set[Identifier] = withCache(_dependencies(id))
    def loops = withCache(_loops)
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

