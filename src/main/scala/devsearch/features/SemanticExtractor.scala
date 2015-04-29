package devsearch.features

import devsearch.ast._
import devsearch.normalized._

case class MapCallFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "map call"
}

case class FlatMapCallFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "flatMap call"
}

object SemanticExtractor extends FeatureExtractor {

  def extract(data: CodeFileData) = {
    val normalized = Normalize(data.ast)

    def loops(graph: Graph) = {
      def rec(seen: Set[Node], chain: List[Node]): Set[List[Node]] = {
        val first = chain.head
        val last = chain.last

        if (!graph.transitiveEdge(first, last)) Set.empty[List[Node]]
        else if (first == last) Set(chain)
        else if (seen(last)) Set.empty[List[Node]]
        else graph.next(last).flatMap(e => rec(seen + last, chain :+ e.to))
      }

      val allChains = graph.nodes.flatMap(n => rec(Set.empty, n :: Nil))

      def filterChains(seen: Set[Node], chains: List[List[Node]]): Set[List[Node]] = chains match {
        case x :: xs =>
          val newSeen = seen ++ x
          filterChains(newSeen, chains.filter(_.forall(seen))) + x
        case Nil => Set.empty[List[Node]]
      }

      filterChains(Set.empty, allChains.toSeq.sortBy(_.size))
    }

    def definitionChains(code: CodeDefinition): Map[Node, Map[Identifier, List[Statement]]] = {
      val graph = code.graph
      var seen = Set.empty[Node]

      def rec(node: Node, definitions: Map[Identifier, List[Statement]]): Map[Node, Map[Identifier, List[Statement]]] = {
        if (seen(node)) Map.empty else {
          seen += node
          val nodeDefs = node.statements.foldLeft(definitions) { case (defs, stmt) =>
            val newDefs = stmt match {
              case Assign(id, expr) => List(id -> expr.dependencies)
              case MultiAssign(ids, value) => ids.map(_ -> value.dependencies)
              case FieldAssign(obj: Identifier, _, value) => List(obj -> value.dependencies)
              case IndexAssign(arr: Identifier, idx, value) => List(arr -> (idx.dependencies ++ value.dependencies))
              case _ => Nil
            }

            val withStmts = newDefs.map { case (id, deps) =>
              id -> (stmt :: deps.toList.flatMap(definitions.getOrElse(_, Nil)))
            }

            // all ids introduced by `newDefs` should not be in defs since we're
            // in SSA form and this is the first time we encounter `node`
            defs ++ withStmts
          }

          Map(node -> definitions) ++ graph.next(node).flatMap(e => rec(e.to, nodeDefs))
        }
      }

      code match {
        case fun: FunctionDefinition =>
          rec(graph.firstNode, fun.params.map {
            case (id, tpe) => id -> Assign(id, New(tpe, Seq.empty))
          }.toMap)
        case _ =>
          rec(graph.firstNode, Map.empty)
      }
    }

    def outPaths(loop: List[Node]) = {
      val nodeSet = loop.toSet
      def rec(seen: Set[Node], node: Node, defs: Map[Identifier, List[Statement]]): Set[(Edge, List[Statement])] = {
        if (seen(node)) Set.empty else {
          
        }

        
      }

    }

    def rec(definition: Definition): Unit = {
      definition match {
        case fun: FunctionDefinition =>
          val graph = fun.graph
          val l = loops(graph)
          l.map { nodes =>
            val nodeSet = nodes.toSet
            val outEdges = nodes.flatMap(n => graph.next(n).filter(e => !nodeSet(e.to)))

            
      simpleChains.flatMap { chain =>
        val dom = chain.find(n => chain.toSet.subsetOf(graph.transitiveDominated(n)))
        dom.map(d => d -> chain.filter(_ != d).toSet)
      }
          
          
        case code: CodeDefinition =>
        case _ =>
      }

      definition.definitions.foreach(rec)
    }

    var features = Set.empty[Feature]
    var idents = Set.empty[String]
    var stack = List.empty[(AST, Set[String])]

    def lookup(name: String): Option[AST] = stack.find { case (ast, ids) => ids(name) }.map(_._1)

    def isListType(ct: ClassType): Boolean = ct match {
      case ClassType(_, "List", _, _) => true
      case ClassType(_, "LinkedList", _, _) => true
      case ClassType(_, "ArrayList", _, _) => true
      case _ => false
    }

    val complexTraverser = new Traverser {
      override def traverse(ast: AST): Unit = ast match {
        case fc @ FunctionCall(Ident("map"), _, List(f)) =>
          features += MapCallFeature(data.location at fc.pos)
          super.traverse(fc)

        case fc @ FunctionCall(Ident("flatMap"), _, List(f)) =>
          features += FlatMapCallFeature(data.location at fc.pos)
          super.traverse(fc)

        case fc @ FunctionCall(FieldAccess(_, "map", _), _, List(f)) =>
          features += MapCallFeature(data.location at fc.pos)
          super.traverse(fc)

        case fc @ FunctionCall(FieldAccess(_, "flatMap", _), _, List(f)) =>
          features += FlatMapCallFeature(data.location at fc.pos)
          super.traverse(fc)

        case fc @ FunctionCall(FieldAccess(Ident(name), "add", _), _, List(elem)) =>
          lookup(name) match {
            case Some(ast) if ast == stack.head._1 => features += MapCallFeature(data.location at ast.pos)
            case Some(ast) => features += FlatMapCallFeature(data.location at ast.pos)
            case _ =>
          }
          super.traverse(fc)

        case fc @ FunctionCall(FieldAccess(Ident(name), "addAll", _), _, List(elem)) =>
          features += FlatMapCallFeature(data.location at ast.pos)
          super.traverse(fc)

        case _ : Foreach | _ : For =>
          val (savedStack, savedIdents) = (stack, idents)
          stack ::= ast -> idents
          idents = Set.empty
          super.traverse(ast)
          stack = savedStack
          idents = savedIdents

        case Assign(Ident(name), ConstructorCall(tpe, _, _), None) if isListType(tpe) =>
          idents += name
          super.traverse(ast)

        case ValDef(_, name, _, _, ConstructorCall(tpe, _, _), _) if isListType(tpe) =>
          idents += name
          super.traverse(ast)

        case _ : Block =>
          val savedIdents = idents
          super.traverse(ast)
          idents = savedIdents

        case _ => super.traverse(ast)
      }
    }

    complexTraverser(data.ast)

    features
  }
}

