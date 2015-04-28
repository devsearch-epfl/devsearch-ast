package devsearch.normalized

class StaticRenamer extends Extraction {

  class StaticRenamingTransformer {
    var namer: Namer = _

    def apply(definition: Definition): Definition = {
      val newDefinitions = definition.definitions.map(apply)

      definition match {
        case code: CodeDefinition =>
          val graph = code.graph

          var orig = Map.empty[Node, Set[Identifier]].withDefaultValue(Set.empty)
          for (n <- graph.nodes; stmt <- n.statements) {
            stmt match {
              case Assign(id, _) => orig += n -> (orig(n) + id)
              case MultiAssign(ids, _) => orig += n -> (orig(n) ++ ids)
              case _ =>
            }
          }

          var defsites = Map.empty[Identifier, Set[Node]].withDefaultValue(Set.empty)
          for (n <- graph.nodes; id <- orig(n)) {
            defsites += id -> (defsites(id) + n)
          }

          var defphis = Map.empty[Node, Set[Identifier]].withDefaultValue(Set.empty)
          for ((id, sites) <- defsites) {
            var w = sites
            while (w.nonEmpty) {
              val n = w.head
              w -= n

              for (y <- graph.frontier(n) if !defphis(y)(id)) {
                defphis += y -> (defphis(y) + id)

                if (!orig(y)(id)) w += y
              }
            }
          }

          var nodeStmts: Map[Node, List[Statement]] = Map.empty
          var nodePhis:  Map[Node, Map[String, Map[Node, String]]] = defphis.map { case (node, ids) =>
            node -> ids.map(id => id.name -> graph.prev(node).map(n => n.from -> id.name).toMap).toMap
          }

          def renameNode(node: Node, prevMapping: Map[String,String]): Unit = {
            var mapping = prevMapping
            def mapped(map: Map[String,String]): Identifier => Identifier = {
              id => Identifier(map.getOrElse(id.name, id.name))
            }

            nodeStmts += node -> (for (stmt <- node.statements) yield {
              val freshIds = Map.empty[String,String] ++ (stmt match {
                case Assign(id, _) => List(id.name -> namer.fresh(id.name))
                case MultiAssign(ids, _) => ids.map(id => id.name -> namer.fresh(id.name))
                case _ => Nil
              })

              // first we rename the arguments given previous mappings...
              val renamedArgs = stmt rename mapped(mapping)

              // ... and then we freshen the assignment ids ...
              val freshStmt = stmt rename mapped(mapping ++ freshIds)

              // ... before making sure the new ids are propagated to future statements!
              mapping ++= freshIds

              freshStmt
            })

            // TODO: freshen phi names too!!
            nodePhis ++= graph.next(node).map(e => e.to).map(n => n -> {
              nodePhis(n).map { case (name, phis) =>
                name -> (phis + (node -> mapping.getOrElse(name, name)))
              }
            })

            graph.dominated(node).foreach(renameNode(_, mapping))
          }

          val phiGraph = graph.map { node =>
            val stmts = defphis(node).map(id => Assign(id, Phi(graph.prev(node).toList.map(_ => id))))
            node.withStatements(stmts.toList ++ node.statements)
          }

          

          code match {
            case fd: FunctionDefinition => new FunctionDefinition(definition.name, fd.params) {
              override val graph = phiGraph
              override val definitions: List[Definition] = newDefinitions
            }
            case _ => new CodeDefinition(definition.name) {
              override val graph = phiGraph
              override val definitions: List[Definition] = newDefinitions
            }
          }

        case _ => new Definition(definition.name) {
          override val definitions: List[Definition] = newDefinitions
        }
      }
    }
  }


}
