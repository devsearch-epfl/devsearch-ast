package devsearch.features

import devsearch.normalized._

case class MapCallFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "map call"
}

case class FlatMapCallFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "flatMap call"
}

object SemanticExtractor extends FeatureExtractor {

  def extract(data: CodeFileData) = {
    val normalized = Normalizer(data.ast)

    def extractFeatures(code: CodeDefinition) = {
      val graph = code.graph

      val imperativeFeatures = (for (loop <- graph.loops; node <- loop) yield {
        val calls = node.statements.flatMap {
          case Assign(_, Call(id: Identifier, _)) => List(id)
          case Mutator(Call(id: Identifier, _)) => List(id)
          case _ => Nil
        }

        calls.flatMap(id => graph.definingStatement(id) match {
          case caller @ Assign(_, Field(obj: Identifier, "add" | "push")) => graph.definingStatement(obj) match {
            case Assign(_, New(ListType, _)) =>
              val defNode = graph.definingNode(obj)
              val depth = graph.loops.filter(loop => loop.contains(defNode))
              if (depth == 1) List(MapCallFeature(data.location at caller.pos))
              else List(FlatMapCallFeature(data.location at caller.pos))
            case _ => Nil
          }
          case _ => Nil
        })
      }).flatten

      val functionalFeatures = graph.nodes.flatMap(node => node.statements.flatMap {
        case Assign(_, call @ Call(id: Identifier, Seq(arg))) => graph.definingStatement(id) match {
          case Assign(_, Field(_, "map")) => List(MapCallFeature(data.location at call.pos))
          case Assign(_, Field(_, "flatMap")) => List(FlatMapCallFeature(data.location at call.pos))
          case _ => Nil
        }
        case _ => Nil
      })

      imperativeFeatures ++ functionalFeatures
    }

    def rec(definition: Definition): Set[Feature] = {
      val defFeatures = definition match {
        case code: CodeDefinition => extractFeatures(code)
        case _ => Set.empty
      }

      defFeatures ++ definition.definitions.flatMap(rec)
    }

    rec(normalized)
  }
}

