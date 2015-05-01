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
          case Some(caller @ Assign(_, Field(obj: Identifier, m @ ("add" | "push" | "addAll" | "extend")))) => graph.definingStatement(obj) match {
            case Some(Assign(_, New(ListType, _))) =>
              val defNode = graph.definingNode(obj).get
              val callNode = graph.definingNode(id).get
              val depth: Int = graph.loops.filter(loop => loop.contains(callNode) && !loop.contains(defNode)).size
              if (depth == 0) Nil
              else if (depth == 1 && (m == "add" || m == "push")) List(MapCallFeature(data.location at caller.pos))
              else List(FlatMapCallFeature(data.location at caller.pos))
            case _ => Nil
          }
          case _ => Nil
        })
      }).flatten

      val functionalFeatures = graph.nodes.flatMap { node =>
        val calls = node.statements.flatMap {
          case Assign(_, Call(id: Identifier, Seq(arg))) => List(id)
          case Mutator(Call(id: Identifier, Seq(arg))) => List(id)
          case _ => Nil
        }

        calls.flatMap(id => graph.definingStatement(id) match {
          case Some(Assign(_, Field(_, "map"))) => List(MapCallFeature(data.location at id.pos))
          case Some(Assign(_, Field(_, "flatMap"))) => List(FlatMapCallFeature(data.location at id.pos))
          case _ => Nil
        })
      }

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

