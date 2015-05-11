package devsearch.features

import devsearch.ast._

case class FieldFeature(position: CodePiecePosition, name: String) extends Feature(position) {
  def key: String = "fieldName=" + name
}

case class FunctionFieldFeature(position: CodePiecePosition, name: String, args: Int) extends Feature(position) {
  def key: String = "functionFieldName=" + name + " args=" + args
}

object FieldExtractor extends FeatureExtractor {

  def extract(data: CodeFile) = data.ast.collect[Feature] {
    case fa @ FieldAccess(_, name, _) if name != Names.DEFAULT =>
      Set(FieldFeature(data.location at fa.pos, name))
    case FunctionCall(fa @ FieldAccess(_, name, _), _, args) if name != Names.DEFAULT =>
      Set(FunctionFieldFeature(data.location at fa.pos, name, args.size))
    case _ => Set.empty
  }
}
