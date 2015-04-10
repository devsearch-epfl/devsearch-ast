package devsearch.features

import devsearch.ast._

case class IfStatementFeature(position: CodeFilePosition) extends Feature(position) {
  def key = "if statement"
}

case class ForeachFeature(position: CodeFilePosition) extends Feature(position) {
  def key = "foreach loop"
}

object StructuralFeatures extends FeatureExtractor {
  def extract(data: CodeFileData) = data.ast.collect[Feature] {
    case i : If => Set(IfStatementFeature(data.location at i.pos))
    case f : Foreach => Set(ForeachFeature(data.location at f.pos))
    case _ => Set.empty
  }
}
