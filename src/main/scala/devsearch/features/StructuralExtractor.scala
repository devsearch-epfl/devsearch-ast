package devsearch.features

import devsearch.ast._

case class ControlFeature(position: CodePiecePosition, ctrl: String) extends Feature(position) {
  def key = "controlStatement=" + ctrl
}

object StructuralExtractor extends FeatureExtractor {
  def extract(data: CodeFileData) = data.ast.collect[Feature] {
    case i : If => Set(ControlFeature(data.location at i.pos, "if"))
    case f : Foreach => Set(ControlFeature(data.location at f.pos, "foreach"))
    case f : For => Set(ControlFeature(data.location at f.pos, "for"))
    case w : While => Set(ControlFeature(data.location at w.pos, "while"))
    case d : Do => Set(ControlFeature(data.location at d.pos, "do"))
    case _ => Set.empty
  }
}
