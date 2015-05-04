package devsearch.features

import devsearch.ast._

case class ImportFeature(position: CodePiecePosition, domain: String) extends Feature(position) {
  def key: String = "import=" + domain
}

object ImportExtractor extends FeatureExtractor {
  def extract(data: CodeFile) = data.ast.collect[Feature] {
    case i: Import if i.name != Names.DEFAULT => Set(ImportFeature(data.location at i.pos, i.name))
    case _ => Set.empty
  }
}
