package devsearch.features

import devsearch.ast._

case class ImportFeature(position: CodeFilePosition, domain: String) extends Feature(position) {
  def key: String = "domain = " + domain
}

object ImportFeatures extends FeatureExtractor {
  def extract(data: CodeFileData): Traversable[Feature] = data.ast.collect {
    case i: Import if i.name != Names.DEFAULT => Set(ImportFeature(data.location at i.pos, i.name))
    case _ => Set.empty[ImportFeature]
  }
}
