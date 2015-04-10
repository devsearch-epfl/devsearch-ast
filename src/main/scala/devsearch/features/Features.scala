package devsearch.features

abstract class Feature(position: CodeFilePosition) extends java.io.Serializable {
  def key: String
}

trait FeatureExtractor extends java.io.Serializable {
  def extract(data: CodeFileData): Traversable[Feature]
}
