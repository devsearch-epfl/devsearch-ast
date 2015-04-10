package devsearch.features

import devsearch.ast._

abstract class Feature(position: CodeFilePosition) extends java.io.Serializable {
  def key: String
}

trait FeatureExtractor extends java.io.Serializable {
  def extract(data: CodeFileData): TraversableOnce[Feature]
}

object Features extends (CodeFileData => TraversableOnce[Feature]) with java.io.Serializable {

  lazy val extractors = List(
    ImportFeatures,
    InheritanceFeatures,
    ValDefFeatures,
    FunDefFeatures,
    TypeFeatures,
    StructuralFeatures
  )

  def apply(data: CodeFileData) = extractors.flatMap(_.extract(data))
}

case class CodeFileLocation(user: String, repoName: String, fileName: String) extends java.io.Serializable {
  def at(pos: Position) = CodeFilePosition(this, pos.line)
  def at(line: Int) = CodeFilePosition(this, line)
}

case class CodeFilePosition(location: CodeFileLocation, line: Int) extends java.io.Serializable

case class CodeFileData(location: CodeFileLocation, ast: AST) extends java.io.Serializable

