package devsearch.features

import devsearch.ast._
import devsearch.parsers._

abstract class Feature(position: CodeFilePosition) extends java.io.Serializable {
  def key: String
  override def toString() = key.replace("\\", "\\\\" ).replace(",", "\\,") + "," + position.toString
}

trait FeatureExtractor extends java.io.Serializable {
  def extract(data: CodeFileData): Set[Feature]
}

object Features extends (CodeFileData => TraversableOnce[Feature]) with java.io.Serializable {

  lazy val extractors = List(
    ClassDefFeatures,
    ImportFeatures,
    StructuralFeatures,
    FunDefFeatures,
    TypeFeatures,
    ValDefFeatures
  )

  def apply(data: CodeFileData) = extractors.flatMap(_.extract(data))
}

case class CodeFileLocation(user: String, repoName: String, fileName: String) extends java.io.Serializable {
  def at(pos: Position) = CodeFilePosition(this, pos.line)
  def at(line: Int) = CodeFilePosition(this, line)
}

case class CodeFilePosition(location: CodeFileLocation, line: Int) extends java.io.Serializable{
  override def toString() = location.user+","+location.repoName+","+location.fileName+","+line
}

case class CodeFileData(location: CodeFileLocation, ast: AST) extends java.io.Serializable

object CodeFileData {
  def apply(location: CodeFileLocation, parser: Parser, source: String): CodeFileData =
    CodeFileData(location, scala.util.Try(parser.parse(new ContentsSource(location.fileName, source))) getOrElse Empty[AST])
}

