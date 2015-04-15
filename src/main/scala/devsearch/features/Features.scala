package devsearch.features

import devsearch.ast._
import devsearch.parsers._
import scala.util.parsing.combinator._

abstract class Feature(val pos: CodeFilePosition) extends java.io.Serializable {
  def key: String

  def encode = {
    def encode(str: String): String = java.net.URLEncoder.encode(str, "UTF-8").replaceAll("\\+","%20")
    encode(key) + "," + encode(pos.location.toString) + "," + pos.line.toString
  }

  override def equals(that: Any) = that match {
    case f : Feature => key == f.key && pos == f.pos
    case _ => false
  }

  override def hashCode: Int = encode.hashCode
}

object Feature {
  def parse(s: String): Feature = {
    val Array(key, location, line) = s.split(",")
    val (user :: repo :: pathList) = new java.net.URI(location).getPath.split("/").toList
    val fileName = pathList.mkString("/")
    val featureKey = new java.net.URI(key).getPath
    val featurePosition = CodeFilePosition(CodeFileLocation(user, repo, fileName), line.toInt)

    new Feature(featurePosition) {
      def key = featureKey
      override def toString = "Feature(" + key + "," + featurePosition + ")"
    }
  }
}

trait FeatureExtractor extends java.io.Serializable {
  def extract(data: CodeFileData): Set[Feature]
}

object Features extends (CodeFileData => TraversableOnce[Feature]) with java.io.Serializable {

  lazy val extractors = List(
    ClassDefFeatures,
    ComplexFeatures,
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
  override def toString = user + "/" + repoName + "/" + fileName
}

case class CodeFilePosition(location: CodeFileLocation, line: Int) extends java.io.Serializable {
  override def toString = location.toString + ":" + line
}

case class CodeFileData(location: CodeFileLocation, ast: AST) extends java.io.Serializable

object CodeFileData {
  def apply(location: CodeFileLocation, parser: Parser, source: String): CodeFileData =
    CodeFileData(location, scala.util.Try(parser.parse(new ContentsSource(location.fileName, source))) getOrElse Empty[AST])
}

