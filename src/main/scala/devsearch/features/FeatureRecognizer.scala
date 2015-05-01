package devsearch.features

import devsearch.ast._
import devsearch.parsers._

/**
 * Base feature class
 *
 * All features we're going to extract from the ASTs must inherit from this base class. The interface
 * guarantees the presence of a `pos: CodeFilePosition` field that is used to reference the feature
 * source during index lookup. We also require a `key: String` field that is used to actually index
 * upon. This field must identify the feature type in the sense that equality between two features
 * is iff keys are identical.
 *
 * We also provide an [[encode]] method that transforms this feature into a CSV line of the format
 * `key, location, line` where each field is encoded with the URI scheme to make sure we can split
 * on '\n' and ',' during index parsing.
 */
abstract class Feature(val pos: CodePiecePosition) extends java.io.Serializable {
  def key: String

  /**
   * Encodes the current feature into something that can easily be dealt with by
   * a CSV parser. We use the URI encoding scheme to make sure the resulting string
   * contains no commas (outside of the ones we want to split upon) and no line feeds.
   */
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

/** Provides feature parsing for strings generated using [[Feature.encode]]. */
object Feature {
  def parse(s: String): Feature = {
    val Array(key, location, line) = s.split(",")
    val (user :: repo :: pathList) = new java.net.URI(location).getPath.split("/").toList
    val fileName = pathList.mkString("/")
    val featureKey = new java.net.URI(key).getPath
    val featurePosition = CodePiecePosition(CodeFileLocation(user, repo, fileName), line.toInt)

    new Feature(featurePosition) {
      def key = featureKey
      override def toString = "Feature(" + key + "," + featurePosition + ")"
    }
  }
}

/**
 * Base trait for all feature extractors.
 *
 * A feature extractor must provide the [[extract]] method that discovers code patterns in a
 * given code file's associated AST. Each feature must be associated to a location for reverse
 * index lookup, which is why we provide the complete CodeFileData with its location and not
 * just the AST.
 */
trait FeatureExtractor extends java.io.Serializable {

  /** Extract all features of a given type from the given code file */
  def extract(data: CodeFileData): Set[Feature]
}

/**
 * Entry-point for extracting all features in a code file.
 *
 * The [[apply]] method should be invoked by library consumers when extracting all features
 * from a given code file. This procedure is performed in a framework-agnostic way and it is the
 * job of the caller to parallelize over multiple code files.
 */
object FeatureRecognizer extends (CodeFileData => TraversableOnce[Feature]) with java.io.Serializable {

  lazy val extractors = List(
    ClassDefExtractor,
    ComplexExtractor,
    ImportExtractor,
    StructuralExtractor,
    FunDefExtractor,
    TypeExtractor,
    ValDefExtractor
  )

  def apply(data: CodeFileData) = extractors.flatMap(_.extract(data))
}

case class CodeFileLocation(user: String, repoName: String, fileName: String) extends java.io.Serializable {
  def at(pos: Position) = CodePiecePosition(this, pos.line)
  def at(line: Int) = CodePiecePosition(this, line)
  override def toString = user + "/" + repoName + "/" + fileName
}

case class CodePiecePosition(location: CodeFileLocation, line: Int) extends java.io.Serializable {
  override def toString = location.toString + ":" + line
}

case class CodeFileData(size: Long, language: String, location: CodeFileLocation, ast: AST) extends java.io.Serializable

object CodeFileData {
  def apply(size: Long, language: String, location: CodeFileLocation, source: String): CodeFileData = {
    val parserOption = Languages.parser(language)
    val ast = parserOption match {
      case Some(parser) =>
        scala.util.Try(
          parser.parse(new ContentsSource(location.fileName, source))
        ) getOrElse Empty[AST]

      case None => Empty[AST]
    }
    new CodeFileData(size, language, location, ast)
  }
}

