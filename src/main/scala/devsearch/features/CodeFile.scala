package devsearch.features

import devsearch.ast._
import devsearch.parsers.Languages

/**
 * Location of a complete code file.
 *
 * A CodeFileLocation instance fully specifies the location of a given source code file
 * on github. This location is used to point back to a features source location on the web.
 */
case class CodeFileLocation(user: String, repoName: String, fileName: String) extends java.io.Serializable {
  def at(pos: Position) = CodePiecePosition(this, pos.line)
  def at(line: Int) = CodePiecePosition(this, line)
  override def toString = user + "/" + repoName + "/" + fileName
}

/**
 * Position in a code file
 *
 * Extact line number inside a [[CodeFileLocation]] (we discard column number as line number
 * granularity is sufficient for all our use-cases).
 */
case class CodePiecePosition(location: CodeFileLocation, line: Int) extends java.io.Serializable {
  override def toString = location.toString + ":" + line
}

/**
 * AST wrapper with meta-data
 *
 * Most analysis is performed directly on the source AST, but it can be useful to have a little
 * more information about the considered code such as location and source language.
 *
 * Location is especially important as it enables feature extractors to point back from
 * locally-positioned ASTs to global positions in the cloud (see [[Feature]]).
 */
trait CodeFile {
  /** Source code language, should be contained in [[devsearch.parsers.Languages]]. */
  def language: String

  /** The global location of the source file in the cloud, see [[CodeFileLocation]] */
  def location: CodeFileLocation

  /** Abstract syntax tree representation of source file */
  def ast: AST
}

/** Companion object to [[CodeFile]] that provides some utility constructor/deconstructors. */
object CodeFile {
  private case class CodeFileImpl(language: String, location: CodeFileLocation, ast: AST) extends CodeFile

  def apply(language: String, location: CodeFileLocation, source: Source): CodeFile = {
    val parserOption = Languages.parser(language)
    val ast = parserOption match {
      case Some(parser) =>
        scala.util.Try(
          parser.parse(source)
        ) getOrElse Empty[AST]

      case None => Empty[AST]
    }
    new CodeFileImpl(language, location, ast)
  }

  def apply(language: String, location: CodeFileLocation, sourceText: String): CodeFile = {
    val source = new ContentsSource(location.fileName, sourceText)

    CodeFile(language, location, source)
  }

  def unapply(c: CodeFile) : Option[(String, CodeFileLocation, AST)] = {
    Some((c.language, c.location, c.ast))
  }
}


