package devsearch.features

import devsearch.ast._
import devsearch.parsers.Languages

case class CodeFileLocation(user: String, repoName: String, fileName: String) extends java.io.Serializable {
  def at(pos: Position) = CodePiecePosition(this, pos.line)
  def at(line: Int) = CodePiecePosition(this, line)
  override def toString = user + "/" + repoName + "/" + fileName
}

case class CodePiecePosition(location: CodeFileLocation, line: Int) extends java.io.Serializable {
  override def toString = location.toString + ":" + line
}

trait CodeFile {
  def language: String
  def location: CodeFileLocation
  def ast: AST
}

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


