package devsearch.parsers

import devsearch.ast._

case class ParsingFailedError(cause: Throwable) extends Exception("Parsing failed: " + cause.getMessage, cause)

trait Parser {
  def language: String
  def parse(source: Source): AST

  def parse(fileName: String): AST = {
    parse(new FileSource(fileName))
  }
}
