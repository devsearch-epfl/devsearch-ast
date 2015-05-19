package devsearch.parsers

import devsearch.ast._

/** Exception parsers will throw when parsing fails. */
case class ParsingFailedError(cause: Throwable) extends Exception("Parsing failed: " + cause.getMessage, cause)

/**
 * Base trait for DevSearch parsers
 *
 * Each parser must specify the language for which is it intended in
 * ```scala
 * def language: String
 * ```
 * Note that this language must be defined in [[Languages.orderedLangList]] with associated
 * meta-data. Furthermore, the meat-and-bones of the parser are defined in
 * ```scala
 * def parse(source: Source): AST
 * ```
 * which will transform a code source into an AST. Note that this function can throw [[ParsingFailedError]]
 * if the parsing fails due to invalid source code.
 */
trait Parser {

  /**
   * The language this parser is intended for
   *
   * This language must be defined in [[Languages.orderedLangList]]!
   */
  def language: String

  /** Actually parsing the source code into an AST */
  def parse(source: Source): AST

  /** Utility method that performs parsing given a file path */
  def parse(fileName: String): AST = {
    parse(new FileSource(fileName))
  }
}
