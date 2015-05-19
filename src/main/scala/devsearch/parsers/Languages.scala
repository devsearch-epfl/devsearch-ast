package devsearch.parsers

import org.apache.commons.io.FilenameUtils

/** Centralized language handling for parsers.
  *
  * Provides utilities to access mappings between language names, file extensions
  * and parsers. Also provides the full list of supported languages.
  *
  * For guessing the language of a code snippet, see [[orderedLangList]].
  */
object Languages {
  val Go = "Go"
  val Java = "Java"
  val JavaScript = "JavaScript"
  val Scala = "Scala"
  val QueryLang = "QueryLang"

  case class LangSpec(extension: String, parser: Parser)

  /** Ordered list of languages and their specifications.
    *
    * This contains the comprehensive list of languages supported by the project
    * along with the associated file extension and parser. All other helper functions
    * in this object are based on this list.
    *
    * The list of languages is ordered and the given order is used when parsing code
    * snippets for which the language is unknown. Parsing these language-agnostic
    * snippets is performed using fallback by trying the parsers in order and returning
    * the first successful parse result. See [[features.QueryRecognizer]] for implementation.
    */
  val orderedLangList = List(
    (Go, LangSpec("go", GoParser)),
    (JavaScript, LangSpec("js", JsParser)),
    (Java, LangSpec("java", JavaParser)),
    (Scala, LangSpec("scala", ScalaParser)),
    (QueryLang, LangSpec("query", QueryParser))
  )

  val langMap = orderedLangList.toMap

  def orderedSupportedLanguages(): List[String] = {
    orderedLangList.map(_._1)
  }

  def supportedLanguages(): Set[String] = langMap.keySet

  def isFileSupported(fileName: String): Boolean = {
    guess(fileName) match {
      case Some(language) => isLanguageSupported(language)
      case None => false
    }
  }

  def isLanguageSupported(language: String): Boolean = {
    langMap.contains(language)
  }

  def parserFromFile(fileName: String): Option[Parser] = {
    guess(fileName) match {
      case Some(language) => parser(language)
      case None => None
    }
  }

  def parser(language: String): Option[Parser] = {
    langMap.get(language) match {
      case Some(langSpec) => Some(langSpec.parser)
      case _ => None
    }
  }

  def guess(filename: String): Option[String] = {
    val extension = FilenameUtils.getExtension(filename)
    langMap.map(_.swap).map { case (langSpec, language) =>
      (langSpec.extension, language)
    }.get(extension)
  }

  def extension(language: String): Option[String] = {
    langMap.map { case (lang, langSpec) =>
      (lang, langSpec.extension)
    }.get(language)
  }
}
