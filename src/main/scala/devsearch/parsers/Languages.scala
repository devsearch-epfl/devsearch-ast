package devsearch.parsers

import org.apache.commons.io.FilenameUtils

object Languages {
  val Go = "Go"
  val Java = "Java"
  val JavaScript = "JavaScript"
  val Scala = "Scala"

  case class LangSpec(extension: String, parser: Parser)

  val langMap = Map(
    (Go, LangSpec("go", GoParser)),
    (Java, LangSpec("java", JavaParser)),
    (JavaScript, LangSpec("js", JsParser)),
    (Scala, LangSpec("scala", QueryParser))
  )

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
    langMap.map { case (language, langSpec) =>
      (language, langSpec.extension)
    }.get(language)
  }
}
