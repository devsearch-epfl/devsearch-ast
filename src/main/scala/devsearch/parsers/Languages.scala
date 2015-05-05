package devsearch.parsers

import org.apache.commons.io.FilenameUtils

object Languages {
  val Go = "Go"
  val Java = "Java"
  val JavaScript = "JavaScript"
  val Scala = "Scala"
  def supportedLanguages(): Set[String] = Set(Go, Java, JavaScript, Scala)

  def isFileSupported(fileName: String): Boolean = {
    guess(fileName) match {
      case Some(language) => isLanguageSupported(language)
      case None => false
    }
  }

  def isLanguageSupported(language: String): Boolean = {
    parser(language).isDefined
  }

  def parserFromFile(fileName: String): Option[Parser] = {
    guess(fileName) match {
      case Some(language) => parser(language)
      case None => None
    }
  }

  def parser(language: String): Option[Parser] = language match {
    case Go => Some(GoParser)
    case Java => Some(JavaParser)
    case JavaScript => Some(JsParser)
    case Scala => Some(QueryParser)
    case _ => None
  }

  def guess(filename: String): Option[String] = {
    val extension = FilenameUtils.getExtension(filename)
    extension match {
      case "go" => Some(Go)
      case "java" => Some(Java)
      case "js" => Some(JavaScript)
      case "scala" => Some(Scala)
      case _ => None
    }
  }
}
