package devsearch.parsers

import org.apache.commons.io.FilenameUtils

object Languages {
  val Go = "Go"
  val Java = "Java"
  val JavaScript = "JavaScript"
  val Scala = "Scala"
  val NotSupported = "NotSupported"

  def isSupported(language: String): Boolean = {
    List(Go, Java, JavaScript, Scala).contains(language)
  }

  def parser(language: String): Parser = language match {
    case Go => GoParser
    case Java => JavaParser
    case JavaScript => JsParser
    case Scala => QueryParser
  }

  def guess(filename: String): String = {
    val extension = FilenameUtils.getExtension(filename)
    extension match {
      case "go" => Go
      case "java" => Java
      case "js" => JavaScript
      case "scala" => Scala
      case _ => NotSupported
    }
  }
}
