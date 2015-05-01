package devsearch.parsers

import org.apache.commons.io.FilenameUtils

object Languages {
  val Go = "Go"
  val Java = "Java"
  val JavaScript = "JavaScript"
  val Scala = "Scala"
  val NotSupported = "NotSupported"

  def isSupported(language: String): Boolean = {
    parser(language).isDefined
  }

  def parser(language: String): Option[Parser] = language match {
    case Go => Some(GoParser)
    case Java => Some(JavaParser)
    case JavaScript => Some(JsParser)
    case Scala => Some(QueryParser)
    case _ => None
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
