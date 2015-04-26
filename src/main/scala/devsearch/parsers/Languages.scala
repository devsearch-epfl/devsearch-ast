package devsearch.parsers

object Languages {
  val Go = "Go"
  val Java = "Java"
  val JavaScript = "JavaScript"
  val Scala = "Scala"

  def extension(language: String): String = language match {
    case Scala => "scala"
    case Go => "go"
    case Java => "java"
    case JavaScript => "js"
    case _ => "unknown"
  }
}
