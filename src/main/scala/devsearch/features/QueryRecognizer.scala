package devsearch.features

import devsearch.ast.ContentsSource
import devsearch.parsers.Languages

object QueryRecognizer {
  def findCodeFile(query: String): Option[CodeFile] = {
    scala.util.Try {
      Languages.supportedLanguages().toList.map { language =>
        val extensionWithDot = Languages.extension(language) match {
          case Some(extension) => "." + extension
          case None => ""
        }

        CodeFile(
          language,
          CodeFileLocation("dummy", "dummy", "dummy"),
          new ContentsSource("unique_filename" + extensionWithDot, query)
        )
      }.map { codeFile =>
        // Since Scala parser is more permissive, we reduce its score for the comparison
        val weight: Double = codeFile.language match {
          case Languages.Scala => 0.7
          case _ => 1.0
        }
        val score: Double = FeatureRecognizer(codeFile).size.toDouble * weight
        (codeFile, score)
      }.sortBy(_._2)
      .reverse.head match {
        case (codeFile, score) if score > 0.0 => Some(codeFile)
        case _ => None
      }
      // head can throw NoSuchElementException
    }.getOrElse(None)
  }
}