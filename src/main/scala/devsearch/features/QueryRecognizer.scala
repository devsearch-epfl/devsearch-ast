package devsearch.features

import devsearch.ast.ContentsSource
import devsearch.parsers.Languages

object QueryRecognizer {
  def findCodeFile(query: String): Option[CodeFile] = {
    scala.util.Try {
      Some(
        Languages.supportedLanguages().toList.map { language =>
          CodeFile(
            language,
            CodeFileLocation("dummy", "dummy", "dummy"),
            new ContentsSource("unique_filename", query)
          )
        }.map { codeFile =>
          // Since Scala parser is more permissive, we reduce its score for the comparison
          val weight = codeFile.language match {
            case Languages.Scala => 0.7
            case _ => 1.0
          }
          val score = FeatureRecognizer(codeFile).size * weight
          (codeFile, score)
        }.sortBy(_._2)
        .reverse.head._1
        // head can throw NoSuchElementException
      )
    }.getOrElse(None)
  }
}