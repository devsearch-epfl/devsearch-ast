package devsearch.features

import devsearch.parsers.Languages

object QueryRecognizer {
  def apply(query: String): Option[CodeFile] = {
    Languages.supportedLanguages().toList.map { language =>
      // Some parsers (Go) need a valid file name to parse correctly, so we
      // generate one for the query.
      val generatedFileName = "file" + (Languages.extension(language) match {
        case Some(extension) => "." + extension
        case None => ""
      })

      CodeFile(
        language,
        CodeFileLocation("dummy_user", "dummy_repo", generatedFileName),
        query
      )
    }.map { codeFile =>
      // Scala parser is more permissive, so we reduce its score for the comparison.
      val weight: Double = codeFile.language match {
        case Languages.Scala => 0.7
        case _ => 1.0
      }
      val score: Double = FeatureRecognizer(codeFile).size.toDouble * weight
      (codeFile, score)
    }.sortBy(-_._2).headOption match {
      case Some((codeFile, score)) if score > 0.0 => Some(codeFile)
      case _ => None
    }
  }
}