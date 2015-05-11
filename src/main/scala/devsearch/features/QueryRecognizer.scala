package devsearch.features

import devsearch.ast.{AST, Empty}
import devsearch.parsers.Languages

object QueryRecognizer {
  def apply(query: String): Option[CodeFile] = {
    Languages.orderedSupportedLanguages().view.map {
      generateCodeFile(_, query)
    }.collectFirst {
      case codeFile if codeFile.ast != Empty[AST] => codeFile
    }
  }

  private def generateCodeFile(language: String, query: String): CodeFile = {
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
  }
}
