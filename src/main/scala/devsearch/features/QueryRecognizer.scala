package devsearch.features

import devsearch.ast.{AST, Empty}
import devsearch.parsers.Languages

/**
 * Parser for language-agnostic code.
 *
 * In some cases, it can be useful to parse some source code even though the
 * source language is unknown. We perform this by successively applying the available
 * parsers (as defined in [[devsearch.parsers.Languages]]) to the code snippet
 * until a parser is able to recognize the snippet.
 *
 * Since we only take the first parse result, it is important to order the languages
 * in a meaningful manner. See [[devsearch.parsers.Languages.orderedLangList]] for
 * more information.
 */
object QueryRecognizer {

  /** Extract a code snippet of unknown language to a code file if possible. */
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
