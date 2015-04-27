package devsearch.parsers

import devsearch.ast.Empty.NoDef
import org.scalatest.FlatSpec

class JsParserVerifier extends FlatSpec {
  "Parser" should "succeed with event.js" in {
    val fileURL = getClass.getResource("/samples/event.js")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    assert(JsParser.parse(filePath) != NoDef)
  }
}
