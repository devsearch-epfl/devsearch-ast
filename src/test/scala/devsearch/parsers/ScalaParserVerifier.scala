package devsearch.parsers

import devsearch.ast.Empty.NoDef
import org.scalatest.FlatSpec

class ScalaParserVerifier extends FlatSpec {
  "Parser" should "succeed with UI.scala" in {
    val fileURL = getClass.getResource("/samples/UI.scala")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    assert(QueryParser.parse(filePath) != NoDef)
  }
}