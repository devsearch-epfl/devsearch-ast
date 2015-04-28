package devsearch.normalized

import devsearch.parsers._
import org.scalatest._

class NormalizationTest extends FlatSpec {

  "Normalization" should "work on JavaConcepts.java" in {
    val fileURL = getClass.getResource("/samples/JavaConcepts.java")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    val ast = JavaParser.parse(filePath)
    Normalizer(ast)
  }
}
