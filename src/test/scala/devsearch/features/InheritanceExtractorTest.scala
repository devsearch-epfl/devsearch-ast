package devsearch.features

import devsearch.parsers.JavaParser
import org.scalatest.{FlatSpec, Matchers}
import spray.json._

class InheritanceExtractorTest extends FlatSpec with Matchers {
  "inheritance extractor" should "extract all class extensions and interface implementations" in {
    val fileURL = getClass.getResource("/samples/JavaConcepts.java")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    val ast = JavaParser.parse(filePath)
    assert(InheritanceExtractor.extractFeatures(ast) == JsonParser( """[
      {"type": "inheritance", "className": "A", "superClassName": "XXX"},
      {"type": "inheritance", "className": "Y", "superClassName": "X"},
      {"type": "inheritance", "className": "XXX", "superClassName": "Serializable"},
      {"type": "inheritance", "className": "JavaConcepts", "superClassName": "Base"},
      {"type": "inheritance", "className": "JavaConcepts", "superClassName": "Serializable"},
      {"type": "inheritance", "className": "XXX", "superClassName": "Cloneable"},
      {"type": "inheritance", "className": "A", "superClassName": "Serializable"},
      {"type": "inheritance", "className": "QWE", "superClassName": "JavaConcepts"}]"""))
  }
}
