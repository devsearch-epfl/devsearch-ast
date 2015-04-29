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

  it should "work on AccountDataManager.java" in {
    val fileURL = getClass.getResource("/samples/AccountDataManager.java")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    val ast = JavaParser.parse(filePath)
    Normalizer(ast)
  }

  it should "work on UI.scala" in {
    val fileURL = getClass.getResource("/samples/UI.scala")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    val ast = QueryParser.parse(filePath)
    Normalizer(ast)
  }

  it should "work on event.js" in {
    val fileURL = getClass.getResource("/samples/event.js")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    val ast = JsParser.parse(filePath)
    Normalizer(ast)
  }
}
