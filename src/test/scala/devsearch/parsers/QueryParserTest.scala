package devsearch.parsers

import org.scalatest._
import devsearch.ast._
import devsearch.ast.Empty._
import devsearch.ast.Modifiers._

class QueryParserTest extends FlatSpec {

  def testParser(code: String): Unit = {
    val source = new ContentsSource("Test.scala", code)

    // just make sure it doesn't crash
    QueryParser.parse(source)

    // make sure this one crashes!
    assert(try {
      val ast = ScalaParser.parse(source)
      println(ast)
      false
    } catch {
      case t: ParsingFailedError =>
        true
    })
  }

  "Query language" should "work for empty names" in {
    testParser("def _ = 1")
  }

  it should "work for argument holes" in {
    testParser("def toto(_) = 1")
  }

  it should "work for for-loop holes" in {
    testParser("for (i <- List.range(1,2); _) yield i")
  }

  it should "be robust" in {
    val source = new ContentsSource("NoFile", """
      def test(a: Int) = {
        a + 1
    """)

    // just make sure parser doesn't crash
    QueryParser.parse(source)
  }

  it should "be very robust" in {
    val source = new ContentsSource("NoFile", """
      class A {
        def a(i: Int) = {
          i + 1
    """)

    QueryParser.parse(source)
  }

  it should "not loop infinitely on unexpected JavaScript source code" in {
    val source = new ContentsSource("unexpected.js",
      """var i = 10;
        |var j = 10;
        |for (var x = 1; x < 10; x++) {
        |   console.log("wtf");
        |}""".stripMargin)

    assert(try {
      QueryParser.parse(source)
      false
    } catch {
      case _: ParsingFailedError =>
        true
    })
  }
}
