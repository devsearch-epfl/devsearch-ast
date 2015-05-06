package devsearch.parsers

import org.scalatest._
import devsearch.ast._
import devsearch.ast.Empty._
import devsearch.ast.Modifiers._

class QueryParserTest extends FlatSpec {

  "Query language" should "be parsable" in {
    val source = new ContentsSource("Test.scala", """
      def a = 2
      def $_ = 1
      def _ = 1

      def toto(_) = 1

      case class Toto(a: Int, b: Int)
      val t @ Toto(a,b) = Toto(1,2)

      for (i <- List.range(1,2)) yield i

      for (i <- List.range(1,2);
           a = 2;
           _ <- toto;
           e <- List.range(3,4)) yield i + e + a
    """)

    QueryParser.parse(source)
  }

  it should "parse for comprehensions" in {
    val source = new ContentsSource("NoFile", """
      for (i <- List.range(1,2);
           a = 2;
           _ <- toto;
           e <- List.range(3,4)) yield i + e + a
    """)

    assert(QueryParser.parse(source) == Foreach(
      List(ValDef(FINAL,"i",List(),NoType,NoExpr,false)),
      FunctionCall(
        FieldAccess(Ident("List"),"range",List()),List(),List(
          SimpleLiteral(PrimitiveTypes.Int,"1"),
          SimpleLiteral(PrimitiveTypes.Int,"2")
        )),Block(List(
          ValDef(FINAL,"a",List(),NoType,SimpleLiteral(PrimitiveTypes.Int,"2"),false),
          Foreach(
            List(ValDef(FINAL,"_",List(),NoType,NoExpr,false)),
            Ident("toto"),
            Foreach(
              List(ValDef(FINAL,"e",List(),NoType,NoExpr,false)),
              FunctionCall(FieldAccess(Ident("List"),"range",List()),List(),List(
                SimpleLiteral(PrimitiveTypes.Int,"3"),
                SimpleLiteral(PrimitiveTypes.Int,"4")
              )), BinaryOp(BinaryOp(Ident("i"),"+",Ident("e")),"+",Ident("a")),true),true))),true))
  }

  it should "parse varArgs" in {
    val source = new ContentsSource("NoFile", """
      def test(a: String*)
    """)

    assert(QueryParser.parse(source) == FunctionDef(FINAL | ABSTRACT,"test",List(),List(),
      List(ValDef(FINAL,"a",List(),PrimitiveTypes.String,NoExpr,true)),
      PrimitiveTypes.Void, NoStmt))
  }

  it should "parse primitive types" in {
    val source = new ContentsSource("NoFile", """
      val a = 1
      val b: Int = 2
      def toto(f: Int => Int) = f(2)
    """)

    assert(QueryParser.parse(source) == Block(List(
      ValDef(FINAL,"a",List(),NoType,SimpleLiteral(PrimitiveTypes.Int,"1"),false),
      ValDef(FINAL,"b",List(),PrimitiveTypes.Int,SimpleLiteral(PrimitiveTypes.Int,"2"),false),
      FunctionDef(FINAL,"toto",List(),List(),List(
        ValDef(FINAL,"f",List(),FunctionType(List(PrimitiveTypes.Int), PrimitiveTypes.Int), NoExpr,false)
      ),NoType,Block(List(
        FunctionCall(Ident("f"),List(),List(SimpleLiteral(PrimitiveTypes.Int,"2")))))),
      VoidLiteral)))
  }

  it should "parse null literal" in {
    val source = new ContentsSource("NoFile", "val a = null")

    assert(QueryParser.parse(source) == ValDef(FINAL,"a",List(),NoType,NullLiteral,false))
  }

  it should "parse loops" in {
    val source = new ContentsSource("NoFile", """
      while (a < 0) {
        a += 1
        println(a)
      }
    """)

    assert(QueryParser.parse(source) == While(
      BinaryOp(Ident("a"),"<",SimpleLiteral(PrimitiveTypes.Int,"0")),
      Block(List(
        Assign(Ident("a"),SimpleLiteral(PrimitiveTypes.Int,"1"), Some("+")),
        FunctionCall(Ident("println"),List(),List(Ident("a")))))))
  }

  it should "parse expression loops" in {
    val source = new ContentsSource("NoFile", """
      val a = while(b < 0) {
        b += 1
      }
    """)

    assert(QueryParser.parse(source) == ValDef(FINAL,"a",List(),NoType,Block(List(
      While(BinaryOp(Ident("b"),"<",SimpleLiteral(PrimitiveTypes.Int,"0")),
        Block(List(Assign(Ident("b"),SimpleLiteral(PrimitiveTypes.Int,"1"),Some("+"))))),
      VoidLiteral)),false))
  }

  it should "parse do-while statements" in {
    val source = new ContentsSource("NoFile", """
      do {
        println(a.next)
      } while (a.hasNext)
    """)

    assert(QueryParser.parse(source) == Do(FieldAccess(Ident("a"),"hasNext",List()),
      Block(List(FunctionCall(Ident("println"),List(),List(FieldAccess(Ident("a"),"next",List())))))))
  }

  it should "work for named arguments" in {
    val source = new ContentsSource("NoFile", "test(a = 1, b = 2)")

    assert(QueryParser.parse(source) == FunctionCall(Ident("test"),List(),List(
      Assign(Ident("a"),SimpleLiteral(PrimitiveTypes.Int,"1"),None),
      Assign(Ident("b"),SimpleLiteral(PrimitiveTypes.Int,"2"),None))))
  }

  it should "work for annotations" in {
    val source = new ContentsSource("NoFile", "@annot(a = 1, b = 2) val a = 1")

    assert(QueryParser.parse(source) == ValDef(FINAL,"a",List(Annotation("annot",Map(
        "a" -> SimpleLiteral(PrimitiveTypes.Int,"1"),
        "b" -> SimpleLiteral(PrimitiveTypes.Int,"2")
      ))),NoType,SimpleLiteral(PrimitiveTypes.Int,"1"),false))
  }

  it should "be robust" in {
    val source = new ContentsSource("NoFile", """
      def test(a: Int) = {
        a + 1
    """)

    // just make sure parser doesn't crash
    QueryParser.parse(source)
  }

  it should "not loop infinitely on unexpected JavaScript source code" in {
    val mySource = """var i = 10;
                     |var j = 10;
                     |for (var x = 1; x < 10; x++) {
                     |   console.log("wtf");
                     |}""".stripMargin

    assert(QueryParser.parse(new ContentsSource("unexpected.js", mySource)) == Empty[AST])
  }
}
