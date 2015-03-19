package devsearch.parsers

import org.scalatest._
import devsearch.ast._
import devsearch.ast.Modifiers._
import devsearch.ast.Empty._

class JavaParserTest extends FlatSpec with Matchers {

  "Java source" should "be parsable without exceptions" in {
    val fileURL = getClass.getResource("/samples/JavaConcepts.java")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    JavaParser.parse(filePath)
  }

  it should "work for parsing constructors" in {
    val source = new ContentsSource("""
      class ClassWithAConstructor {
        protected ClassWithAConstructor(int a, String b) throws This, AndThat, AndWhatElse {
        }
      }
    """)

    assert(JavaParser.parse(source) == PackageDef(Names.default, Nil, Nil, List(
      ClassDef(NoModifiers, "ClassWithAConstructor", Nil, Nil, Nil, List(
        ConstructorDef(PROTECTED, "ClassWithAConstructor", Nil, Nil, List(
          ValDef(NoModifiers, "a", Nil, PrimitiveTypes.Int, Empty[Expr]),
          ValDef(NoModifiers, "b", Nil, PrimitiveTypes.String, Empty[Expr])
        ), List("This", "AndThat", "AndWhatElse"), Block(Nil))
      ), false)
    )))
  }

  it should "work for imports and functions" in {
    val source = new ContentsSource("""
      package com.github.javapasrser.bdd.parsing;
      import java.util.function.Function;
      public class ParameterizedLambdas {
        public static void main(String[] args) {
          Function<Integer,String> f1 = (Integer i) -> String.valueOf(i);
          Function<Integer,String> f2 = (i) -> String.valueOf(i);
          Function<Integer,String> f3 = i -> String.valueOf(i);
        }
      }
    """)

    assert(JavaParser.parse(source) == PackageDef("com.github.javapasrser.bdd.parsing",List(),
        List(Import("java.util.function.Function",false,false)),
        List(ClassDef(PUBLIC,"ParameterizedLambdas",List(),List(),List(),
          List(FunctionDef(PUBLIC | STATIC,"main",List(),List(),
            List(ValDef(NoModifiers,"args",List(),ArrayType(PrimitiveTypes.String),NoExpr,false)),
            PrimitiveTypes.Void,List(),Block(List(
              ValDef(NoModifiers,"f1",List(),
                ClassType("Function",NoType,List(),List(ClassType("Integer",NoType,List(),List()), PrimitiveTypes.String)),
                FunctionLiteral(
                  List(ValDef(NoModifiers,"i",List(),ClassType("Integer",NoType,List(),List()),NoExpr,false)),
                  FunctionCall(Ident("String"),"valueOf",List(),List(Ident("i")))),false),
              ValDef(NoModifiers,"f2",List(),
                ClassType("Function",NoType,List(),List(ClassType("Integer",NoType,List(),List()), PrimitiveTypes.String)),
                FunctionLiteral(
                  List(ValDef(NoModifiers,"i",List(),NoType,NoExpr,false)),
                  FunctionCall(Ident("String"),"valueOf",List(),List(Ident("i")))),false),
              ValDef(NoModifiers,"f3",List(),
                ClassType("Function",NoType,List(),List(ClassType("Integer",NoType,List(),List()), PrimitiveTypes.String)),
                FunctionLiteral(
                  List(ValDef(NoModifiers,"i",List(),NoType,NoExpr,false)),
                  FunctionCall(Ident("String"),"valueOf",List(),List(Ident("i")))),false)
              )))),false))))
  }
}
