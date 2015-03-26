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
    val source = new ContentsSource("ClassWithAConstructor.java", """
      class ClassWithAConstructor {
        protected ClassWithAConstructor(int a, String b) throws This, AndThat, AndWhatElse {
        }
      }
    """)

    assert(JavaParser.parse(source) == PackageDef(Names.default, Nil, Nil, List(
      ClassDef(NoModifiers, "ClassWithAConstructor", Nil, Nil, Nil, List(
        ConstructorDef(PROTECTED, "ClassWithAConstructor",
          List("This", "AndThat", "AndWhatElse").map { s =>
            Annotation(Names.THROWS_ANNOTATION, Map(Names.default -> Ident(s)))
          }, Nil, List(
            ValDef(NoModifiers, "a", Nil, PrimitiveTypes.Int, Empty[Expr]),
            ValDef(NoModifiers, "b", Nil, PrimitiveTypes.String, Empty[Expr])
          ),Block(Nil))
      ), ClassSort)
    )))
  }

  it should "work for imports and functions" in {
    val source = new ContentsSource("ParametrizedLambdas.java", """
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
            PrimitiveTypes.Void,Block(List(
              ValDef(NoModifiers,"f1",List(),
                ClassType(NoExpr,"Function",List(),List(ClassType(NoExpr,"Integer",List(),List()), PrimitiveTypes.String)),
                FunctionLiteral(
                  List(ValDef(NoModifiers,"i",List(),ClassType(NoExpr,"Integer",List(),List()),NoExpr,false)), NoType,
                  FunctionCall(FieldAccess(Ident("String"),"valueOf",List()),List(),List(Ident("i")))),false),
              ValDef(NoModifiers,"f2",List(),
                ClassType(NoExpr,"Function",List(),List(ClassType(NoExpr,"Integer",List(),List()), PrimitiveTypes.String)),
                FunctionLiteral(
                  List(ValDef(NoModifiers,"i",List(),NoType,NoExpr,false)), NoType,
                  FunctionCall(FieldAccess(Ident("String"),"valueOf",List()),List(),List(Ident("i")))),false),
              ValDef(NoModifiers,"f3",List(),
                ClassType(NoExpr,"Function",List(),List(ClassType(NoExpr,"Integer",List(),List()), PrimitiveTypes.String)),
                FunctionLiteral(
                  List(ValDef(NoModifiers,"i",List(),NoType,NoExpr,false)), NoType,
                  FunctionCall(FieldAccess(Ident("String"),"valueOf",List()),List(),List(Ident("i")))),false)
              )))),ClassSort))))
  }

  it should "work for switch statements" in {
    val source = new ContentsSource("Test.java", """
      class Test {
        public void toto(int i) {
          switch(i) {
          case 0: System.out.println("1");
          case 1: System.out.println("0");
          case 2: System.out.println("5");
          case 3: System.out.println("3");
          default:
          }
        }
      }
    """)

    assert(JavaParser.parse(source) ==  PackageDef(Names.default,List(),List(),List(
      ClassDef(NoModifiers,"Test",List(),List(),List(),List(
        FunctionDef(PUBLIC,"toto",List(),List(),List(ValDef(NoModifiers,"i",List(),PrimitiveTypes.Int,NoExpr,false)),PrimitiveTypes.Void,Block(List(
          Switch(Ident("i"),List(
            (SimpleLiteral(PrimitiveTypes.Int,"0"),Block(List(
              FunctionCall(FieldAccess(FieldAccess(Ident("System"),"out",List()),"println",List()),List(),List(SimpleLiteral(PrimitiveTypes.String,"1")))))),
            (SimpleLiteral(PrimitiveTypes.Int,"1"),Block(List(
              FunctionCall(FieldAccess(FieldAccess(Ident("System"),"out",List()),"println",List()),List(),List(SimpleLiteral(PrimitiveTypes.String,"0")))))),
            (SimpleLiteral(PrimitiveTypes.Int,"2"),Block(List(
              FunctionCall(FieldAccess(FieldAccess(Ident("System"),"out",List()),"println",List()),List(),List(SimpleLiteral(PrimitiveTypes.String,"5")))))),
            (SimpleLiteral(PrimitiveTypes.Int,"3"),Block(List(
              FunctionCall(FieldAccess(FieldAccess(Ident("System"),"out",List()),"println",List()),List(),List(SimpleLiteral(PrimitiveTypes.String,"3")))))),
            (NoExpr,NoStmt))))))),ClassSort))))
  }
}
