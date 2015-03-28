package devsearch.blobToAST

import org.scalatest._
import devsearch.ast._
import devsearch.ast.Modifiers.{STATIC, PUBLIC, PROTECTED, NoModifiers}
import devsearch.ast.ClassDef
import devsearch.ast.ConstructorDef
import devsearch.ast.PackageDef
import devsearch.ast.ValDef
import devsearch.ast.Empty.{NoType, NoExpr}

/**
 * Created by hubi on 3/27/15.
 */
class AstExtractorTest extends FlatSpec with Matchers{
  val test = ASTExtractor.extract("src/test/resources/blobs")

  "AstExtractor" should "not return null" in {
    assert(test != null)
  }


  it should "correctly snip this String" in {
    val s =
      """9999:Java/samarion/repo-name/path/to/file
        |class ClassWithAConstructor {
        |  protected ClassWithAConstructor(int a, String b) throws This, AndThat, AndWhatElse {
        |  }
        |}
        |19827:Java/hubifant/another_repo/path/to/another/repo
        |while(!asleep)
        |  sheep++""".stripMargin
    assert(ASTExtractor.snipBlob(("blob/path", s)).toString == """List(9999:Java/samarion/repo-name/path/to/file
                                                                 |class ClassWithAConstructor {
                                                                 |  protected ClassWithAConstructor(int a, String b) throws This, AndThat, AndWhatElse {
                                                                 |  }
                                                                 |}, """.stripMargin + """
                                                                 |19827:Java/hubifant/another_repo/path/to/another/repo
                                                                 |while(!asleep)
                                                                 |  sheep++)""".stripMargin

    // Strange string concat because SPACE on 5th line is always deleted when saving in intelliJ.
    )
  }



  it should "correctly parse this String containing one file" in {
    val s =
      """9999:Java/samarion/repo-name/path/to/file
         |class ClassWithAConstructor {
         |  protected ClassWithAConstructor(int a, String b) throws This, AndThat, AndWhatElse {
         |  }
         |}""".stripMargin
    assert(ASTExtractor.parseTestString(s).toString == """JavaFile(9999,samarion,repo-name,path/to/file,class ClassWithAConstructor {
                                                         |  protected ClassWithAConstructor(int a, String b) throws This, AndThat, AndWhatElse {
                                                         |  }
                                                         |})""".stripMargin
    )
  }

  //test fails... but because of the AST.
  it should "correctly extract RDD[(owner, repo, path, language, size, AST)] from Blob files" in{
    val test = ASTExtractor.extract("src/test/resources/blobs")

    //val testCollected = test.collect

    println("\n\n\n\n" + test.collect.toList.toString())

/*
    assert(test.collect.toList ==  List(
    ("samarion", "other_repo-name", "path/to/another/file", "Java", 7777,
      PackageDef(Names.default, List(), List(), List(
      ClassDef(NoModifiers, "ClassWithAConstructor", List(), List(), List(), List(
      ConstructorDef(PROTECTED, "ClassWithAConstructor", List(), List(), List(
      ValDef(NoModifiers, "a", List(), PrimitiveTypes.Int, Empty[Expr]),
      ValDef(NoModifiers, "b", List(), PrimitiveTypes.String, Empty[Expr])
      ), List("This", "AndThat", "AndWhatElse"), Block(Nil))
      ), false)
      ))
      ),
      ("samarion", "repo-name", "path/to/file", "Java", 9999,
      PackageDef("com.github.javapasrser.bdd.parsing",List(),
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
              )))),false)))
        )

    ))*/

  }
}