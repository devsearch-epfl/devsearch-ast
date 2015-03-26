package devsearch.ast

import org.scalatest._
import devsearch.parsers.JavaParser

class OperatorsTest extends FlatSpec with Matchers {

  "postMap" should "provide identity for identity mapper" in {
    val source = new ContentsSource("ClassWithAConstructor.java", """
      class ClassWithAConstructor {
        protected ClassWithAConstructor(int a, String b) throws This, AndThat, AndWhatElse {
        }
      }
    """)

    val ast = JavaParser.parse(source)
    assert(ast == ast.postMap { ast => 
      val (children, builder) = Operators.unapply(ast)
      Some(builder(children)) // make sure references change
    })
  }

  it should "also provide identity for complex inputs" in {
    val fileURL = getClass.getResource("/samples/JavaConcepts.java")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
    val ast = JavaParser.parse(filePath)

    assert(ast == ast.postMap { ast => 
      val (children, builder) = Operators.unapply(ast)
      Some(builder(children)) // make sure references change
    })
  }
}
