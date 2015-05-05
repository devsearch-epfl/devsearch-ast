package devsearch.features

import devsearch.ast.ContentsSource
import devsearch.parsers.Languages
import devsearch.utils.CodeProvider
import org.scalatest.FlatSpec

class FeatureRecognizerTest extends FlatSpec {
  "Feature recognizer" should "catch exception on SlickQueryContext.scala" in {
    assert(
      try {
        FeatureRecognizer(CodeProvider.sampleCode("SlickQueryContext.scala"))
        true
      } catch {
        case _: RuntimeException => false
      }
    )
  }

  it should "unapply CodeFile" in {
    val source = new ContentsSource("Test.java", """
      List<Integer> l = new java.util.LinkedList<Integer>();
      for(int elem : elements) {
        l.add(elem + 1);
      }
    """)

    val location = CodeFileLocation("unknown_repo", "unknown_user", "Test.java")
    val code = CodeFile(Languages.Java, location, source)

    assert(
      code match {
        case cf @ CodeFile(lang, loc, ast) => true
        case _ => false
      }
    )
  }
}
