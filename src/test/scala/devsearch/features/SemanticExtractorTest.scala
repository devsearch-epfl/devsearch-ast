package devsearch.features

import devsearch.ast._
import devsearch.parsers._
import org.scalatest._

class SemanticExtractorTest extends FlatSpec {
  
  "ComplexFeatures" should "find map/flatMap calls in Scala" in {
    val source = new ContentsSource("Test.scala", """
      a.map(x => x + 1)
      obj.field1.field2.map(f)
      l.flatMap(f)
    """)

    val ast = QueryParser.parse(source)
    val location = CodeFileLocation("unknown_repo", "unknown_user", "Test.scala")
    val code = new CodeFile(location, ast)
    assert(SemanticExtractor.extract(code) == Set(
      MapCallFeature(location at 1),
      MapCallFeature(location at 2),
      FlatMapCallFeature(location at 3)
    ))
  }

  it should "find map calls in Java" in {
    val source = new ContentsSource("Test.java", """
      List<Integer> l = new java.util.LinkedList<Integer>();
      for(int elem : elements) {
        l.add(elem + 1);
      }
    """)

    val ast = JavaParser.parse(source)
    val location = CodeFileLocation("unknown_repo", "unknown_user", "Test.java")
    val code = new CodeFile(location, ast)
    assert(SemanticExtractor.extract(code) == Set(
      MapCallFeature(location at 4)
    ))
  }

  it should "find flatMap calls in Java" in {
    val source = new ContentsSource("Test2.java", """
      List<A> list = new ArrayList<>();
      for (B b : bs) {
        for (C c : b.getCs()) {
          A a = transform(c);
          list.add(a);
        }
      }

      List<L> l2 = new LinkedList<>();
      for (List<L> l : listOfLists) {
        l2.addAll(l);
      }
    """)

    val location = CodeFileLocation("unknown_user", "unknown_repo", "Test2.java")
    val code = CodeFile(Languages.Java, location, source)
    assert(SemanticExtractor.extract(code) == Set(
      FlatMapCallFeature(location at 6),
      FlatMapCallFeature(location at 12)
    ))
  }
}
