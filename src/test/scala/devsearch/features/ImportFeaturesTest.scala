package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ImportFeaturesTest extends FlatSpec with CodeProvider {

  "import extractor" should "extract all imports" in {
    assert(ImportFeatures.extract(code).toSet == Set(
      ImportFeature(location at 10, "java.util"),
      ImportFeature(location at 6,  "com.github.javaparser.ast.CompilationUnit"),
      ImportFeature(location at 9,  "java.io"),
      ImportFeature(location at 5,  "japa.parser.ParseException"),
      ImportFeature(location at 7,  "org.junit.Ignore"),
      ImportFeature(location at 3,  "com.github.javaparser.JavaParser")
    ))
  }

  it should "toString correctly" in {
    assert(ImportFeatures.extract(code).toList.map(_.toString).sortBy(e => e).foldLeft("")((acc, curr) => acc + curr+"\n") ==
      """import=com.github.javaparser.JavaParser,unknown_user,unknown_repo/JavaConcepts.java,3;
        |import=com.github.javaparser.ast.CompilationUnit,unknown_user,unknown_repo/JavaConcepts.java,6;
        |import=japa.parser.ParseException,unknown_user,unknown_repo/JavaConcepts.java,5;
        |import=java.io,unknown_user,unknown_repo/JavaConcepts.java,9;
        |import=java.util,unknown_user,unknown_repo/JavaConcepts.java,10;
        |import=org.junit.Ignore,unknown_user,unknown_repo/JavaConcepts.java,7;
        |""".stripMargin
    )
  }
}
