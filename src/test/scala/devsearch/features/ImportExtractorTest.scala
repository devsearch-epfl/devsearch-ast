package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ImportExtractorTest extends FlatSpec with CodeProvider {

  "import extractor" should "extract all imports" in {
    assert(ImportExtractor.extract(code) == Set(
      ImportFeature(location at 9, "java.util"),
      ImportFeature(location at 5, "com.github.javaparser.ast.CompilationUnit"),
      ImportFeature(location at 8, "java.io"),
      ImportFeature(location at 4, "japa.parser.ParseException"),
      ImportFeature(location at 6, "org.junit.Ignore"),
      ImportFeature(location at 2, "com.github.javaparser.JavaParser")
    ))
  }

  it should "toString correctly" in {
    val features = ImportExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
