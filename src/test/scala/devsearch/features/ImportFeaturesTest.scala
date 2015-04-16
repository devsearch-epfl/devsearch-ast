package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ImportFeaturesTest extends FlatSpec with CodeProvider {

  "import extractor" should "extract all imports" in {
    assert(ImportFeatures.extract(code) == Set(
      ImportFeature(location at 10, "java.util"),
      ImportFeature(location at 6,  "com.github.javaparser.ast.CompilationUnit"),
      ImportFeature(location at 9,  "java.io"),
      ImportFeature(location at 5,  "japa.parser.ParseException"),
      ImportFeature(location at 7,  "org.junit.Ignore"),
      ImportFeature(location at 3,  "com.github.javaparser.JavaParser")
    ))
  }

  it should "toString correctly" in {
    val features = ImportFeatures.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
