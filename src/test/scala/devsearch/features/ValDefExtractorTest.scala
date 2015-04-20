package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ValDefExtractorTest extends FlatSpec with CodeProvider {
  "variable declaration extractor" should "extract all variable declarations" in {
    // TODO(julien, mateusz): add a test for each type name
    assert(Set[Feature](
      TypedVarFeature(location at 376, "Exception", "e"),
      TypedVarFeature(location at 32,  "Int", "intWithUnderscore"),
      TypedVarFeature(location at 109, "Array[Array[Int]]", "arr4")
    ).subsetOf(ValDefExtractor.extract(code)))
  }


  it should "toString correctly" in {
    val features = ValDefExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
