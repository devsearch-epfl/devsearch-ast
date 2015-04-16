package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ValDefFeaturesTest extends FlatSpec with CodeProvider {
  "variable declaration extractor" should "extract all variable declarations" in {
    // TODO(julien, mateusz): add a test for each type name
    assert(Set[Feature](
      TypedVariable(location at 376, "Exception", "e"),
      TypedVariable(location at 32,  "Int", "intWithUnderscore"),
      TypedVariable(location at 109, "Array[Array[Int]]", "arr4")
    ).subsetOf(ValDefFeatures.extract(code)))
  }


  it should "toString correctly" in {
    val features = ValDefFeatures.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
