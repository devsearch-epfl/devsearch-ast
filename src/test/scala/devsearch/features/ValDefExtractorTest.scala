package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ValDefExtractorTest extends FlatSpec with CodeProvider {
  "variable declaration extractor" should "extract all variable declarations" in {
    // TODO(julien, mateusz): add a test for each type name
    assert(Set[Feature](
      TypedVariable(location at 376, "Exception", "e"),
      TypedVariable(location at 32,  "devsearch.ast.PrimitiveTypes.Int$", "intWithUnderscore"),
      TypedVariable(location at 109, "Array[Array[devsearch.ast.PrimitiveTypes.Int$]]", "arr4")
    ).subsetOf(ValDefExtractor.extract(code).toSet))
  }
}
