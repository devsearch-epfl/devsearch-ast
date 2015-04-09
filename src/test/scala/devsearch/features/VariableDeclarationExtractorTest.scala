package devsearch.features

import org.scalatest.{Matchers, FlatSpec}

class VariableDeclarationExtractorTest extends FlatSpec with Matchers {
    "variable declaration extractor" should "extract all variable declarations" in {
      FeatureTestHelper.withSampleCodeData { codeFileData =>

        val variableDeclarationFeatures = VariableDeclarationExtractor.extract(codeFileData)
        // TODO(julien, mateusz): add a test for each type name
        assert(
            Set[AbstractFeature](
                VariableDeclarationFeature(FeatureTestHelper.codeFileLocation, InFilePosition(376, 26), "Exception", "e"),
                VariableDeclarationFeature(FeatureTestHelper.codeFileLocation, InFilePosition(32, 9), "devsearch.ast.PrimitiveTypes.Int$", "intWithUnderscore"),
                VariableDeclarationFeature(FeatureTestHelper.codeFileLocation, InFilePosition(109,18), "Array[Array[devsearch.ast.PrimitiveTypes.Int$]]", "arr4")
            ).subsetOf(variableDeclarationFeatures.collect.toSet)
        )
      }
    }
}
