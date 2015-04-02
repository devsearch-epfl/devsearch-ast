package devsearch.features

import org.scalatest.{Matchers, FlatSpec}

class VariableDeclarationExtractorTest extends FlatSpec with Matchers {
    "variable declaration extractor" should "extract all variable declarations" in {
        val codeFileData = FeatureTestHelper.getSampleCodeData()

        val variableDeclarationFeatures = VariableDeclarationExtractor.extract(codeFileData)
        variableDeclarationFeatures.foreach(println)
        assert(
            Set[AbstractFeature](
                VariableDeclarationFeature(FeatureTestHelper.codeFileLocation, InFilePosition(376, 26), "Exception", "e"),
                VariableDeclarationFeature(FeatureTestHelper.codeFileLocation, InFilePosition(32, 9), "devsearch.ast.PrimitiveTypes.Int$", "intWithUnderscore")
            ).subsetOf(variableDeclarationFeatures.collect.toSet)
        )
    }
}
