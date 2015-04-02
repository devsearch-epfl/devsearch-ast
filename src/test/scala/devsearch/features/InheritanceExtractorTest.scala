package devsearch.features

import org.scalatest.{FlatSpec, Matchers}

class InheritanceExtractorTest extends FlatSpec with Matchers {
    "inheritance extractor" should "extract all class extensions and interface implementations" in {
        val codeFileData = FeatureTestHelper.getSampleCodeData()

        val inheritanceFeatures = InheritanceExtractor.extract(codeFileData)
        assert(inheritanceFeatures.collect.toSet == Set(
                InheritanceFeature(FeatureTestHelper.codeFileLocation, "A", "XXX"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, "Y", "X"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, "XXX", "Serializable"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, "JavaConcepts", "Base"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, "JavaConcepts", "Serializable"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, "XXX", "Cloneable"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, "A", "Serializable"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, "QWE", "JavaConcepts")
            )
        )
    }
}
