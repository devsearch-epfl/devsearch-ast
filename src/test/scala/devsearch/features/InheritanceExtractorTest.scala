package devsearch.features

import org.scalatest.{FlatSpec, Matchers}

class InheritanceExtractorTest extends FlatSpec with Matchers {
    "inheritance extractor" should "extract all class extensions and interface implementations" in {
        val codeFileData = FeatureTestHelper.getSampleCodeData()

        val inheritanceFeatures = InheritanceExtractor.extract(codeFileData)
        assert(inheritanceFeatures.collect.toSet == Set(
                InheritanceFeature(FeatureTestHelper.codeFileLocation, InFilePosition(363, 363), "A", "XXX"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, InFilePosition(185, 185), "Y", "X"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, InFilePosition(399, 399), "XXX", "Serializable"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, InFilePosition(12, 12), "JavaConcepts", "Base"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, InFilePosition(12, 12), "JavaConcepts", "Serializable"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, InFilePosition(399, 399), "XXX", "Cloneable"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, InFilePosition(363, 363), "A", "Serializable"),
                InheritanceFeature(FeatureTestHelper.codeFileLocation, InFilePosition(208, 208), "QWE", "JavaConcepts")
            )
        )
    }
}
