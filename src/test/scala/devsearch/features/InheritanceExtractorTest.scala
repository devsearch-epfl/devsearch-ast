package devsearch.features

import org.scalatest.{FlatSpec, Matchers}

class InheritanceExtractorTest extends FlatSpec with Matchers {
    "inheritance extractor" should "extract all class extensions and interface implementations" in {
        val codeFileData = FeatureTestHelper.getSampleCodeData()

        val inheritanceFeatures = InheritanceExtractor.extract(codeFileData)
        val codeFileLocation = new CodeFileLocation("unknown_repo", "unknown_user", "JavaConcepts.java")
        assert(inheritanceFeatures.collect.toSet == Set(
                new InheritanceFeature(codeFileLocation, "A", "XXX"),
                new InheritanceFeature(codeFileLocation, "Y", "X"),
                new InheritanceFeature(codeFileLocation, "XXX", "Serializable"),
                new InheritanceFeature(codeFileLocation, "JavaConcepts", "Base"),
                new InheritanceFeature(codeFileLocation, "JavaConcepts", "Serializable"),
                new InheritanceFeature(codeFileLocation, "XXX", "Cloneable"),
                new InheritanceFeature(codeFileLocation, "A", "Serializable"),
                new InheritanceFeature(codeFileLocation, "QWE", "JavaConcepts")
            )
        )
    }
}
