package devsearch.features

import org.scalatest.{FlatSpec}

class ImportExtractorTest extends FlatSpec {
    "import extractor" should "extract all imports" in {
        val sampleCodeData = FeatureTestHelper.getSampleCodeData()
        val importFeatures = ImportExtractor.extract(sampleCodeData)

        assert(importFeatures.collect.toSet == Set(
                ImportFeature(FeatureTestHelper.codeFileLocation, "java.util", true, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, "com.github.javaparser.ast.CompilationUnit", false, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, "java.io", true, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, "japa.parser.ParseException", false, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, "org.junit.Ignore", false, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, "com.github.javaparser.JavaParser", false, false)
            )
        )
    }
}
