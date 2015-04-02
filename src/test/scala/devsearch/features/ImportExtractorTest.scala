package devsearch.features

import org.scalatest.{FlatSpec}

class ImportExtractorTest extends FlatSpec {
    "import extractor" should "extract all imports" in {
        val sampleCodeData = FeatureTestHelper.getSampleCodeData()
        val importFeatures = ImportExtractor.extract(sampleCodeData)

        val codeFileLocation = new CodeFileLocation("github", "android", "AccountDataManager.java")
        assert(importFeatures.collect.toSet == Set(
                ImportFeature(codeFileLocation, "java.util", true, false),
                ImportFeature(codeFileLocation, "com.github.javaparser.ast.CompilationUnit", false, false),
                ImportFeature(codeFileLocation, "java.io", true, false),
                ImportFeature(codeFileLocation, "japa.parser.ParseException", false, false),
                ImportFeature(codeFileLocation, "org.junit.Ignore", false, false),
                ImportFeature(codeFileLocation, "com.github.javaparser.JavaParser", false, false)
            )
        )
    }
}
