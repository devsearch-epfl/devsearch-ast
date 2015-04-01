package devsearch.features

import devsearch.parsers.JavaParser
import org.scalatest.{Matchers, FlatSpec}
import spray.json.JsonParser

class ImportExtractorTest extends FlatSpec with Matchers {
    "import extractor" should "extract all imports" in {
        val sampleCodeData = FeatureTestHelper.getSampleCodeData()
        val importFeatures = ImportExtractor.extract(FeatureTestHelper.sc, sampleCodeData)

        val codeFileLocation = new CodeFileLocation("github", "android", "AccountDataManager.java")
        assert(importFeatures.collect.toSet == Set(
                new ImportFeature(codeFileLocation, "java.util", true, false),
                new ImportFeature(codeFileLocation, "com.github.javaparser.ast.CompilationUnit", false, false),
                new ImportFeature(codeFileLocation, "java.io", true, false),
                new ImportFeature(codeFileLocation, "japa.parser.ParseException", false, false),
                new ImportFeature(codeFileLocation, "org.junit.Ignore", false, false),
                new ImportFeature(codeFileLocation, "com.github.javaparser.JavaParser", false, false)
            )
        )
    }
}
