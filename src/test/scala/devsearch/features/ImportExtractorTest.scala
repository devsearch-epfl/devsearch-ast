package devsearch.features

import org.scalatest.{FlatSpec}

class ImportExtractorTest extends FlatSpec {
    "import extractor" should "extract all imports" in {
      FeatureTestHelper.withSampleCodeData { sampleCodeData =>
        val importFeatures = ImportExtractor.extract(sampleCodeData)

        assert(importFeatures.collect.toSet == Set(
                ImportFeature(FeatureTestHelper.codeFileLocation, InFilePosition(10, 1), "java.util", true, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, InFilePosition(6, 1), "com.github.javaparser.ast.CompilationUnit", false, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, InFilePosition(9, 1), "java.io", true, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, InFilePosition(5, 1), "japa.parser.ParseException", false, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, InFilePosition(7, 1), "org.junit.Ignore", false, false),
                ImportFeature(FeatureTestHelper.codeFileLocation, InFilePosition(3, 1), "com.github.javaparser.JavaParser", false, false)
            )
        )
      }
    }
}
