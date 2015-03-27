package devsearch.features

import devsearch.parsers.JavaParser
import org.scalatest.{Matchers, FlatSpec}
import spray.json.JsonParser

/**
 * Created by pierre on 27/03/15.
 */
class ImportExtractorTest extends FlatSpec with Matchers {
    "import extractor" should "extract all imports" in {
        val fileURL = getClass.getResource("/samples/JavaConcepts.java")
        val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
        val ast = JavaParser.parse(filePath)
        assert(ImportExtractor.extractFeatures(ast) == JsonParser( """[
            {"type": "import", "domain": "com.github.javaparser.JavaParser", "containsAsterisk": "false", "isStatic": "false"},
            {"type": "import", "domain": "japa.parser.ParseException", "containsAsterisk": "false", "isStatic": "false"},
            {"type": "import", "domain": "com.github.javaparser.ast.CompilationUnit", "containsAsterisk": "false", "isStatic": "false"},
            {"type": "import", "domain": "org.junit.Ignore", "containsAsterisk": "false", "isStatic": "false"},
            {"type": "import", "domain": "java.io.*", "containsAsterisk": "true", "isStatic": "false"},
            {"type": "import", "domain": "java.util.*", "containsAsterisk": "true", "isStatic": "false"}
        ]"""))
    }
}
