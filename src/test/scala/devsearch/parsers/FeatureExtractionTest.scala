package devsearch.parsers

import devsearch.ast.AST
import org.scalatest.FlatSpec

/**
 * Created by pierre on 25/03/15.
 */
class FeatureExtractionTest extends FlatSpec {
    def getAst(): AST = {
        val fileURL = getClass.getResource("/samples/AccountDataManager.java")
        val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
        JavaParser.parse(filePath)
    }

    "Feature extractor" should "be blabla" in {
        println(getAst())
    }

    it should "coucou" in  {

    }

}
