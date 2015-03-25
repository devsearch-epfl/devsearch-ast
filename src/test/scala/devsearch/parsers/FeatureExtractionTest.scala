package devsearch.parsers

import devsearch.ast.AST
import org.scalatest.FlatSpec
import sext._

/**
 * Created by pierre on 25/03/15.
 */
class FeatureExtractionTest extends FlatSpec {
    def getSampleAst(): AST = {
        val fileURL = getClass.getResource("/samples/AccountDataManager.java")
        val filePath = new java.io.File(fileURL.toURI).getAbsolutePath
        JavaParser.parse(filePath)
    }

    def convertToReadableString(tree: AST) = {
        tree.treeString
    }

    "Feature extractor" should "be blabla" in {
        println(convertToReadableString(getSampleAst()))
    }

    it should "coucou" in  {

    }

}
