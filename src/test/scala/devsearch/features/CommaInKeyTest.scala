package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

/**
 * Created by hubi on 4/15/15.
 */
class CommaInKeyTest extends FlatSpec {
    "Features.toString()" should "escape correctly commas and backslashes." in {
      val loc = CodeFileLocation("ownerName", "repoName", "File.java")
      val testFeat = ImportFeature(loc at 42, "Backslash\\ and Comma,")
      assert(testFeat.toString() == "import=Backslash\\\\ and Comma\\,,ownerName,repoName,File.java,42")
    }
}
