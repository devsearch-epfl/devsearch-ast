package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

/**
 * Created by hubi on 4/15/15.
 */
class FeatureEncodingTest extends FlatSpec {

  "Feature.encode" should "escape correctly commas and backslashes." in {
    val loc = CodeFileLocation("ownerName", "repoName", "path/to/File.java")
    val testFeat = ImportFeature(loc at 42, "Backslash\\ and Comma,")
    val decoded = Feature.parse(testFeat.encode)
    assert(decoded.pos == (loc at 42))
    assert(testFeat == decoded)
  }

  it should "even work on line breaks" in {
    val loc = CodeFileLocation("ownerName", "repoName", "path/to/File.java")
    val testFeat = ImportFeature(loc at 41, "even on \n a line break!")
    val decoded = Feature.parse(testFeat.encode)
    assert(decoded.pos == (loc at 41))
    assert(testFeat == decoded)
  }
}
