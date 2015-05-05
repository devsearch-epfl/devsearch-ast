package devsearch.features

import devsearch.utils.CodeProvider
import org.scalatest.FlatSpec

class FeatureRecognizerTest extends FlatSpec {
  "Feature recognizer" should "catch exception on SlickQueryContext.scala" in {
    assert(
      try {
        FeatureRecognizer(CodeProvider.sampleCode("SlickQueryContext.scala"))
        true
      } catch {
        case _: RuntimeException => false
      }
    )
  }
}
