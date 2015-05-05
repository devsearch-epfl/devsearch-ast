package devsearch.normalized

import devsearch.features.{FeatureRecognizer, CodeFile}
import devsearch.utils._
import org.scalatest._

class NormalizationTest extends FlatSpec with CodeProvider {

  "Normalization" should "work on JavaConcepts.java" in {
    Normalizer(sampleCode("JavaConcepts.java").ast)
  }

  it should "work on AccountDataManager.java" in {
    Normalizer(sampleCode("AccountDataManager.java").ast)
  }

  it should "work on UI.scala" in {
    Normalizer(sampleCode("UI.scala").ast)
  }

  it should "work on event.js" in {
    Normalizer(sampleCode("event.js").ast)
  }

  it should "work on fcktextcolorcommand.js" in {
    Normalizer(sampleCode("fcktextcolorcommand.js").ast)
  }

  it should "work on SlickQueryContext.scala" in {
    Normalizer(sampleCode("SlickQueryContext.scala").ast)
  }
}
