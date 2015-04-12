package devsearch.features

import devsearch.utils._
import org.scalatest._

class StructuralFeaturesTest extends FlatSpec with CodeProvider {

  "Structural features" should "be found in JavaConcepts.java" in {
    assert(StructuralFeatures.extract(code).toSet == Set(
      ControlFeature(location at 248, "if"),
      ControlFeature(location at 268, "if"),
      ControlFeature(location at 280, "if"),
      ControlFeature(location at 326, "if"),
      ControlFeature(location at 282, "if"),
      ControlFeature(location at 276, "if"),
      ControlFeature(location at 315, "if"),
      ControlFeature(location at 333, "if"),
      ControlFeature(location at 301, "for"),
      ControlFeature(location at 297, "for"),
      ControlFeature(location at 294, "foreach"),
      ControlFeature(location at 286, "while"),
      ControlFeature(location at 287, "while"),
      ControlFeature(location at 290, "do"),
      ControlFeature(location at 293, "do")
    ))
  }
}
