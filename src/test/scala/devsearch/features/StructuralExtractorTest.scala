package devsearch.features

import devsearch.utils._
import org.scalatest._

class StructuralExtractorTest extends FlatSpec with CodeProvider {

  "Structural features" should "be found in JavaConcepts.java" in {
    assert(StructuralExtractor.extract(code) == Set(
      ControlFeature(location at 247, "if"),
      ControlFeature(location at 267, "if"),
      ControlFeature(location at 279, "if"),
      ControlFeature(location at 325, "if"),
      ControlFeature(location at 281, "if"),
      ControlFeature(location at 275, "if"),
      ControlFeature(location at 314, "if"),
      ControlFeature(location at 332, "if"),
      ControlFeature(location at 300, "for"),
      ControlFeature(location at 296, "for"),
      ControlFeature(location at 293, "foreach"),
      ControlFeature(location at 285, "while"),
      ControlFeature(location at 286, "while"),
      ControlFeature(location at 289, "do"),
      ControlFeature(location at 292, "do")
    ))
  }


  it should "toString correctly" in {
    val features = StructuralExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
