package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class FieldExtractorTest extends FlatSpec with CodeProvider {

  "field access extractor" should "work in JavaConcepts.java" in {
    assert(FieldExtractor.extract(code) == Set(
      FieldFeature(location at 319, "println"),
      FieldFeature(location at 330, "out"),
      FieldFeature(location at 348, "out"),
      FieldFeature(location at 354, "println"),
      FieldFeature(location at 190, "cc"),
      FieldFeature(location at 358, "println"),
      FieldFeature(location at 249, "println"),
      FieldFeature(location at 351, "out"),
      FieldFeature(location at 170, "arr"),
      FieldFeature(location at 351, "println"),
      FieldFeature(location at 192, "m"),
      FieldFeature(location at 340, "println"),
      FieldFeature(location at 337, "out"),
      FieldFeature(location at 346, "println"),
      FieldFeature(location at 358, "out"),
      FieldFeature(location at 173, "check2"),
      FieldFeature(location at 342, "println"),
      FieldFeature(location at 342, "out"),
      FieldFeature(location at 308, "getName"),
      FieldFeature(location at 240, "println"),
      FieldFeature(location at 309, "intValue"),
      FieldFeature(location at 319, "out"),
      FieldFeature(location at 323, "println"),
      FieldFeature(location at 240, "out"),
      FieldFeature(location at 354, "out"),
      FieldFeature(location at 321, "out"),
      FieldFeature(location at 330, "println"),
      FieldFeature(location at 159, "x"),
      FieldFeature(location at 348, "println"),
      FieldFeature(location at 356, "println"),
      FieldFeature(location at 337, "println"),
      FieldFeature(location at 244, "out"),
      FieldFeature(location at 267, "JavaConcepts"),
      FieldFeature(location at 346, "out"),
      FieldFeature(location at 244, "println"),
      FieldFeature(location at 360, "parse"),
      FieldFeature(location at 265, "out"),
      FieldFeature(location at 265, "println"),
      FieldFeature(location at 340, "out"),
      FieldFeature(location at 323, "out"),
      FieldFeature(location at 383, "equals"),
      FieldFeature(location at 249, "out"),
      FieldFeature(location at 356, "out"),
      FieldFeature(location at 191, "i"),
      FieldFeature(location at 321, "println"),
      FunctionFieldFeature(location at 337, "println", 1),
      FunctionFieldFeature(location at 309, "intValue", 0),
      FunctionFieldFeature(location at 348, "println", 1),
      FunctionFieldFeature(location at 323, "println", 1),
      FunctionFieldFeature(location at 265, "println", 1),
      FunctionFieldFeature(location at 383, "equals", 1),
      FunctionFieldFeature(location at 358, "println", 1),
      FunctionFieldFeature(location at 173, "check2", 2),
      FunctionFieldFeature(location at 340, "println", 1),
      FunctionFieldFeature(location at 351, "println", 1),
      FunctionFieldFeature(location at 354, "println", 1),
      FunctionFieldFeature(location at 319, "println", 1),
      FunctionFieldFeature(location at 342, "println", 1),
      FunctionFieldFeature(location at 192, "m", 0),
      FunctionFieldFeature(location at 240, "println", 1),
      FunctionFieldFeature(location at 308, "getName", 0),
      FunctionFieldFeature(location at 346, "println", 1),
      FunctionFieldFeature(location at 249, "println", 1),
      FunctionFieldFeature(location at 321, "println", 1),
      FunctionFieldFeature(location at 330, "println", 1),
      FunctionFieldFeature(location at 356, "println", 1),
      FunctionFieldFeature(location at 360, "parse", 1),
      FunctionFieldFeature(location at 244, "println", 1)
    ))
  }

  it should "toString correctly" in {
    val features = FieldExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
