package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class FieldExtractorTest extends FlatSpec with CodeProvider {

  "field access extractor" should "work in JavaConcepts.java" in {
    assert(FieldExtractor.extract(code) == Set(
      FieldFeature(location at 318, "println"),
      FieldFeature(location at 329, "out"),
      FieldFeature(location at 347, "out"),
      FieldFeature(location at 353, "println"),
      FieldFeature(location at 189, "cc"),
      FieldFeature(location at 357, "println"),
      FieldFeature(location at 248, "println"),
      FieldFeature(location at 350, "out"),
      FieldFeature(location at 169, "arr"),
      FieldFeature(location at 350, "println"),
      FieldFeature(location at 191, "m"),
      FieldFeature(location at 339, "println"),
      FieldFeature(location at 336, "out"),
      FieldFeature(location at 345, "println"),
      FieldFeature(location at 357, "out"),
      FieldFeature(location at 172, "check2"),
      FieldFeature(location at 341, "println"),
      FieldFeature(location at 341, "out"),
      FieldFeature(location at 307, "getName"),
      FieldFeature(location at 239, "println"),
      FieldFeature(location at 308, "intValue"),
      FieldFeature(location at 318, "out"),
      FieldFeature(location at 322, "println"),
      FieldFeature(location at 239, "out"),
      FieldFeature(location at 353, "out"),
      FieldFeature(location at 320, "out"),
      FieldFeature(location at 329, "println"),
      FieldFeature(location at 158, "x"),
      FieldFeature(location at 347, "println"),
      FieldFeature(location at 355, "println"),
      FieldFeature(location at 336, "println"),
      FieldFeature(location at 243, "out"),
      FieldFeature(location at 266, "JavaConcepts"),
      FieldFeature(location at 345, "out"),
      FieldFeature(location at 243, "println"),
      FieldFeature(location at 359, "parse"),
      FieldFeature(location at 264, "out"),
      FieldFeature(location at 264, "println"),
      FieldFeature(location at 339, "out"),
      FieldFeature(location at 322, "out"),
      FieldFeature(location at 382, "equals"),
      FieldFeature(location at 248, "out"),
      FieldFeature(location at 355, "out"),
      FieldFeature(location at 190, "i"),
      FieldFeature(location at 320, "println"),
      FunctionFieldFeature(location at 336, "println", 1),
      FunctionFieldFeature(location at 308, "intValue", 0),
      FunctionFieldFeature(location at 347, "println", 1),
      FunctionFieldFeature(location at 322, "println", 1),
      FunctionFieldFeature(location at 264, "println", 1),
      FunctionFieldFeature(location at 382, "equals", 1),
      FunctionFieldFeature(location at 357, "println", 1),
      FunctionFieldFeature(location at 172, "check2", 2),
      FunctionFieldFeature(location at 339, "println", 1),
      FunctionFieldFeature(location at 350, "println", 1),
      FunctionFieldFeature(location at 353, "println", 1),
      FunctionFieldFeature(location at 318, "println", 1),
      FunctionFieldFeature(location at 341, "println", 1),
      FunctionFieldFeature(location at 191, "m", 0),
      FunctionFieldFeature(location at 239, "println", 1),
      FunctionFieldFeature(location at 307, "getName", 0),
      FunctionFieldFeature(location at 345, "println", 1),
      FunctionFieldFeature(location at 248, "println", 1),
      FunctionFieldFeature(location at 320, "println", 1),
      FunctionFieldFeature(location at 329, "println", 1),
      FunctionFieldFeature(location at 355, "println", 1),
      FunctionFieldFeature(location at 359, "parse", 1),
      FunctionFieldFeature(location at 243, "println", 1)
    ))
  }

  it should "toString correctly" in {
    val features = FieldExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
