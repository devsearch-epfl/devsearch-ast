package devsearch.features

import devsearch.utils._
import org.scalatest._

class FunDefExtractorTest extends FlatSpec with CodeProvider {
  "function extractor" should "work in JavaConcepts.java" in {

    assert(FunDefExtractor.extract(code) == Set(
      FunNameFeature(location at 148, "mm"),
      FunNameFeature(location at 181, "m"),
      FunNameFeature(location at 142, "mm"),
      FunNameFeature(location at 394, "check2"),
      FunNameFeature(location at 153, "nnn"),
      FunNameFeature(location at 63,  "main"),
      FunNameFeature(location at 368, "x"),
      FunNameFeature(location at 164, "ddd"),
      FunNameFeature(location at 255, "doSomething"),
      FunNameFeature(location at 387, "createInputStream"),
      FunNameFeature(location at 261, "main"),
      FunNameFeature(location at 161, "mm"),
      FunNameFeature(location at 305, "parse"),
      FunNameFeature(location at 371, "compare"),
      FunNameFeature(location at 380, "equals"),
      ArgNameFeature(location at 305, "file"),
      ArgNameFeature(location at 394, "val1"),
      ArgNameFeature(location at 381, "obj"),
      ArgNameFeature(location at 368, "x"),
      ArgNameFeature(location at 371, "o2"),
      ArgNameFeature(location at 371, "o1"),
      ArgNameFeature(location at 394, "val2"),
      ArgNameFeature(location at 63,  "args"),
      ArgNameFeature(location at 261, "args"),
      ThrowsFeature(location at 261, "IOException"),
      ThrowsFeature(location at 305, "IOException"),
      ThrowsFeature(location at 261, "ParseException"),
      ThrowsFeature(location at 305, "ParseException"),
      AbstractFunFeature(location at 161),
      ParametricFunFeature(location at 394),
      ParametricFunFeature(location at 368),
      OverridingFunFeature(location at 380),
      OverridingFunFeature(location at 142)
    ))
  }

  it should "toString correctly." in {
    val features = FunDefExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
