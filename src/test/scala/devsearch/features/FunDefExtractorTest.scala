package devsearch.features

import devsearch.utils._
import org.scalatest._

class FunDefExtractorTest extends FlatSpec with CodeProvider {
  "function extractor" should "work in JavaConcepts.java" in {

    assert(FunDefExtractor.extract(code) == Set(
      FunNameFeature(location at 149, "mm"),
      FunNameFeature(location at 182, "m"),
      FunNameFeature(location at 143, "mm"),
      FunNameFeature(location at 395, "check2"),
      FunNameFeature(location at 154, "nnn"),
      FunNameFeature(location at 64,  "main"),
      FunNameFeature(location at 369, "x"),
      FunNameFeature(location at 165, "ddd"),
      FunNameFeature(location at 256, "doSomething"),
      FunNameFeature(location at 388, "createInputStream"),
      FunNameFeature(location at 262, "main"),
      FunNameFeature(location at 162, "mm"),
      FunNameFeature(location at 306, "parse"),
      FunNameFeature(location at 372, "compare"),
      FunNameFeature(location at 381, "equals"),
      ArgNameFeature(location at 306, "file"),
      ArgNameFeature(location at 395, "val1"),
      ArgNameFeature(location at 382, "obj"),
      ArgNameFeature(location at 369, "x"),
      ArgNameFeature(location at 372, "o2"),
      ArgNameFeature(location at 372, "o1"),
      ArgNameFeature(location at 395, "val2"),
      ArgNameFeature(location at 64,  "args"),
      ArgNameFeature(location at 262, "args"),
      ThrowsFeature(location at 262, "IOException"),
      ThrowsFeature(location at 306, "IOException"),
      ThrowsFeature(location at 262, "ParseException"),
      ThrowsFeature(location at 306, "ParseException"),
      AbstractFunFeature(location at 162),
      ParametricFunFeature(location at 395),
      ParametricFunFeature(location at 369),
      OverridingFunFeature(location at 381),
      OverridingFunFeature(location at 143)
    ))
  }

  it should "toString correctly." in {
    val features = FunDefExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
