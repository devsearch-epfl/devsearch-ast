package devsearch.features

import devsearch.utils._
import org.scalatest._

class FunDefFeaturesTest extends FlatSpec with CodeProvider {
  "function extractor" should "work in JavaConcepts.java" in {

    assert(FunDefFeatures.extract(code) == Set(
      FunctionName(location at 149, "mm"),
      FunctionName(location at 182, "m"),
      FunctionName(location at 143, "mm"),
      FunctionName(location at 395, "check2"),
      FunctionName(location at 154, "nnn"),
      FunctionName(location at 64,  "main"),
      FunctionName(location at 369, "x"),
      FunctionName(location at 165, "ddd"),
      FunctionName(location at 256, "doSomething"),
      FunctionName(location at 388, "createInputStream"),
      FunctionName(location at 262, "main"),
      FunctionName(location at 162, "mm"),
      FunctionName(location at 306, "parse"),
      FunctionName(location at 372, "compare"),
      FunctionName(location at 381, "equals"),
      ArgumentName(location at 306, "file"),
      ArgumentName(location at 395, "val1"),
      ArgumentName(location at 382, "obj"),
      ArgumentName(location at 369, "x"),
      ArgumentName(location at 372, "o2"),
      ArgumentName(location at 372, "o1"),
      ArgumentName(location at 395, "val2"),
      ArgumentName(location at 64,  "args"),
      ArgumentName(location at 262, "args"),
      ThrowsException(location at 262, "IOException"),
      ThrowsException(location at 306, "IOException"),
      ThrowsException(location at 262, "ParseException"),
      ThrowsException(location at 306, "ParseException"),
      AbstractFunction(location at 162),
      ParametricFunction(location at 395),
      ParametricFunction(location at 369),
      OverridingFunction(location at 381),
      OverridingFunction(location at 143)
    ))
  }

  it should "toString correctly." in {
    val features = FunDefFeatures.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
