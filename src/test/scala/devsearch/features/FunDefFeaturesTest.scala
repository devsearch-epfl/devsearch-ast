package devsearch.features

import devsearch.utils._
import org.scalatest._

class FunDefFeaturesTest extends FlatSpec with CodeProvider {
  "function extractor" should "work in JavaConcepts.java" in {
    assert(FunDefFeatures.extract(code).toSet == Set(
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
      OverridingFunction(location at 381),
      OverridingFunction(location at 143)
    ))
  }

  it should "toString correctly." in {
    assert(FunDefFeatures.extract(code).toList.map(_.toString).sortBy(e => e).foldLeft("")((acc, curr) => acc + curr+"\n") ==
      """abstractFunction,unknown_user,unknown_repo,JavaConcepts.java,162
        |argumentName=args,unknown_user,unknown_repo,JavaConcepts.java,262
        |argumentName=args,unknown_user,unknown_repo,JavaConcepts.java,64
        |argumentName=file,unknown_user,unknown_repo,JavaConcepts.java,306
        |argumentName=o1,unknown_user,unknown_repo,JavaConcepts.java,372
        |argumentName=o2,unknown_user,unknown_repo,JavaConcepts.java,372
        |argumentName=obj,unknown_user,unknown_repo,JavaConcepts.java,382
        |argumentName=val1,unknown_user,unknown_repo,JavaConcepts.java,395
        |argumentName=val2,unknown_user,unknown_repo,JavaConcepts.java,395
        |argumentName=x,unknown_user,unknown_repo,JavaConcepts.java,369
        |functionName=check2,unknown_user,unknown_repo,JavaConcepts.java,395
        |functionName=compare,unknown_user,unknown_repo,JavaConcepts.java,372
        |functionName=createInputStream,unknown_user,unknown_repo,JavaConcepts.java,388
        |functionName=ddd,unknown_user,unknown_repo,JavaConcepts.java,165
        |functionName=doSomething,unknown_user,unknown_repo,JavaConcepts.java,256
        |functionName=equals,unknown_user,unknown_repo,JavaConcepts.java,381
        |functionName=m,unknown_user,unknown_repo,JavaConcepts.java,182
        |functionName=main,unknown_user,unknown_repo,JavaConcepts.java,262
        |functionName=main,unknown_user,unknown_repo,JavaConcepts.java,64
        |functionName=mm,unknown_user,unknown_repo,JavaConcepts.java,143
        |functionName=mm,unknown_user,unknown_repo,JavaConcepts.java,149
        |functionName=mm,unknown_user,unknown_repo,JavaConcepts.java,162
        |functionName=nnn,unknown_user,unknown_repo,JavaConcepts.java,154
        |functionName=parse,unknown_user,unknown_repo,JavaConcepts.java,306
        |functionName=x,unknown_user,unknown_repo,JavaConcepts.java,369
        |overridingFunction,unknown_user,unknown_repo,JavaConcepts.java,143
        |overridingFunction,unknown_user,unknown_repo,JavaConcepts.java,381
        |throwsException=IOException,unknown_user,unknown_repo,JavaConcepts.java,262
        |throwsException=IOException,unknown_user,unknown_repo,JavaConcepts.java,306
        |throwsException=ParseException,unknown_user,unknown_repo,JavaConcepts.java,262
        |throwsException=ParseException,unknown_user,unknown_repo,JavaConcepts.java,306
        |""".stripMargin)
  }
}
