package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ValDefExtractorTest extends FlatSpec with CodeProvider {
  "variable declaration extractor" should "extract all variable declarations" in {
    // TODO(julien, mateusz"): add a test for each type name
    assert(ValDefExtractor.extract(code) == Set(
      VarFeature(location at 218,"x"),
      VarFeature(location at 293,"i"),
      VarFeature(location at 368,"x"),
      VarFeature(location at 317,"e"),
      VarFeature(location at 232,"min"),
      VarFeature(location at 173,"b"),
      VarFeature(location at 308,"y"),
      VarFeature(location at 53, "b"),
      VarFeature(location at 176,"i"),
      VarFeature(location at 263,"cu"),
      VarFeature(location at 210,"z"),
      VarFeature(location at 233,"sl"),
      VarFeature(location at 265,"teste"),
      VarFeature(location at 61, "x"),
      VarFeature(location at 35, "floatWithUnderscore"),
      VarFeature(location at 299,"i"),
      VarFeature(location at 17, "clz2"),
      VarFeature(location at 171,"val2"),
      VarFeature(location at 261,"args"),
      VarFeature(location at 104,"cc"),
      VarFeature(location at 373,"a"),
      VarFeature(location at 29, "sh2"),
      VarFeature(location at 155,"x"),
      VarFeature(location at 50, "z"),
      VarFeature(location at 25, "bye"),
      VarFeature(location at 194,"y"),
      VarFeature(location at 296,"i"),
      VarFeature(location at 23, "arr"),
      VarFeature(location at 394,"val1"),
      VarFeature(location at 262,"x"),
      VarFeature(location at 381,"obj"),
      VarFeature(location at 256,"x"),
      VarFeature(location at 349,"in"),
      VarFeature(location at 27, "byebye"),
      VarFeature(location at 354,"e"),
      VarFeature(location at 214,"x"),
      VarFeature(location at 106,"arr3"),
      VarFeature(location at 228,"iii"),
      VarFeature(location at 100,"arr2"),
      VarFeature(location at 21, "clz4"),
      VarFeature(location at 340,"e"),
      VarFeature(location at 108,"arr4"),
      VarFeature(location at 296,"j"),
      VarFeature(location at 37, "floatWithUnderscoreAndExponent"),
      VarFeature(location at 224,"iii"),
      VarFeature(location at 204,"str"),
      VarFeature(location at 45, "arrLS"),
      VarFeature(location at 63, "args"),
      VarFeature(location at 110,"t"),
      VarFeature(location at 157,"x"),
      VarFeature(location at 41, "doubleWithUnderscoreAndExponent"),
      VarFeature(location at 344,"in2"),
      VarFeature(location at 43, "binaryLiteral"),
      VarFeature(location at 266,"qwe"),
      VarFeature(location at 51, "a"),
      VarFeature(location at 364,"integer"),
      VarFeature(location at 356,"e"),
      VarFeature(location at 15, "clz1"),
      VarFeature(location at 364,"string"),
      VarFeature(location at 335,"e"),
      VarFeature(location at 234,"minl"),
      VarFeature(location at 346,"e"),
      VarFeature(location at 19, "clz3"),
      VarFeature(location at 29, "sh1"),
      VarFeature(location at 170,"val1"),
      VarFeature(location at 394,"val2"),
      VarFeature(location at 212,"i"),
      VarFeature(location at 306,"a"),
      VarFeature(location at 173,"y"),
      VarFeature(location at 33, "longWithUnderscore"),
      VarFeature(location at 343,"in"),
      VarFeature(location at 39, "doubleWithUnderscore"),
      VarFeature(location at 371,"o2"),
      VarFeature(location at 305,"file"),
      VarFeature(location at 369,"c"),
      VarFeature(location at 102,"fff"),
      VarFeature(location at 299,"j"),
      VarFeature(location at 338,"in"),
      VarFeature(location at 375,"e"),
      VarFeature(location at 319,"e"),
      VarFeature(location at 50, "y"),
      VarFeature(location at 168,"x"),
      VarFeature(location at 198,"x"),
      VarFeature(location at 371,"o1"),
      VarFeature(location at 31, "intWithUnderscore"),
      VarFeature(location at 307,"x"),
      VarFeature(location at 67, "b"),
      VarFeature(location at 56, "diamond1"),
      TypedVarFeature(location at 218,"Int","x"),
      TypedVarFeature(location at 364,"ABC","string"),
      TypedVarFeature(location at 364,"Integer","integer"),
      TypedVarFeature(location at 61, "Int","x"),
      TypedVarFeature(location at 256,"List","x"),
      TypedVarFeature(location at 35, "Float","floatWithUnderscore"),
      TypedVarFeature(location at 50, "Int","z"),
      TypedVarFeature(location at 53, "Byte","b"),
      TypedVarFeature(location at 308,"Int","y"),
      TypedVarFeature(location at 335,"RuntimeException","e"),
      TypedVarFeature(location at 346,"IOException","e"),
      TypedVarFeature(location at 100,"Array[Array[Array[Array[Int]]]]","arr2"),
      TypedVarFeature(location at 343,"InputStream","in"),
      TypedVarFeature(location at 233,"Long","sl"),
      TypedVarFeature(location at 319,"RuntimeException","e"),
      TypedVarFeature(location at 110,"JavaConcepts","t"),
      TypedVarFeature(location at 214,"String","x"),
      TypedVarFeature(location at 173,"Boolean","b"),
      TypedVarFeature(location at 43, "Int","binaryLiteral"),
      TypedVarFeature(location at 228,"Int","iii"),
      TypedVarFeature(location at 106,"Array[Array[Int]]","arr3"),
      TypedVarFeature(location at 157,"Int","x"),
      TypedVarFeature(location at 306,"String","a"),
      TypedVarFeature(location at 25, "Byte","bye"),
      TypedVarFeature(location at 293,"Int","i"),
      TypedVarFeature(location at 344,"InputStream","in2"),
      TypedVarFeature(location at 262,"Int","x"),
      TypedVarFeature(location at 234,"Long","minl"),
      TypedVarFeature(location at 375,"Exception","e"),
      TypedVarFeature(location at 371,"Object","o1"),
      TypedVarFeature(location at 317,"NullPointerException","e"),
      TypedVarFeature(location at 50, "Int","y"),
      TypedVarFeature(location at 263,"CompilationUnit","cu"),
      TypedVarFeature(location at 41, "Double","doubleWithUnderscoreAndExponent"),
      TypedVarFeature(location at 369,"Comparator","c"),
      TypedVarFeature(location at 307,"String","x"),
      TypedVarFeature(location at 173,"Boolean","y"),
      TypedVarFeature(location at 67, "Boolean","b"),
      TypedVarFeature(location at 299,"Int","j"),
      TypedVarFeature(location at 21, "Class","clz4"),
      TypedVarFeature(location at 204,"String","str"),
      TypedVarFeature(location at 224,"Int","iii"),
      TypedVarFeature(location at 381,"Object","obj"),
      TypedVarFeature(location at 296,"Int","i"),
      TypedVarFeature(location at 232,"Int","min"),
      TypedVarFeature(location at 394,"A","val1"),
      TypedVarFeature(location at 168,"Int","x"),
      TypedVarFeature(location at 15, "Class","clz1"),
      TypedVarFeature(location at 56, "List","diamond1"),
      TypedVarFeature(location at 340,"IOException","e"),
      TypedVarFeature(location at 266,"QWE","qwe"),
      TypedVarFeature(location at 212,"Int","i"),
      TypedVarFeature(location at 171,"E","val2"),
      TypedVarFeature(location at 356,"Error","e"),
      TypedVarFeature(location at 63, "Array[String]","args"),
      TypedVarFeature(location at 176,"Int","i"),
      TypedVarFeature(location at 368,"Map","x"),
      TypedVarFeature(location at 354,"RuntimeException","e"),
      TypedVarFeature(location at 210,"Int","z"),
      TypedVarFeature(location at 33, "Long","longWithUnderscore"),
      TypedVarFeature(location at 37, "Float","floatWithUnderscoreAndExponent"),
      TypedVarFeature(location at 23, "Array[Int]","arr"),
      TypedVarFeature(location at 27, "Array[Byte]","byebye"),
      TypedVarFeature(location at 349,"InputStream","in"),
      TypedVarFeature(location at 265,"JavaConcepts","teste"),
      TypedVarFeature(location at 155,"Int","x"),
      TypedVarFeature(location at 338,"InputStream","in"),
      TypedVarFeature(location at 394,"B","val2"),
      TypedVarFeature(location at 296,"Int","j"),
      TypedVarFeature(location at 299,"Int","i"),
      TypedVarFeature(location at 29, "Short","sh1"),
      TypedVarFeature(location at 102,"Float","fff"),
      TypedVarFeature(location at 108,"Array[Array[Int]]","arr4"),
      TypedVarFeature(location at 29, "Short","sh2"),
      TypedVarFeature(location at 261,"Array[String]","args"),
      TypedVarFeature(location at 373,"A","a"),
      TypedVarFeature(location at 305,"File","file"),
      TypedVarFeature(location at 198,"Long","x"),
      TypedVarFeature(location at 19, "Class","clz3"),
      TypedVarFeature(location at 39, "Double","doubleWithUnderscore"),
      TypedVarFeature(location at 17, "Class","clz2"),
      TypedVarFeature(location at 51, "Int","a"),
      TypedVarFeature(location at 170,"T","val1"),
      TypedVarFeature(location at 356,"Exception","e"),
      TypedVarFeature(location at 194,"Int","y"),
      TypedVarFeature(location at 371,"Object","o2"),
      TypedVarFeature(location at 104,"Char","cc"),
      TypedVarFeature(location at 45, "Array[Array[List]]","arrLS"),
      TypedVarFeature(location at 31, "Int","intWithUnderscore")
    ))
  }


  it should "toString correctly" in {
    val features = ValDefExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
