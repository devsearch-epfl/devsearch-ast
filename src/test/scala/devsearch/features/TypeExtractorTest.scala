package devsearch.features

import devsearch.utils._
import org.scalatest._

class TypeExtractorTest extends FlatSpec with CodeProvider {
  
  "Type feature extractor" should "work on JavaConcepts.java" in {
    assert(TypeExtractor.extract(code) == Set(
      TypeRefFeature(location at 398, "Cloneable"),
      TypeRefFeature(location at 130, "Cloneable"),
      TypeRefFeature(location at 388, "ByteArrayInputStream"),
      TypeRefFeature(location at 354, "RuntimeException"),
      TypeRefFeature(location at 266, "teste.QWE"),
      TypeRefFeature(location at 375, "Exception"),
      TypeRefFeature(location at 317, "NullPointerException"),
      TypeRefFeature(location at 349, "InputStream"),
      TypeRefFeature(location at 335, "RuntimeException"),
      TypeRefFeature(location at 247, "Base"),
      TypeRefFeature(location at 308, "Integer"),
      TypeRefFeature(location at 308, "Object"),
      TypeRefFeature(location at 13,  "Base"),
      TypeRefFeature(location at 256, "Number"),
      TypeRefFeature(location at 319, "RuntimeException"),
      TypeRefFeature(location at 130, "Serializable"),
      TypeRefFeature(location at 340, "IOException"),
      TypeRefFeature(location at 333, "NullPointerException"),
      TypeRefFeature(location at 256, "ArrayList"),
      TypeRefFeature(location at 4,   "japa.parser.ParseException"),
      TypeRefFeature(location at 5,   "com.github.javaparser.ast.CompilationUnit"),
      TypeRefFeature(location at 394, "A"),
      TypeRefFeature(location at 398, "Serializable"),
      TypeRefFeature(location at 9,   "java.util"),
      TypeRefFeature(location at 56,  "List"),
      TypeRefFeature(location at 381, "Object"),
      TypeRefFeature(location at 394, "B"),
      TypeRefFeature(location at 263, "File"),
      TypeRefFeature(location at 338, "InputStream"),
      TypeRefFeature(location at 326, "NullPointerException"),
      TypeRefFeature(location at 256, "List"),
      TypeRefFeature(location at 170, "T"),
      TypeRefFeature(location at 8,   "java.io"),
      TypeRefFeature(location at 21,  "Class"),
      TypeRefFeature(location at 343, "InputStream"),
      TypeRefFeature(location at 368, "T"),
      TypeRefFeature(location at 6,   "org.junit.Ignore"),
      TypeRefFeature(location at 272, "JavaConcepts"),
      TypeRefFeature(location at 346, "IOException"),
      TypeRefFeature(location at 356, "Exception"),
      TypeRefFeature(location at 344, "InputStream"),
      TypeRefFeature(location at 373, "A"),
      TypeRefFeature(location at 15,  "Class"),
      TypeRefFeature(location at 311, "File"),
      TypeRefFeature(location at 17,  "Class"),
      TypeRefFeature(location at 362, "Integer"),
      TypeRefFeature(location at 369, "Comparator"),
      TypeRefFeature(location at 110, "JavaConcepts"),
      TypeRefFeature(location at 171, "E"),
      TypeRefFeature(location at 362, "XXX"),
      TypeRefFeature(location at 263, "CompilationUnit"),
      TypeRefFeature(location at 13,  "Serializable"),
      TypeRefFeature(location at 368, "X"),
      TypeRefFeature(location at 207, "JavaConcepts"),
      TypeRefFeature(location at 305, "CompilationUnit"),
      TypeRefFeature(location at 265, "JavaConcepts"),
      TypeRefFeature(location at 371, "Object"),
      TypeRefFeature(location at 269, "JavaConcepts"),
      TypeRefFeature(location at 364, "ABC"),
      TypeRefFeature(location at 387, "InputStream"),
      TypeRefFeature(location at 356, "Error"),
      TypeRefFeature(location at 362, "Serializable"),
      TypeRefFeature(location at 364, "Integer"),
      TypeRefFeature(location at 13,  "List"),
      TypeRefFeature(location at 172, "T"),
      TypeRefFeature(location at 19,  "Class"),
      TypeRefFeature(location at 172, "E"),
      TypeRefFeature(location at 207, "List"),
      TypeRefFeature(location at 315, "NullPointerException"),
      TypeRefFeature(location at 56,  "LinkedList"),
      TypeRefFeature(location at 368, "Map"),
      TypeRefFeature(location at 2,   "com.github.javaparser.JavaParser"),
      TypeRefFeature(location at 266, "JavaConcepts.QWE"),
      TypeRefFeature(location at 373, "Integer"),
      TypeRefFeature(location at 45,  "List"),
      TypeRefFeature(location at 185, "X"),
      TypeRefFeature(location at 305, "File")
    ))
  }


  it should "toString correctly" in {
    val features = TypeExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
