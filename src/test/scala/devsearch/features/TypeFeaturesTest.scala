package devsearch.features

import devsearch.utils._
import org.scalatest._

class TypeFeaturesTest extends FlatSpec with CodeProvider {
  
  "Type feature extractor" should "work on JavaConcepts.java" in {
    assert(TypeFeatures.extract(code) == Set(
      TypeReference(location at 399, "Cloneable"),
      TypeReference(location at 131, "Cloneable"),
      TypeReference(location at 389, "ByteArrayInputStream"),
      TypeReference(location at 355, "RuntimeException"),
      TypeReference(location at 267, "teste.QWE"),
      TypeReference(location at 376, "Exception"),
      TypeReference(location at 318, "NullPointerException"),
      TypeReference(location at 350, "InputStream"),
      TypeReference(location at 336, "RuntimeException"),
      TypeReference(location at 248, "Base"),
      TypeReference(location at 309, "Integer"),
      TypeReference(location at 309, "Object"),
      TypeReference(location at 14,  "Base"),
      TypeReference(location at 257, "Number"),
      TypeReference(location at 320, "RuntimeException"),
      TypeReference(location at 131, "Serializable"),
      TypeReference(location at 341, "IOException"),
      TypeReference(location at 334, "NullPointerException"),
      TypeReference(location at 257, "ArrayList"),
      TypeReference(location at 5,   "japa.parser.ParseException"),
      TypeReference(location at 6,   "com.github.javaparser.ast.CompilationUnit"),
      TypeReference(location at 395, "A"),
      TypeReference(location at 399, "Serializable"),
      TypeReference(location at 10,  "java.util"),
      TypeReference(location at 57,  "List"),
      TypeReference(location at 382, "Object"),
      TypeReference(location at 395, "B"),
      TypeReference(location at 264, "File"),
      TypeReference(location at 339, "InputStream"),
      TypeReference(location at 327, "NullPointerException"),
      TypeReference(location at 257, "List"),
      TypeReference(location at 171, "T"),
      TypeReference(location at 9,   "java.io"),
      TypeReference(location at 22,  "Class"),
      TypeReference(location at 344, "InputStream"),
      TypeReference(location at 369, "T"),
      TypeReference(location at 7,   "org.junit.Ignore"),
      TypeReference(location at 273, "JavaConcepts"),
      TypeReference(location at 347, "IOException"),
      TypeReference(location at 357, "Exception"),
      TypeReference(location at 345, "InputStream"),
      TypeReference(location at 374, "A"),
      TypeReference(location at 16,  "Class"),
      TypeReference(location at 312, "File"),
      TypeReference(location at 18,  "Class"),
      TypeReference(location at 363, "Integer"),
      TypeReference(location at 370, "Comparator"),
      TypeReference(location at 111, "JavaConcepts"),
      TypeReference(location at 172, "E"),
      TypeReference(location at 363, "XXX"),
      TypeReference(location at 264, "CompilationUnit"),
      TypeReference(location at 14,  "Serializable"),
      TypeReference(location at 369, "X"),
      TypeReference(location at 208, "JavaConcepts"),
      TypeReference(location at 306, "CompilationUnit"),
      TypeReference(location at 266, "JavaConcepts"),
      TypeReference(location at 372, "Object"),
      TypeReference(location at 270, "JavaConcepts"),
      TypeReference(location at 365, "ABC"),
      TypeReference(location at 388, "InputStream"),
      TypeReference(location at 357, "Error"),
      TypeReference(location at 363, "Serializable"),
      TypeReference(location at 365, "Integer"),
      TypeReference(location at 14,  "List"),
      TypeReference(location at 173, "T"),
      TypeReference(location at 20,  "Class"),
      TypeReference(location at 173, "E"),
      TypeReference(location at 208, "List"),
      TypeReference(location at 316, "NullPointerException"),
      TypeReference(location at 57,  "LinkedList"),
      TypeReference(location at 369, "Map"),
      TypeReference(location at 3,   "com.github.javaparser.JavaParser"),
      TypeReference(location at 267, "JavaConcepts.QWE"),
      TypeReference(location at 374, "Integer"),
      TypeReference(location at 46,  "List"),
      TypeReference(location at 186, "X"),
      TypeReference(location at 306, "File")
    ))
  }


  it should "toString correctly" in {
    val features = TypeFeatures.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
