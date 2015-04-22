package devsearch.features

import devsearch.utils._
import org.scalatest._

class TypeExtractorTest extends FlatSpec with CodeProvider {
  
  "Type feature extractor" should "work on JavaConcepts.java" in {
    assert(TypeExtractor.extract(code) == Set(
      TypeRefFeature(location at 399, "Cloneable"),
      TypeRefFeature(location at 131, "Cloneable"),
      TypeRefFeature(location at 389, "ByteArrayInputStream"),
      TypeRefFeature(location at 355, "RuntimeException"),
      TypeRefFeature(location at 267, "teste.QWE"),
      TypeRefFeature(location at 376, "Exception"),
      TypeRefFeature(location at 318, "NullPointerException"),
      TypeRefFeature(location at 350, "InputStream"),
      TypeRefFeature(location at 336, "RuntimeException"),
      TypeRefFeature(location at 248, "Base"),
      TypeRefFeature(location at 309, "Integer"),
      TypeRefFeature(location at 309, "Object"),
      TypeRefFeature(location at 14,  "Base"),
      TypeRefFeature(location at 257, "Number"),
      TypeRefFeature(location at 320, "RuntimeException"),
      TypeRefFeature(location at 131, "Serializable"),
      TypeRefFeature(location at 341, "IOException"),
      TypeRefFeature(location at 334, "NullPointerException"),
      TypeRefFeature(location at 257, "ArrayList"),
      TypeRefFeature(location at 5,   "japa.parser.ParseException"),
      TypeRefFeature(location at 6,   "com.github.javaparser.ast.CompilationUnit"),
      TypeRefFeature(location at 395, "A"),
      TypeRefFeature(location at 399, "Serializable"),
      TypeRefFeature(location at 10,  "java.util"),
      TypeRefFeature(location at 57,  "List"),
      TypeRefFeature(location at 382, "Object"),
      TypeRefFeature(location at 395, "B"),
      TypeRefFeature(location at 264, "File"),
      TypeRefFeature(location at 339, "InputStream"),
      TypeRefFeature(location at 327, "NullPointerException"),
      TypeRefFeature(location at 257, "List"),
      TypeRefFeature(location at 171, "T"),
      TypeRefFeature(location at 9,   "java.io"),
      TypeRefFeature(location at 22,  "Class"),
      TypeRefFeature(location at 344, "InputStream"),
      TypeRefFeature(location at 369, "T"),
      TypeRefFeature(location at 7,   "org.junit.Ignore"),
      TypeRefFeature(location at 273, "JavaConcepts"),
      TypeRefFeature(location at 347, "IOException"),
      TypeRefFeature(location at 357, "Exception"),
      TypeRefFeature(location at 345, "InputStream"),
      TypeRefFeature(location at 374, "A"),
      TypeRefFeature(location at 16,  "Class"),
      TypeRefFeature(location at 312, "File"),
      TypeRefFeature(location at 18,  "Class"),
      TypeRefFeature(location at 363, "Integer"),
      TypeRefFeature(location at 370, "Comparator"),
      TypeRefFeature(location at 111, "JavaConcepts"),
      TypeRefFeature(location at 172, "E"),
      TypeRefFeature(location at 363, "XXX"),
      TypeRefFeature(location at 264, "CompilationUnit"),
      TypeRefFeature(location at 14,  "Serializable"),
      TypeRefFeature(location at 369, "X"),
      TypeRefFeature(location at 208, "JavaConcepts"),
      TypeRefFeature(location at 306, "CompilationUnit"),
      TypeRefFeature(location at 266, "JavaConcepts"),
      TypeRefFeature(location at 372, "Object"),
      TypeRefFeature(location at 270, "JavaConcepts"),
      TypeRefFeature(location at 365, "ABC"),
      TypeRefFeature(location at 388, "InputStream"),
      TypeRefFeature(location at 357, "Error"),
      TypeRefFeature(location at 363, "Serializable"),
      TypeRefFeature(location at 365, "Integer"),
      TypeRefFeature(location at 14,  "List"),
      TypeRefFeature(location at 173, "T"),
      TypeRefFeature(location at 20,  "Class"),
      TypeRefFeature(location at 173, "E"),
      TypeRefFeature(location at 208, "List"),
      TypeRefFeature(location at 316, "NullPointerException"),
      TypeRefFeature(location at 57,  "LinkedList"),
      TypeRefFeature(location at 369, "Map"),
      TypeRefFeature(location at 3,   "com.github.javaparser.JavaParser"),
      TypeRefFeature(location at 267, "JavaConcepts.QWE"),
      TypeRefFeature(location at 374, "Integer"),
      TypeRefFeature(location at 46,  "List"),
      TypeRefFeature(location at 186, "X"),
      TypeRefFeature(location at 306, "File")
    ))
  }


  it should "toString correctly" in {
    val features = TypeExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
