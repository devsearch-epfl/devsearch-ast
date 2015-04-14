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
    assert(TypeFeatures.extract(code).toList.map(_.toString).sortBy(e => e).foldLeft("")((acc, curr) => acc + curr+"\n") ==
      """typeReference=A,unknown_user,unknown_repo,JavaConcepts.java,374
        |typeReference=A,unknown_user,unknown_repo,JavaConcepts.java,395
        |typeReference=ABC,unknown_user,unknown_repo,JavaConcepts.java,365
        |typeReference=ArrayList,unknown_user,unknown_repo,JavaConcepts.java,257
        |typeReference=B,unknown_user,unknown_repo,JavaConcepts.java,395
        |typeReference=Base,unknown_user,unknown_repo,JavaConcepts.java,14
        |typeReference=Base,unknown_user,unknown_repo,JavaConcepts.java,248
        |typeReference=ByteArrayInputStream,unknown_user,unknown_repo,JavaConcepts.java,389
        |typeReference=Class,unknown_user,unknown_repo,JavaConcepts.java,16
        |typeReference=Class,unknown_user,unknown_repo,JavaConcepts.java,18
        |typeReference=Class,unknown_user,unknown_repo,JavaConcepts.java,20
        |typeReference=Class,unknown_user,unknown_repo,JavaConcepts.java,22
        |typeReference=Cloneable,unknown_user,unknown_repo,JavaConcepts.java,131
        |typeReference=Cloneable,unknown_user,unknown_repo,JavaConcepts.java,399
        |typeReference=Comparator,unknown_user,unknown_repo,JavaConcepts.java,370
        |typeReference=CompilationUnit,unknown_user,unknown_repo,JavaConcepts.java,264
        |typeReference=CompilationUnit,unknown_user,unknown_repo,JavaConcepts.java,306
        |typeReference=E,unknown_user,unknown_repo,JavaConcepts.java,172
        |typeReference=E,unknown_user,unknown_repo,JavaConcepts.java,173
        |typeReference=Error,unknown_user,unknown_repo,JavaConcepts.java,357
        |typeReference=Exception,unknown_user,unknown_repo,JavaConcepts.java,357
        |typeReference=Exception,unknown_user,unknown_repo,JavaConcepts.java,376
        |typeReference=File,unknown_user,unknown_repo,JavaConcepts.java,264
        |typeReference=File,unknown_user,unknown_repo,JavaConcepts.java,306
        |typeReference=File,unknown_user,unknown_repo,JavaConcepts.java,312
        |typeReference=IOException,unknown_user,unknown_repo,JavaConcepts.java,341
        |typeReference=IOException,unknown_user,unknown_repo,JavaConcepts.java,347
        |typeReference=InputStream,unknown_user,unknown_repo,JavaConcepts.java,339
        |typeReference=InputStream,unknown_user,unknown_repo,JavaConcepts.java,344
        |typeReference=InputStream,unknown_user,unknown_repo,JavaConcepts.java,345
        |typeReference=InputStream,unknown_user,unknown_repo,JavaConcepts.java,350
        |typeReference=InputStream,unknown_user,unknown_repo,JavaConcepts.java,388
        |typeReference=Integer,unknown_user,unknown_repo,JavaConcepts.java,309
        |typeReference=Integer,unknown_user,unknown_repo,JavaConcepts.java,363
        |typeReference=Integer,unknown_user,unknown_repo,JavaConcepts.java,365
        |typeReference=Integer,unknown_user,unknown_repo,JavaConcepts.java,374
        |typeReference=JavaConcepts,unknown_user,unknown_repo,JavaConcepts.java,111
        |typeReference=JavaConcepts,unknown_user,unknown_repo,JavaConcepts.java,208
        |typeReference=JavaConcepts,unknown_user,unknown_repo,JavaConcepts.java,266
        |typeReference=JavaConcepts,unknown_user,unknown_repo,JavaConcepts.java,270
        |typeReference=JavaConcepts,unknown_user,unknown_repo,JavaConcepts.java,273
        |typeReference=JavaConcepts.QWE,unknown_user,unknown_repo,JavaConcepts.java,267
        |typeReference=LinkedList,unknown_user,unknown_repo,JavaConcepts.java,57
        |typeReference=List,unknown_user,unknown_repo,JavaConcepts.java,14
        |typeReference=List,unknown_user,unknown_repo,JavaConcepts.java,208
        |typeReference=List,unknown_user,unknown_repo,JavaConcepts.java,257
        |typeReference=List,unknown_user,unknown_repo,JavaConcepts.java,46
        |typeReference=List,unknown_user,unknown_repo,JavaConcepts.java,57
        |typeReference=Map,unknown_user,unknown_repo,JavaConcepts.java,369
        |typeReference=NullPointerException,unknown_user,unknown_repo,JavaConcepts.java,316
        |typeReference=NullPointerException,unknown_user,unknown_repo,JavaConcepts.java,318
        |typeReference=NullPointerException,unknown_user,unknown_repo,JavaConcepts.java,327
        |typeReference=NullPointerException,unknown_user,unknown_repo,JavaConcepts.java,334
        |typeReference=Number,unknown_user,unknown_repo,JavaConcepts.java,257
        |typeReference=Object,unknown_user,unknown_repo,JavaConcepts.java,309
        |typeReference=Object,unknown_user,unknown_repo,JavaConcepts.java,372
        |typeReference=Object,unknown_user,unknown_repo,JavaConcepts.java,382
        |typeReference=RuntimeException,unknown_user,unknown_repo,JavaConcepts.java,320
        |typeReference=RuntimeException,unknown_user,unknown_repo,JavaConcepts.java,336
        |typeReference=RuntimeException,unknown_user,unknown_repo,JavaConcepts.java,355
        |typeReference=Serializable,unknown_user,unknown_repo,JavaConcepts.java,131
        |typeReference=Serializable,unknown_user,unknown_repo,JavaConcepts.java,14
        |typeReference=Serializable,unknown_user,unknown_repo,JavaConcepts.java,363
        |typeReference=Serializable,unknown_user,unknown_repo,JavaConcepts.java,399
        |typeReference=T,unknown_user,unknown_repo,JavaConcepts.java,171
        |typeReference=T,unknown_user,unknown_repo,JavaConcepts.java,173
        |typeReference=T,unknown_user,unknown_repo,JavaConcepts.java,369
        |typeReference=X,unknown_user,unknown_repo,JavaConcepts.java,186
        |typeReference=X,unknown_user,unknown_repo,JavaConcepts.java,369
        |typeReference=XXX,unknown_user,unknown_repo,JavaConcepts.java,363
        |typeReference=com.github.javaparser.JavaParser,unknown_user,unknown_repo,JavaConcepts.java,3
        |typeReference=com.github.javaparser.ast.CompilationUnit,unknown_user,unknown_repo,JavaConcepts.java,6
        |typeReference=japa.parser.ParseException,unknown_user,unknown_repo,JavaConcepts.java,5
        |typeReference=java.io,unknown_user,unknown_repo,JavaConcepts.java,9
        |typeReference=java.util,unknown_user,unknown_repo,JavaConcepts.java,10
        |typeReference=org.junit.Ignore,unknown_user,unknown_repo,JavaConcepts.java,7
        |typeReference=teste.QWE,unknown_user,unknown_repo,JavaConcepts.java,267
        |""".stripMargin
    )
  }
}
