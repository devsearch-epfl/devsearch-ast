package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class InheritanceFeaturesTest extends FlatSpec with CodeProvider {
  "inheritance extractor" should "extract all class extensions and interface implementations" in {
    assert(InheritanceFeatures.extract(code).toSet == Set(
      InheritanceFeature(location at 363, "A", "XXX"),
      InheritanceFeature(location at 185, "Y", "X"),
      InheritanceFeature(location at 399, "XXX", "Serializable"),
      InheritanceFeature(location at 12,  "JavaConcepts", "Base"),
      InheritanceFeature(location at 12,  "JavaConcepts", "Serializable"),
      InheritanceFeature(location at 399, "XXX", "Cloneable"),
      InheritanceFeature(location at 363, "A", "Serializable"),
      InheritanceFeature(location at 208, "QWE", "JavaConcepts")
    ))
  }


  it should "toString correctly" in {
    assert(InheritanceFeatures.extract(code).toList.map(_.toString).sortBy(e => e).foldLeft("")((acc, curr) => acc + curr+"\n") ==
      """inheritance=A from=Serializable,unknown_user,unknown_repo,JavaConcepts.java,363
        |inheritance=A from=XXX,unknown_user,unknown_repo,JavaConcepts.java,363
        |inheritance=JavaConcepts from=Base,unknown_user,unknown_repo,JavaConcepts.java,12
        |inheritance=JavaConcepts from=Serializable,unknown_user,unknown_repo,JavaConcepts.java,12
        |inheritance=QWE from=JavaConcepts,unknown_user,unknown_repo,JavaConcepts.java,208
        |inheritance=XXX from=Cloneable,unknown_user,unknown_repo,JavaConcepts.java,399
        |inheritance=XXX from=Serializable,unknown_user,unknown_repo,JavaConcepts.java,399
        |inheritance=Y from=X,unknown_user,unknown_repo,JavaConcepts.java,185
        |""".stripMargin
    )
  }
}
