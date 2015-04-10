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
}
