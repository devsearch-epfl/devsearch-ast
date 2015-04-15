package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ClassDefFeaturesTest extends FlatSpec with CodeProvider {

  "classDef extractor" should "work in JavaConcepts.java" in {
    assert(ClassDefFeatures.extract(code) == Set(
      ClassName(location at 399, "XXX"),
      ClassName(location at 185, "Y"),
      ClassName(location at 393, "Base"),
      ClassName(location at 12,  "JavaConcepts"),
      ClassName(location at 59,  "Ugly"),
      ClassName(location at 363, "A"),
      ClassName(location at 208, "QWE"),
      ClassName(location at 175, "X"),
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
    val features = ClassDefFeatures.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
