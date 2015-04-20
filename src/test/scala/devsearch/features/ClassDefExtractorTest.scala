package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ClassDefExtractorTest extends FlatSpec with CodeProvider {

  "classDef extractor" should "work in JavaConcepts.java" in {
    assert(ClassDefExtractor.extract(code) == Set(
      ClassNameFeature(location at 399, "XXX"),
      ClassNameFeature(location at 185, "Y"),
      ClassNameFeature(location at 393, "Base"),
      ClassNameFeature(location at 12,  "JavaConcepts"),
      ClassNameFeature(location at 59,  "Ugly"),
      ClassNameFeature(location at 363, "A"),
      ClassNameFeature(location at 208, "QWE"),
      ClassNameFeature(location at 175, "X"),
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
    val features = ClassDefExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
