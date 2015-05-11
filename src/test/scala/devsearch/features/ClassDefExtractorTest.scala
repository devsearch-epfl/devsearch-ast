package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ClassDefExtractorTest extends FlatSpec with CodeProvider {

  "classDef extractor" should "work in JavaConcepts.java" in {
    assert(ClassDefExtractor.extract(code) == Set(
      ClassNameFeature(location at 398, "XXX"),
      ClassNameFeature(location at 184, "Y"),
      ClassNameFeature(location at 392, "Base"),
      ClassNameFeature(location at 11,  "JavaConcepts"),
      ClassNameFeature(location at 58,  "Ugly"),
      ClassNameFeature(location at 362, "A"),
      ClassNameFeature(location at 207, "QWE"),
      ClassNameFeature(location at 174, "X"),
      InheritanceFeature(location at 362, "A", "XXX"),
      InheritanceFeature(location at 184, "Y", "X"),
      InheritanceFeature(location at 398, "XXX", "Serializable"),
      InheritanceFeature(location at 11,  "JavaConcepts", "Base"),
      InheritanceFeature(location at 11,  "JavaConcepts", "Serializable"),
      InheritanceFeature(location at 398, "XXX", "Cloneable"),
      InheritanceFeature(location at 362, "A", "Serializable"),
      InheritanceFeature(location at 207, "QWE", "JavaConcepts")
    ))
  }

  it should "toString correctly" in {
    val features = ClassDefExtractor.extract(code)
    assert(features == features.map { f => Feature.parse(f.encode) })
  }
}
