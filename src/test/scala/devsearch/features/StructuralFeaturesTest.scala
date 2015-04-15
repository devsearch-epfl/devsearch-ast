package devsearch.features

import devsearch.utils._
import org.scalatest._

class StructuralFeaturesTest extends FlatSpec with CodeProvider {

  "Structural features" should "be found in JavaConcepts.java" in {
    assert(StructuralFeatures.extract(code).toSet == Set(
      ControlFeature(location at 248, "if"),
      ControlFeature(location at 268, "if"),
      ControlFeature(location at 280, "if"),
      ControlFeature(location at 326, "if"),
      ControlFeature(location at 282, "if"),
      ControlFeature(location at 276, "if"),
      ControlFeature(location at 315, "if"),
      ControlFeature(location at 333, "if"),
      ControlFeature(location at 301, "for"),
      ControlFeature(location at 297, "for"),
      ControlFeature(location at 294, "foreach"),
      ControlFeature(location at 286, "while"),
      ControlFeature(location at 287, "while"),
      ControlFeature(location at 290, "do"),
      ControlFeature(location at 293, "do")
    ))
  }


  it should "toString correctly" in {
    assert(StructuralFeatures.extract(code).toList.map(_.toString).sortBy(e => e).foldLeft("")((acc, curr) => acc + curr+"\n") ==
      """controlStatement=do,unknown_user,unknown_repo,JavaConcepts.java,290
        |controlStatement=do,unknown_user,unknown_repo,JavaConcepts.java,293
        |controlStatement=for,unknown_user,unknown_repo,JavaConcepts.java,297
        |controlStatement=for,unknown_user,unknown_repo,JavaConcepts.java,301
        |controlStatement=foreach,unknown_user,unknown_repo,JavaConcepts.java,294
        |controlStatement=if,unknown_user,unknown_repo,JavaConcepts.java,248
        |controlStatement=if,unknown_user,unknown_repo,JavaConcepts.java,268
        |controlStatement=if,unknown_user,unknown_repo,JavaConcepts.java,276
        |controlStatement=if,unknown_user,unknown_repo,JavaConcepts.java,280
        |controlStatement=if,unknown_user,unknown_repo,JavaConcepts.java,282
        |controlStatement=if,unknown_user,unknown_repo,JavaConcepts.java,315
        |controlStatement=if,unknown_user,unknown_repo,JavaConcepts.java,326
        |controlStatement=if,unknown_user,unknown_repo,JavaConcepts.java,333
        |controlStatement=while,unknown_user,unknown_repo,JavaConcepts.java,286
        |controlStatement=while,unknown_user,unknown_repo,JavaConcepts.java,287
        |""".stripMargin
    )
  }
}
