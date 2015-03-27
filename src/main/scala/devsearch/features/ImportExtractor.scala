package devsearch.features

import devsearch.ast.{AST, Import, Operators}
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * Created by pierre on 27/03/15.
 */
object ImportExtractor extends FeatureExtractor {
    override def extractFeatures(ast: AST): JsArray = {
        Operators.collect[JsValue] {
            case Import(name, asterisk, static) =>
                makeJsSet(name, asterisk, static)
            case _ => Set()
        }(ast).toJson.asInstanceOf[JsArray]
    }

    private def makeJsSet(domain: String, containsAsterisk: Boolean, isStatic: Boolean): Set[JsValue] = {
        Set[JsValue](
            Map(
                ("type", "import"),
                ("domain", domain.toString),
                ("containsAsterisk", containsAsterisk.toString),
                ("isStatic", isStatic.toString)
            ).toJson
        )
    }
}
