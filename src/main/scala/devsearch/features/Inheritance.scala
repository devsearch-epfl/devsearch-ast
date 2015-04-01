package devsearch.features

import devsearch.ast._
import spray.json.DefaultJsonProtocol._
import spray.json._

object InheritanceExtractor extends FeatureExtractor {
  override def extractFeatures(ast: AST): JsArray = {
    Operators.collect[JsValue] {
      case ClassDef(modifiers /*Modifiers*/, name /*String*/, annotations /*List[Annotation]*/,
                    tparams /*List[TypeDef]*/, superClasses /*List[ClassType]*/,
                    definitions /*List[Definition]*/, sort /*StructuralSort*/) =>
        makeJsSet(name, superClasses.map(_.name))
      case _ => Set()
    }(ast).toJson.asInstanceOf[JsArray]
  }

  private def makeJsSet(className: String, superClasses: List[String]): Set[JsValue] = superClasses.map(cls =>
    Map(("type", "inheritance"), ("className", className), ("superClassName", cls)).toJson
  ).toSet
}
