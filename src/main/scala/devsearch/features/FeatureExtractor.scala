package devsearch.features

import devsearch.ast.AST
import spray.json.JsArray

trait FeatureExtractor {
  def extractFeatures(ast: AST): JsArray
}
