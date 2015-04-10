package devsearch.features

import devsearch.ast._
import org.apache.spark.rdd.RDD

case class FunctionName(position: CodeFilePosition, name: String) extends Feature(position) {
  def key: String = "function name = " + name
}

case class ArgumentName(position: CodeFilePosition, name: String) extends Feature(position) {
  def key: String = "function argument name = " + name
}

case class AbstractFunction(position: CodeFilePosition) extends Feature(position) {
  def key: String = "abstract function"
}

case class OverridingFunction(position: CodeFilePosition) extends Feature(position) {
  def key: String = "overriding function"
}

case class ThrowsException(position: CodeFilePosition, exception: String) extends Feature(position) {
  def key: String = "throws = " + exception
}

object FunDefExtractor extends FeatureExtractor {
  def extract(data: CodeFileData): Traversable[Feature] = data.ast.collect {
    case fd @ FunctionDef(mods, name, annotations, _, params, _, _) =>
      // extract function name feature
      (if (name != Names.DEFAULT) Set(FunctionName(data.location at fd.pos, name)) else Set.empty[Feature]) ++
      // extract function parameter names features
      params.collect { case vd if vd.name != Names.DEFAULT => ArgumentName(data.location at vd.pos, vd.name) }.toSet ++
      // extract thrown exception type features
      annotations.collect { case a @ Annotation(Names.THROWS_ANNOTATION, throws) =>
        throws.values.collect { case Ident(n) if n != Names.DEFAULT => ThrowsException(data.location at a.pos, n) }
      }.flatten.toSet ++
      // extract whether method is overiding feature
      annotations.collect { case a @ Annotation(Names.OVERRIDE_ANNOTATION, _) =>
        OverridingFunction(data.location at a.pos)
      }.toSet ++
      // extract whether function is abstract feature
      (if (mods.isAbstract) Set(AbstractFunction(data.location at fd.pos)) else Set.empty[Feature])

    case _ => Set.empty[Feature]
  }
}

