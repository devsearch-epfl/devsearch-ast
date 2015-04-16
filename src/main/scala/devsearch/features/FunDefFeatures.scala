package devsearch.features

import devsearch.ast._

case class FunctionName(position: CodeFilePosition, name: String) extends Feature(position) {
  def key: String = "functionName=" + name
}

case class ArgumentName(position: CodeFilePosition, name: String) extends Feature(position) {
  def key: String = "argumentName=" + name
}

case class ParametricFunction(position: CodeFilePosition) extends Feature(position) {
  def key: String = "function is parametric"
}

case class AbstractFunction(position: CodeFilePosition) extends Feature(position) {
  def key: String = "abstractFunction"
}

case class OverridingFunction(position: CodeFilePosition) extends Feature(position) {
  def key: String = "overridingFunction"
}

case class ThrowsException(position: CodeFilePosition, exception: String) extends Feature(position) {
  def key: String = "throwsException=" + exception
}

object FunDefFeatures extends FeatureExtractor {
  def extract(data: CodeFileData) = data.ast.collect[Feature] {
    case fd @ FunctionDef(mods, name, annotations, tparams, params, _, _) =>
      // extract function name feature
      (if (name != Names.DEFAULT) Set(FunctionName(data.location at fd.pos, name)) else Set.empty) ++
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
      (if (mods.isAbstract) Set(AbstractFunction(data.location at fd.pos)) else Set.empty) ++
      // extract whether functions is parametric, i.e. has type parameters
      (if (tparams.nonEmpty) Set(ParametricFunction(data.location at fd.pos)) else Set.empty)

    case _ => Set.empty
  }
}

