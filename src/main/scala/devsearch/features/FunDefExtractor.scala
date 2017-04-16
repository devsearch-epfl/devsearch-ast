package devsearch.features

import devsearch.ast._

case class FunNameFeature(position: CodePiecePosition, name: String) extends Feature(position) {
  def key: String = "functionName=" + name
  override def toNiceString: String = "Function '" + name + "'"
}

case class ArgNameFeature(position: CodePiecePosition, name: String) extends Feature(position) {
  def key: String = "argumentName=" + name
  override def toNiceString: String = "Argument '" + name + "'"
}

case class ParametricFunFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "function is parametric"
  override def toNiceString: String = "Parametric Function"
}

case class AbstractFunFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "abstractFunction"
  override def toNiceString: String = "Abstract Function"
}

case class OverridingFunFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "overridingFunction"
  override def toNiceString: String = "Overriding Function"
}

case class ThrowsFeature(position: CodePiecePosition, exception: String) extends Feature(position) {
  def key: String = "throwsException=" + exception
  override def toNiceString: String = "Throws '"+ exception + "'"
}

object FunDefExtractor extends FeatureExtractor {
  def extract(data: CodeFile) = data.ast.collect[Feature] {
    case fd @ FunctionDef(mods, name, annotations, tparams, params, _, _) =>
      // extract function name feature
      (if (name != Names.DEFAULT) Set(FunNameFeature(data.location at fd.pos, name)) else Set.empty) ++
      // extract function parameter names features
      params.collect { case vd if vd.name != Names.DEFAULT => ArgNameFeature(data.location at vd.pos, vd.name) }.toSet ++
      // extract thrown exception type features
      annotations.collect { case a @ Annotation(Names.THROWS_ANNOTATION, throws) =>
        throws.values.collect { case Ident(n) if n != Names.DEFAULT => ThrowsFeature(data.location at a.pos, n) }
      }.flatten.toSet ++
      // extract whether method is overiding feature
      annotations.collect { case a @ Annotation(Names.OVERRIDE_ANNOTATION, _) =>
        OverridingFunFeature(data.location at a.pos)
      }.toSet ++
      // extract whether function is abstract feature
      (if (mods.isAbstract) Set(AbstractFunFeature(data.location at fd.pos)) else Set.empty) ++
      // extract whether functions is parametric, i.e. has type parameters
      (if (tparams.nonEmpty) Set(ParametricFunFeature(data.location at fd.pos)) else Set.empty)

    case _ => Set.empty
  }
}

