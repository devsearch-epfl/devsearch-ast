package devsearch.features

import devsearch.ast._

case class ClassName(position: CodeFilePosition, name: String) extends Feature(position) {
  def key: String = "className=" + name
}

case class InheritanceFeature(position: CodeFilePosition, className: String, superClassName: String) extends Feature(position) {
  def key: String = "inheritance=" + className + " from=" + superClassName
}

object ClassDefFeatures extends FeatureExtractor {
  def extract(data: CodeFileData): Set[Feature] = data.ast.collect[Feature] {
    case cd @ ClassDef(_, name, _, _, _, _, _) if name != Names.DEFAULT =>
      Set(ClassName(data.location at cd.pos, name)) ++
      cd.superClasses.collect { case ClassType(_, superClass, _, _) if superClass != Names.DEFAULT =>
        InheritanceFeature(data.location at cd.pos, cd.name, superClass)
      }.toSet
    case _ => Set.empty
  }
}

