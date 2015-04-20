package devsearch.features

import devsearch.ast._

case class ClassNameFeature(position: CodeFilePosition, name: String) extends Feature(position) {
  def key: String = "className=" + name
}

case class InheritanceFeature(position: CodeFilePosition, className: String, superClassName: String) extends Feature(position) {
  def key: String = "inheritance=" + className + " from=" + superClassName
}

object ClassDefExtractor extends FeatureExtractor {
  def extract(data: CodeFileData): Set[Feature] = data.ast.collect[Feature] {
    case cd @ ClassDef(_, name, _, _, _, _, _) if name != Names.DEFAULT =>
      Set(ClassNameFeature(data.location at cd.pos, name)) ++
      cd.superClasses.collect { case ClassType(_, superClass, _, _) if superClass != Names.DEFAULT =>
        InheritanceFeature(data.location at cd.pos, cd.name, superClass)
      }.toSet
    case _ => Set.empty
  }
}

