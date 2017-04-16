package devsearch.features

import devsearch.ast._

case class ClassNameFeature(position: CodePiecePosition, name: String) extends Feature(position) {
  def key: String = "className=" + name
  override def toNiceString: String = "Class Name '" + name + "'"
}

case class InheritanceFeature(position: CodePiecePosition, className: String, superClassName: String) extends Feature(position) {
  def key: String = "inheritance=" + className + " from=" + superClassName
  override def toNiceString: String = "'"+ className +"' inherits from '" + superClassName + "'"
}

object ClassDefExtractor extends FeatureExtractor {
  def extract(data: CodeFile): Set[Feature] = data.ast.collect[Feature] {
    case cd @ ClassDef(_, name, _, _, _, _, _) if name != Names.DEFAULT =>
      try {
        Set(ClassNameFeature(data.location at cd.pos, name)) ++
            cd.superClasses.collect { case ClassType(_, superClass, _, _) if superClass != Names.DEFAULT =>
              InheritanceFeature(data.location at cd.pos, cd.name, superClass)
            }.toSet
      } catch {
        case cce: ClassCastException =>
          // println("Class cast exception at:" + (data.location at cd.pos))
          Set.empty
      }

    case _ => Set.empty
  }
}

