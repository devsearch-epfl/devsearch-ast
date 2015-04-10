package devsearch.features

import devsearch.ast._
import org.apache.spark.rdd.RDD

case class InheritanceFeature(position: CodeFilePosition, className: String, superClassName: String) extends Feature(position) {
  def key: String = "inheritance = " + className + " from " + superClassName
}

object InheritanceExtractor extends FeatureExtractor {
  def extract(data: CodeFileData) = data.ast.collect {
    case cd: ClassDef if cd.name != Names.DEFAULT =>
      cd.superClasses.collect { case ClassType(_, superClass, _, _) if superClass != Names.DEFAULT =>
        InheritanceFeature(data.location at cd.pos, cd.name, superClass)
      }.toSet

    case _ => Set.empty[InheritanceFeature]
  }
}
