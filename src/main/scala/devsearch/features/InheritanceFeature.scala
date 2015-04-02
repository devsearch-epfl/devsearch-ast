package devsearch.features

import devsearch.ast._
import org.apache.spark.rdd.RDD

case class InheritanceFeature(codeLocation: CodeFileLocation,
                              className: String,
                              superClassName: String)
    extends AbstractFeature(codeLocation)

object InheritanceExtractor extends AbstractFeatureExtractor {
    override def extract(codeFileData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        codeFileData.flatMap { codeFile =>
            Operators.collect[InheritanceFeature] {
                case ClassDef(modifiers, name, annotations, tparams, superClasses, definitions, sort) =>
                    superClasses.map(superClass =>
                        InheritanceFeature(codeFile.codeFileLocation, name, superClass.name)
                    ).toSet

                case _ => Set()
            }(codeFile.syntaxTree)
        }
    }
}
