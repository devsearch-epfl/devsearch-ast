package devsearch.features

import devsearch.ast._
import org.apache.spark.rdd.RDD

case class InheritanceFeature(codeLocation: CodeFileLocation,
                              inFilePosition: InFilePosition,
                              className: String,
                              superClassName: String)
    extends AbstractFeature(codeLocation, inFilePosition) {
    override def getKey(): String = "inheritance = " + className
}

object InheritanceExtractor extends AbstractFeatureExtractor {
    override def extract(codeFileData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        codeFileData.flatMap { codeFile =>
            Operators.collect[InheritanceFeature] {
                case classDefinition: ClassDef =>
                    classDefinition.superClasses.map(superClass =>
                        InheritanceFeature(codeFile.codeFileLocation,
                                           InFilePosition(classDefinition.pos.line, classDefinition.pos.line),
                                           classDefinition.name, superClass.name)
                    ).toSet

                case _ => Set()
            }(codeFile.syntaxTree)
        }
    }
}
