package devsearch.features

import devsearch.ast.{ClassType, ValDef, Operators, PrimitiveType, ArrayType}
import org.apache.spark.rdd.RDD

case class VariableDeclarationFeature(codeLocation: CodeFileLocation, inFilePosition: InFilePosition,
                                      variableType: String, variableName: String)
    extends AbstractFeature(codeLocation, inFilePosition) {
    override def getKey(): String = "variable declaration = " + variableName
}

object VariableDeclarationExtractor extends AbstractFeatureExtractor {
    override def extract(codeFileData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        codeFileData.flatMap { codeFile =>
            Operators.collect[VariableDeclarationFeature] {
                case valueDefinition: ValDef =>
                    // TODO(pwalch, mateusz, julien): extract variable declaration
                    val typeName = valueDefinition.tpe match {
                        case classType: ClassType => classType.name
                        case primitiveType: PrimitiveType => primitiveType.getClass.getCanonicalName
                        case _ => "unknown_type"
                    }
                    Set(VariableDeclarationFeature(codeFile.codeFileLocation,
                                                   InFilePosition(valueDefinition.pos.line, valueDefinition.pos.col),
                                                   typeName, valueDefinition.name))
                case _ => Set()
            }(codeFile.syntaxTree)
        }
    }
}
