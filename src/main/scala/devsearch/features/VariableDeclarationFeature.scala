package devsearch.features

import devsearch.ast._
import org.apache.spark.rdd.RDD

case class VariableDeclarationFeature(codeLocation: CodeFileLocation, inFilePosition: InFilePosition,
                                      variableType: String, variableName: String)
    extends AbstractFeature(codeLocation, inFilePosition) {
    override def getKey(): String = "variable declaration = " + variableName
}

object VariableDeclarationExtractor extends AbstractFeatureExtractor {

    def convertTypeToString(tpe: Type): String = {
        // TODO(julien, mateusz): need to integrate as many types as possible, and make current strings more readable
        tpe match {
            case classType: ClassType => classType.name
            case primitiveType: PrimitiveType => primitiveType.getClass.getCanonicalName
            case arrayType: ArrayType => "Array[" + convertTypeToString(arrayType.base) + "]"
            case wildcardType: WildcardType => "? wildcard " + wildcardType.subType + " " + wildcardType.superType
            case typeHint: TypeHint => "type hint"
            case _ => "unknown_type"
        }
    }

    override def extract(codeFileData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        codeFileData.flatMap { codeFile =>
            Operators.collect[VariableDeclarationFeature] {
                case valueDefinition: ValDef =>
                    Set(
                        VariableDeclarationFeature(
                            codeFile.codeFileLocation,
                            InFilePosition(valueDefinition.pos.line, valueDefinition.pos.col),
                            convertTypeToString(valueDefinition.tpe), valueDefinition.name
                        )
                    )

                case _ => Set()
            }(codeFile.syntaxTree)
        }
    }
}
