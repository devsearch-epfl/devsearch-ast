package devsearch.features

import devsearch.ast.{ValDef, Operators, Position}
import org.apache.spark.rdd.RDD

case class VariableDeclarationFeature(codeLocation: CodeFileLocation, inFilePosition: InFilePosition,
                                      variableType: String, variableName: String)
    extends AbstractFeature(codeLocation, inFilePosition) {
    override def getKey(): String = "variable declaration = " + variableName
}

object VariableDeclarationExtractor extends AbstractFeatureExtractor {
    def extract(codeFileData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        codeFileData.flatMap { codeFile =>
            Operators.collect[VariableDeclarationFeature] {
                case valueDefinition: ValDef =>
                    // TODO(pwalch, mateusz, julien): extract variable declaration
                    Set(VariableDeclarationFeature(codeFile.codeFileLocation,
                                                   InFilePosition(valueDefinition.pos.line, valueDefinition.pos.col),
                                                   "", ""))
                case _ => Set()
            }(codeFile.syntaxTree)
        }
    }
}
