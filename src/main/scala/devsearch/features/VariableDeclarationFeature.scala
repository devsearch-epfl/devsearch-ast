package devsearch.features

import devsearch.ast.{ValDef, Import, Operators}
import org.apache.spark.rdd.RDD

case class VariableDeclarationFeature(codeLocation: CodeFileLocation, variableType: String, variableName: String)
    extends AbstractFeature(codeLocation)

object VariableDeclarationExtractor extends AbstractFeatureExtractor {
    def extract(codeFileData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        codeFileData.flatMap { codeFile =>
            Operators.collect[VariableDeclarationFeature] {
                case ValDef(modifiers, name, annotations, tpe, rhs, varArgs) =>
                    // TODO(pwalch, mateusz, julien): extract variable declaration
                    Set(VariableDeclarationFeature(codeFile.codeFileLocation, "", ""))
                case _ => Set()
            }(codeFile.syntaxTree)
        }
    }
}