package devsearch.features

import devsearch.ast.{Position, Import, Operators}
import org.apache.spark.rdd.RDD

case class ImportFeature(codeLocation: CodeFileLocation,
                         inFilePosition: InFilePosition,
                         domain: String,
                         containsAsterisk: Boolean,
                         isStatic: Boolean)
    extends AbstractFeature(codeLocation, inFilePosition)

object ImportExtractor extends AbstractFeatureExtractor {
    def extract(codeFileData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        codeFileData.flatMap { codeFile =>
            Operators.collect[ImportFeature] {
                case importStatement: Import =>
                    Set(ImportFeature(codeFile.codeFileLocation,
                                      InFilePosition(importStatement.pos.line, importStatement.pos.col),
                                      importStatement.name, importStatement.asterisk, importStatement.static))

                case _ => Set()
            }(codeFile.syntaxTree)
        }
    }
}
