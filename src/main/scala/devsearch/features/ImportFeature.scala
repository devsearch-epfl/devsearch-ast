package devsearch.features

import devsearch.ast.{Import, Operators}
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

case class ImportFeature(codeLocation: CodeFileLocation,
                         domain: String,
                         containsAsterisk: Boolean,
                         isStatic: Boolean)
    extends AbstractFeature(codeLocation)

object ImportExtractor extends AbstractFeatureExtractor {
    override def extract(sc: SparkContext, codeFileData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        codeFileData.flatMap(codeFile =>
            Operators.collect[ImportFeature] {
                case Import(name, asterisk, static) =>
                    Set(new ImportFeature(codeFile.codeFileLocation, name, asterisk, static))

                case _ => Set()
            }(codeFile.syntaxTree)
        )
    }
}
