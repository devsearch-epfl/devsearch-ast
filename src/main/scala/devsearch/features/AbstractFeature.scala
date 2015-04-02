package devsearch.features

import org.apache.spark.rdd.RDD

case class InFilePosition(line: Int, column: Int)
abstract class AbstractFeature(codeFileLocation: CodeFileLocation, inFilePosition: InFilePosition) extends Serializable {
    def getKey(): String
}

trait AbstractFeatureExtractor {
    def extract(codeFileData: RDD[CodeFileData]): RDD[AbstractFeature]
}
