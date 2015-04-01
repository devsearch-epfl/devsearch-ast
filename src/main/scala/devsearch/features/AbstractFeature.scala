package devsearch.features

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

abstract class AbstractFeature(codeFileLocation: CodeFileLocation)

trait AbstractFeatureExtractor {
    def extract(sparkContext: SparkContext, codeFileData: RDD[CodeFileData]): RDD[AbstractFeature]
}
