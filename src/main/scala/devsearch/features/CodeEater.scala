package devsearch.features

import devsearch.ast.AST
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

case class CodeFileLocation(user: String, repoName: String, fileName: String)
case class CodeFileData(codeFileLocation: CodeFileLocation, syntaxTree: AST)

object CodeEater {
    def eat(sc: SparkContext, inputData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        val emptyRDD: RDD[AbstractFeature] = sc.emptyRDD[AbstractFeature]
        List[AbstractFeatureExtractor](ImportExtractor).foldLeft(emptyRDD)(
            (acc, extractor) =>
                sc.union(acc, extractor.extract(inputData))
        )
    }
}
