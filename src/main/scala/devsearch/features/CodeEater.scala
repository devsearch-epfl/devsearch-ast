package devsearch.features

import devsearch.ast.AST
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

case class CodeFileLocation(user: String, repoName: String, fileName: String) extends Serializable
case class CodeFileData(codeFileLocation: CodeFileLocation, syntaxTree: AST) extends Serializable

object CodeEater {
    def eat(sc: SparkContext, inputData: RDD[CodeFileData]): RDD[AbstractFeature] = {
        val emptyRDD: RDD[AbstractFeature] = sc.emptyRDD[AbstractFeature]
        List[AbstractFeatureExtractor](
            ImportExtractor,
            InheritanceExtractor,
            VariableDeclarationExtractor
        ).foldLeft(emptyRDD)(
            (acc, extractor) =>
                sc.union(acc, extractor.extract(inputData))
        )
    }
}
