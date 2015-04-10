package devsearch.features

import devsearch.ast._
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

case class CodeFileData(location: CodeFileLocation, ast: AST) extends java.io.Serializable

case class CodeFileLocation(user: String, repoName: String, fileName: String) extends java.io.Serializable {
  def at(pos: Position) = CodeFilePosition(this, pos.line)
  def at(line: Int) = CodeFilePosition(this, line)
}

case class CodeFilePosition(location: CodeFileLocation, line: Int) extends java.io.Serializable

/**
 * Entry point for Spark feature mining script.
 */
object CodeEater {
    /**
     * Eats code and returns distinct features (no duplicates)
     */
    def eat(sc: SparkContext, inputData: RDD[CodeFileData]): RDD[Feature] = {
        val emptyRDD: RDD[Feature] = sc.emptyRDD[Feature]
        List[FeatureExtractor](
            ImportExtractor,
            InheritanceExtractor,
            ValDefExtractor,
            FunDefExtractor
        ).foldLeft(emptyRDD)(
            (acc, extractor) =>
              sc.union(acc, inputData.flatMap(data => extractor.extract(data)).distinct)
        )
    }
}
