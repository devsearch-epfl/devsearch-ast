package devsearch.features

import devsearch.ast._
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

/**
 * Entry point for Spark feature mining script.
 */
object CodeEater {
  /**
   * Eats code and returns distinct features (no duplicates)
   */
  def eat(sc: SparkContext, inputData: RDD[CodeFileData]): RDD[Feature] = {
    inputData.flatMap(Features)
  }
}
