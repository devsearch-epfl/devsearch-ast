package devsearch.features

import devsearch.parsers.JavaParser
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD

object FeatureTestHelper {
    val sc = new SparkContext(new SparkConf().setAppName("featureTest").setMaster("local"))

    private def generateSingleCodeFileData(): CodeFileData = {
        val fileURL = getClass.getResource("/samples/AccountDataManager.java")
        val filePath = new java.io.File(fileURL.toURI).getAbsolutePath

        val codeFileLocation = new CodeFileLocation("github", "android", "AccountDataManager.java")
        new CodeFileData(codeFileLocation, JavaParser.parse(filePath))
    }

    def getSampleCodeData(): RDD[CodeFileData] = {
        sc.parallelize(List(generateSingleCodeFileData()))
    }
}
