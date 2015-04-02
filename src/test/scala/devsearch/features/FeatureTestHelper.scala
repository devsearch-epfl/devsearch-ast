package devsearch.features

import devsearch.parsers.JavaParser
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object FeatureTestHelper {
    val sc = new SparkContext(new SparkConf().setAppName("featureTest").setMaster("local"))

    private def generateSingleCodeFileData(): CodeFileData = {
        val fileURL = getClass.getResource("/samples/AccountDataManager.java")
        val filePath = new java.io.File(fileURL.toURI).getAbsolutePath

        val codeFileLocation = new CodeFileLocation("github", "android", "AccountDataManager.java")
        new CodeFileData(codeFileLocation, JavaParser.parse(filePath))
    }

    // TODO(julien) Compilation warning in this method
//    def convertImportsToTreeString(tree: AST) = {
//        def importsToJSON(imports : List[List[String]]) : String = {
//            def importHierarchy(list :List[List[String]]): Map[String, Any] = list match{
//                case List(Nil) =>
//                    Map[String, Any]()
//                case a : List[List[String]] =>
//                    a groupBy(_.head) mapValues (x =>x map (x => x.tail)) mapValues (importHierarchy)
//            }
//
//            importHierarchy(imports).toString()
//        }
//
//        def matching(x : Any) : String= x match{
//            case x: List[AST] => matching(x.head)
//            case x: PackageDef => importsToJSON(x.imports.map(x => (x.name.split("\\.")).toList))
//        }
//        matching(Traverser(List(_))(tree).toList)
//    }

    def getSampleCodeData(): RDD[CodeFileData] = {
        sc.parallelize(List(generateSingleCodeFileData()))
    }
}
