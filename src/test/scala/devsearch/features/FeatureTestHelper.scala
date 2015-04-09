package devsearch.features

import devsearch.parsers.JavaParser
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object FeatureTestHelper {

    // disable spark info-level logging during tests
    // XXX: some INFO-level logs don't seem to want to disappear... Oh well...
    org.apache.log4j.Logger.getLogger("org").setLevel(org.apache.log4j.Level.WARN)
    org.apache.log4j.Logger.getLogger("akka").setLevel(org.apache.log4j.Level.WARN)

    val codeFileLocation = CodeFileLocation("unknown_repo", "unknown_user", "JavaConcepts.java")

    private def generateSingleCodeFileData(): CodeFileData = {
        val fileURL = getClass.getResource("/samples/JavaConcepts.java")
        val filePath = new java.io.File(fileURL.toURI).getAbsolutePath

        CodeFileData(codeFileLocation, JavaParser.parse(filePath))
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

    def withSampleCodeData[T](f: RDD[CodeFileData] => T): T = {
      val conf = new SparkConf().setAppName("featureTest").setMaster("local[2]")

      val sc = new SparkContext(conf)
      val res = f(sc.parallelize(List(generateSingleCodeFileData())))
      sc.stop()
      res
    }
}
