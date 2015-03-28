package devsearch.blobToAST

import devsearch.ast.{StringSource, ContentsSource, AST}
import devsearch.parsers._
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import scala.util.parsing.combinator._


/**
 * Created by hubi on 3/27/15.
 */
abstract class BlobSnippet extends java.io.Serializable
  case class JavaFile(size: String, owner: String, repository: String, path: String, code: String)
    extends BlobSnippet
  case class PythonFile(size: String, owner: String, repository: String, path: String, code: String)
    extends BlobSnippet
  case class GoFile(size: String, owner: String, repository: String, path: String, code: String)
    extends BlobSnippet
  case class JavaScriptFile(size: String, owner: String, repository: String, path: String, code: String)
    extends BlobSnippet
  case class UnknownFile() extends BlobSnippet


object BlobParser extends RegexParsers with java.io.Serializable {
  def parseBlob: Parser[BlobSnippet] = (
    number~":Java/"~noSlash~"/"~noSlash~"/"~path~code ^^ {//~code ^^ {
      case size~_~owner~_~repo~_~path~code => JavaFile(size, owner, repo, path, code)
    }
    |number~":Python/"~noSlash~"/"~noSlash~"/"~path~"\n"~code ^^ {
      case size~_~owner~_~repo~_~path~_~code => PythonFile(size, owner, repo, path, code)
    }
    |number~":Go/"~noSlash~"/"~noSlash~"/"~path~"\n"~code ^^ {
      case size~_~owner~_~repo~_~path~_~code => GoFile(size, owner, repo, path, code)
    }
    |number~":JavaScript/"~noSlash~"/"~noSlash~"/"~path~"\n"~code ^^ {
      case size~_~owner~_~repo~_~path~_~code => JavaScriptFile(size, owner, repo, path, code)
    }
  )

  val number:  Parser[String] = """\d+""".r
  val noSlash: Parser[String] = """[^/]+""".r
  val path:    Parser[String] = """[^\n]+""".r                     //"[/[^/]+]+\\.[a-zA-Z]+[^\n]".r
  val code:    Parser[String] = """(?s).*[^\n\d+:]""".r              //TODO: eof [^(\n\d+:)]+
  val anything:Parser[String] = """.*""".r
  //val code:    Parser[String] = "(.*[\n]*)*[^(\n\\d+:)]".r   //everything until "\n897162346:"
}




/* REPL helpers...

def matchString(s: String, r: scala.util.matching.Regex): Boolean = s match{
     case r() => true
     case _   => false
}


def showMatches(s: String, r: scala.util.matching.Regex): Unit = {
     for (m <- r.findAllIn(s)) println (m+"\n-------------------------------------------------")
}
 */





object ASTExtractor {
  val sc = new SparkContext(new SparkConf().setAppName("ASTExtractor").setMaster("local[4]"))


  def snipBlob(blob: (String, String)): List[String] = {
    val snippet = """(?s).+?(?=(\n\d+:|\Z))""".r    //match everything until some "28764:" or end of string
    blob match {
      case (path, content) => snippet.findAllIn(content).toList
      case _               => List()
    }
  }


  def toBlobSnippet(snippet: String): BlobSnippet = {
    BlobParser.parse(BlobParser.parseBlob, snippet).getOrElse(UnknownFile())
  }


  /*
   * TODO: Add more parsers here
   */
  def toAST(snippet: BlobSnippet): (String, String, String, String, Int, AST) = snippet match{
    case JavaFile(size, owner, repo, path, code)
      => (owner, repo, path, "Java", size.toInt, JavaParser.parse(new StringSource(code)))
    case _
      => null

  }


  def parseTestString(s: String): BlobSnippet = {
    BlobParser.parse(BlobParser.parseBlob, s).getOrElse(UnknownFile())
  }




  /*
   * Argument: a path that leads to the language directories
   */
  def extract(path: String): RDD[(String, String, String, String, Int, AST)] = {
    // type: RDD(path: String, file: String)
    val rddBlobs = sc.wholeTextFiles(path+"/Java")


    //TODO: check if path is valid!
    //TODO: uncompress files
    rddBlobs flatMap snipBlob map toBlobSnippet map toAST
  }
}