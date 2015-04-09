package devsearch.blobToAST

import devsearch.ast.{StringSource, ContentsSource, AST}
import devsearch.parsers._
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import scala.util.parsing.combinator._


/**
 * Created by hubi on 3/27/15.
 */
abstract class CodeFile extends java.io.Serializable
  case class JavaFile(size: String, owner: String, repository: String, path: String, code: String)
    extends CodeFile
  case class PythonFile(size: String, owner: String, repository: String, path: String, code: String)
    extends CodeFile
  case class GoFile(size: String, owner: String, repository: String, path: String, code: String)
    extends CodeFile
  case class JavaScriptFile(size: String, owner: String, repository: String, path: String, code: String)
    extends CodeFile
  case class UnknownFile() extends CodeFile


object SnippetParser extends RegexParsers with java.io.Serializable {
  def parseBlob: Parser[CodeFile] = (
    number~":Java/"~noSlash~"/"~noSlash~"/"~path~code ^^ {
      case size~_~owner~_~repo~_~path~code => JavaFile(size, owner, repo, path, code)
    }
    |number~":Python/"~noSlash~"/"~noSlash~"/"~path~code ^^ {
      case size~_~owner~_~repo~_~path~code => PythonFile(size, owner, repo, path, code)
    }
    |number~":Go/"~noSlash~"/"~noSlash~"/"~path~code ^^ {
      case size~_~owner~_~repo~_~path~code => GoFile(size, owner, repo, path, code)
    }
    |number~":JavaScript/"~noSlash~"/"~noSlash~"/"~path~code ^^ {
      case size~_~owner~_~repo~_~path~code => JavaScriptFile(size, owner, repo, path, code)
    }
  )

  val number:  Parser[String] = """\d+""".r
  val noSlash: Parser[String] = """[^/]+""".r
  val path:    Parser[String] = """[^\n]+""".r                 //everything until eol
  val code:    Parser[String] = """(?s).*[^\n\d+:]""".r        //everything until "\n897162346:"
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


  def toBlobSnippet(blob: (String, String)): List[String] = blob match {
    case (path, content) => {
      val lines = content.split('\n')
      var ret = List[String]()

      var size = 0
      var snippet = ""
      for(line <- lines) {
        if (size <= 0){

          //add the snippet to the list...
          if(snippet != ""){
            ret = ret:::List(snippet)
          }

          size = (line.split(':')(0)).toInt
          snippet = line

        } else {
          snippet += ("\n" + line)
          size -= (line.length + 1)     //chars on line + newline
        }
      }

      //add the last snippet
      ret = ret:::List(snippet)

      return ret

    }
    case _ => null
  }



  /*def toBlobSnippet(blob: (String, String)): List[String] = {
    val snippet = """(?s).+?(?=(\n\d+:|\Z))""".r    //match everything until some "<NUMBER>:" or end of string
    blob match {
      case (path, content) => snippet.findAllIn(content).toList
      case _               => List()
    }
  }*/


  def toCodeFile(snippet: String): CodeFile = {
    SnippetParser.parse(SnippetParser.parseBlob, snippet).getOrElse(UnknownFile())
  }


  /*
   * TODO: Add more parsers here
   */
  def toAST(snippet: CodeFile): (String, String, String, String, Int, AST) = snippet match{
    case JavaFile(size, owner, repo, path, code)
      => (owner, repo, path, "Java", size.toInt, JavaParser.parse(new StringSource(code)))
    case _
      => null

  }


  def parseTestString(s: String): CodeFile = {
    SnippetParser.parse(SnippetParser.parseBlob, s).getOrElse(UnknownFile())
  }




  /*
   * Argument: a path that leads to the language directories
   */
  def extract(path: String): RDD[(String, String, String, String, Int, AST)] = {
    // type: RDD(path: String, file: String)
    val rddBlobs = sc.wholeTextFiles(path+"/*")


    //TODO: check if path is valid!
    //TODO: uncompress files
    rddBlobs flatMap toBlobSnippet map toCodeFile map toAST
  }
}