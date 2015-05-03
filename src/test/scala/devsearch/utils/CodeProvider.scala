package devsearch.utils

import devsearch.ast._
import devsearch.features._
import devsearch.parsers._

trait CodeProvider {
  @inline def absResourcePath(path: String): String = CodeProvider.absResourcePath(path)
  @inline def sampleCode(fileName: String): CodeFileData = CodeProvider.sampleCode(fileName)
  lazy val location = CodeProvider.location
  lazy val code = CodeProvider.code
}

object CodeProvider {

  def absResourcePath(path: String): String = {
    val fileURL = getClass.getResource(path)
    new java.io.File(fileURL.toURI).getAbsolutePath
  }

  def sampleCode(fileName: String): CodeFileData = {
    val filePath = absResourcePath("/samples/" + fileName)
    val parser = fileName.substring(fileName.lastIndexOf(".") + 1, fileName.length) match {
      case "java" => JavaParser
      case "js" => JsParser
      case "scala" => QueryParser
      case "go" => GoParser
      case ext => sys.error("Unknown extension: " + ext)
    }

    val source = new FileSource(filePath)
    val location = CodeFileLocation("unknown_user", "unknown_repo", fileName)
    CodeFileData(source.contents.length, parser.language, location, parser.parse(source))
  }

  lazy val location = CodeFileLocation("unknown_user", "unknown_repo", "JavaConcepts.java")

  lazy val code = sampleCode("JavaConcepts.java")
}
