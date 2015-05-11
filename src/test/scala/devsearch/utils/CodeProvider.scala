package devsearch.utils

import devsearch.ast._
import devsearch.features._
import devsearch.parsers._

trait CodeProvider {
  @inline def absResourcePath(path: String): String = CodeProvider.absResourcePath(path)
  @inline def sampleCode(fileName: String): CodeFile = CodeProvider.sampleCode(fileName)
  lazy val location = CodeProvider.location
  lazy val code = CodeProvider.code
}

object CodeProvider {

  def absResourcePath(path: String): String = {
    val fileURL = getClass.getResource(path)
    new java.io.File(fileURL.toURI).getAbsolutePath
  }

  def sampleCode(fileName: String): CodeFile = {
    val filePath = absResourcePath("/samples/" + fileName)
    Languages.guess(fileName) match {
      case Some(language) =>
        val location = CodeFileLocation("unknown_user", "unknown_repo", fileName)
        CodeFile(language, location, new FileSource(filePath))

      case None => sys.error("Unknown extension for filename: " + fileName)
    }
  }

  lazy val location = CodeFileLocation("unknown_user", "unknown_repo", "JavaConcepts.java")

  lazy val code = sampleCode("JavaConcepts.java")
}
