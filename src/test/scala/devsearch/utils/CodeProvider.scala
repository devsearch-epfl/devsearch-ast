package devsearch.utils

import devsearch.features._
import devsearch.parsers.JavaParser

trait CodeProvider {
  lazy val location = CodeProvider.location
  lazy val code = CodeProvider.code
}

object CodeProvider {

  lazy val location = CodeFileLocation("unknown_user", "unknown_repo", "JavaConcepts.java")

  lazy val code = {
    val fileURL = getClass.getResource("/samples/JavaConcepts.java")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath

    CodeFileData(42, "unknown_language", location, JavaParser.parse(filePath))
  }
}
