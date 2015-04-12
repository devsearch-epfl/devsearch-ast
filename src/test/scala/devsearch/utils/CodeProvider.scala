package devsearch.utils

import devsearch.features._
import devsearch.parsers.JavaParser

trait CodeProvider {
  lazy val location = CodeProvider.location
  lazy val code = CodeProvider.code
}

object CodeProvider {

  lazy val location = CodeFileLocation("unknown_repo", "unknown_user", "JavaConcepts.java")

  lazy val code = {
    val fileURL = getClass.getResource("/samples/JavaConcepts.java")
    val filePath = new java.io.File(fileURL.toURI).getAbsolutePath

    CodeFileData(location, JavaParser.parse(filePath))
  }
}
