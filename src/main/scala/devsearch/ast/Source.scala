package devsearch.ast

case class SourceCreationError(cause: Throwable) extends Exception("Couldn't create source: " + cause.getMessage, cause)

/**
 * Source
 *
 * Flexible code source base trait that is associated to AST positions. This is useful to
 * refer back to the source code from a position, be it for reporting or feature extraction.
 */
trait Source extends Serializable {
  def path: String
  def contents: Array[Char]

  lazy val lineSizes = {
    def indexes(o: Int, lineSizes: List[Int]): List[Int] = {
      val idx = contents.indexOf(o, '\n')
      if (idx >= 0) indexes(o + 1, (idx - o) :: lineSizes)
      else (contents.length - o) :: lineSizes
    }
    indexes(0, Nil).reverse
  }

  def position(offset: Int) = {
    def extractPosition(offset: Int, line: Int, sizes: List[Int]): (Int, Int) = sizes match {
      case lineSize :: xs =>
        if (offset < lineSize) (line, offset)
        else extractPosition(offset - lineSize, line + 1, xs)
      case _ => (offset, line)
    }
    val (line, col) = extractPosition(offset, 0, lineSizes)
    SimplePosition(this, line, col)
  }

  def position(line: Int, col: Int) = SimplePosition(this, line, col)

  def position(startLine: Int, startCol: Int, endLine: Int, endCol: Int) =
    RangePosition(this, (startLine, startCol), (endLine, endCol))

  def toStream = new java.io.ByteArrayInputStream(contents.map(_.toByte))
}

case object NoSource extends Source {
  val path = "NoSource"
  val contents = new Array[Char](0)
}

class FileSource(val path: String) extends Source {
  val contents = try {
    scala.io.Source.fromFile(path).toArray
  } catch {
    case io: java.io.IOException => throw SourceCreationError(io)
  }
}

object ContentsSource {
  private val random = new java.util.Random
  private val tmpDir = System.getProperty("java.io.tmpdir") + "/"
  private def fileName(path: String) = tmpDir + "devsearch-input-" + random.nextInt(Integer.MAX_VALUE) + "/" + path
}

class ContentsSource(_path: String, _contents: String) extends Source {
  lazy val path = {
    var output : java.io.BufferedWriter = null
    try {
      val fileName = ContentsSource.fileName(_path)
      val file = new java.io.File(fileName)
      file.getParentFile.mkdirs()
      output = new java.io.BufferedWriter(new java.io.FileWriter(file))
      output.write(_contents)
      fileName
    } catch {
      case io: java.io.IOException => throw SourceCreationError(io)
    } finally {
      if (output != null) output.close
    }
  }

  val contents = _contents.toArray
}
