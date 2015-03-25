package devsearch.ast

/**
 * Source
 *
 * Flexible code source base trait that is associated to AST positions. This is useful to
 * refer back to the source code from a position, be it for reporting or feature extraction.
 */
trait Source {
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
  val contents = scala.io.Source.fromFile(path).toArray
}

class ContentsSource(str: String) extends Source {
  val path = "$$memorySource"
  val contents = str.toArray
}
