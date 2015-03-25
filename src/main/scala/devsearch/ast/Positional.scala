package devsearch.ast

/**
 * Copied over from https://github.com/scala/scala/blob/v2.10.4/src/library/scala/util/parsing/input/Positional.scala
 * We redefine this here to make sure the positions we use are our positions
 */
trait Positional {
  private var _pos: Position = NoPosition
  def pos: Position = _pos

  def setPos(newpos: Position): this.type = {
    if (_pos eq NoPosition) _pos = newpos
    this
  }
}

/**
 * A general position type that gives point-like position in a given source file.
 *
 * @see [[Source]]
 */
trait Position extends java.io.Serializable {
  def source: Source
  def line: Int
  def col: Int
}

/** Empty position for initializing node positions */
case object NoPosition extends Position {
  val source = NoSource
  val line = 0
  val col = 0
}

/** Concrete point-like position */
case class SimplePosition(source: Source, line: Int, col: Int) extends Position

/** Range position that gives both start and end-points to a position */
case class RangePosition(source: Source, start: (Int, Int), end: (Int, Int)) extends Position {
  val (line, col) = start
}
