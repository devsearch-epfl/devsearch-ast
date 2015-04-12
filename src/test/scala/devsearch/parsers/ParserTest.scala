package devsearch.ast

trait ParserTest {
  def checkPositions(ast: AST): Unit = {
    val noPositions = ast.collect {
      case u: Unassignable => Set.empty[AST]
      case ast if ast.pos == NoPosition => Set(ast)
      case _ => Set.empty[AST]
    }

    assert(noPositions.isEmpty, noPositions.mkString("\n"))
  }
}
