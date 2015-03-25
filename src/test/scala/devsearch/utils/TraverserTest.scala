package devsearch.utils

import devsearch.ast._
import org.scalatest._

class TraverserTest extends FlatSpec with Matchers {
  "Traverser" should "visit all subnodes and leaves which are AST" in {
    val ast = Block(List(If(Ident("aaa"), Return(Empty.NoExpr), Empty.NoStmt)))
    assert(Traverser(List(_))(ast) ==
      List(ast, ast.statements.head,
        Ident("aaa"), Return(Empty.NoExpr), Empty.NoExpr, Empty.NoStmt))
  }
}
