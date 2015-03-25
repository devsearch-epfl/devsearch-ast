package devsearch.utils

import devsearch.ast.AST

object Traverser {
  def apply[U](f: AST => Iterable[U])(a: Any): Iterable[U] = {
    val headList = a match {
      case a: AST => f(a)
      case _ => List()
    }
    val childrenList = a match {
      case a: Traversable[_] =>
        a.view.flatMap(apply(f))
      case a: Product =>
        a.productIterator.flatMap(apply(f))
      case _ => List()
    }
    headList ++ childrenList
  }
}
