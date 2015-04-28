package devsearch.normalized

object Normalizer
  extends AstExtraction
     with ScopingRenamer
     with SingleAssignment
     with ControlFlowGraphs {

  def apply(ast: devsearch.ast.AST): Definition = ast match {
    case stmt: devsearch.ast.Statement =>
      val (scopingNamer, scoped: devsearch.ast.Statement) = scopeNames(stmt)
      val definition = extract(scoped)
      val (singleNamer, ssa) = singleAssignment(definition)
      ssa
    case _ => new Definition("$empty")
  }
}
