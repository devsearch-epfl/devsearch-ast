package devsearch.normalized

/**
 * Transforms an AST into SSA normal form.
 *
 * Uses components in
 * - [[ScopingRenamer]]
 * - [[AstExtraction]]
 * - [[SingleAssignment]]
 * - [[ControlFlowGraphs]]
 * to extract the control-flow graph and transform the AST
 * into normal form.
 */
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
