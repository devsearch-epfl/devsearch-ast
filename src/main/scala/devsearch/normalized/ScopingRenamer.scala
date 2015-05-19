package devsearch.normalized

import devsearch.ast.{Expr => AstExpr, Definition => AstDefinition, _}

/**
 * Freshen name clashes based on scopes
 *
 * It is easier to track scoping information in structural AST form than in
 * control-flow graph form, so we perform some simple name freshening when
 * variables are masked by local scopes on the AST.
 */
trait ScopingRenamer {

  def scopeNames(ast: AST): (Namer, AST) = {
    val namer = new Namer
    val scoped = new RenamingTransformer(namer).apply(ast)
    namer -> scoped
  }

  class RenamingTransformer(namer: Namer) extends Transformer {
    private var context: Map[String,String] = _
    private var inBlock: Boolean = _

    private def saveContext(ast: AST): AST = {
      val savedContext = context
      val newAST = super.transform(ast)
      context = savedContext
      newAST
    }

    private def newContext[T](t: => T): T = {
      val saved = context
      val computed = t
      context = saved
      computed
    }

    private def inBlock[T](in: Boolean)(t: => T): T = {
      val saved = inBlock
      inBlock = in
      val computed = t
      inBlock = saved
      computed
    }

    override def apply(ast: AST): AST = {
      context = Map.empty[String, String]
      inBlock = false

      def maintainFields(defs: List[AstDefinition]): Unit = defs.foreach {
        case ValDef(_, name, _, _, _, _) => namer.maintain(name)
        case FunctionDef(_, name, _, _, _, _, _) => namer.maintain(name)
        case _ =>
      }

      ast.foreach {
        case PackageDef(_, _, _, defs) => maintainFields(defs)
        case ClassDef(_, _, _, _, _, defs, _) => maintainFields(defs)
        case EnumDef(_, _, _, _, defs, _) => maintainFields(defs)
        case EnumConstantDef(_, _, _, defs) => maintainFields(defs)
        case ConstructorCall(_, _, defs) => maintainFields(defs)
        case Try(_, catchs, _) => maintainFields(catchs.map(_._1))
        case _ =>
      }

      transform(ast)
    }

    override protected def transform(ast: AST): AST = ast match {
      case (_: PackageDef) | (_: ClassDef) | (_: AnnotationDef) | (_: EnumDef) =>
        inBlock(false)(super.transform(ast))

      case EnumConstantDef(name, annots, args, members) =>
        val (newAnnots, newArgs) = inBlock(true) {
          val newAnnots = annots.map(transform(_).asInstanceOf[Annotation])
          val newArgs = args.map(transform(_).asInstanceOf[AstExpr])
          (newAnnots, newArgs)
        }
        val newMembers = inBlock(false)(members.map(transform(_).asInstanceOf[AstDefinition]))
        EnumConstantDef(name, newAnnots, newArgs, newMembers).fromAST(ast)

      case _: ValueDefinition if !inBlock => inBlock(true)(super.transform(ast))

      case _: FunctionDef if !inBlock => inBlock(true)(super.transform(ast))

      case ConstructorCall(tpe, args, body) =>
        val (newTpe, newArgs) = inBlock(true) {
          val newTpe = transform(tpe).asInstanceOf[ClassType]
          val newArgs = args.map(transform(_).asInstanceOf[AstExpr])
          (newTpe, newArgs)
        }
        val newBody = inBlock(false)(body.map(transform(_).asInstanceOf[AstDefinition]))
        ConstructorCall(newTpe, newArgs, newBody).fromAST(ast)

      case _ => inBlock(true)(ast match {
        case Block(stmts) => newContext {
          stmts.foreach {
            case fd @ FunctionDef(_, name, _, _, _, _, _) => context += name -> namer.fresh(name)
            case _ =>
          }
          super.transform(ast)
        }

        case (_: For) | (_: Foreach) | (_: FunctionLiteral) =>
          newContext(super.transform(ast))

        case Switch(selector, entries) =>
          val newSelector = transform(selector).asInstanceOf[AstExpr]
          val newEntries = entries.map { case (guard, block) => newContext {
            val newGuard = transform(guard).asInstanceOf[AstExpr]
            val newBlock = transform(block).asInstanceOf[Block]
            newGuard -> newBlock
          }}
          Switch(newSelector, newEntries).fromAST(ast)

        case Try(tryBlock, catchs, finallyBlock) =>
          val newTry = transform(tryBlock).asInstanceOf[Block]
          val newCatchs = catchs.map { case (valDef, block) => newContext {
            val newVal = transform(valDef).asInstanceOf[ValueDefinition]
            val newBlock = transform(block).asInstanceOf[Block]
            newVal -> newBlock
          }}
          val newFinally = transform(finallyBlock).asInstanceOf[Block]
          Try(newTry, newCatchs, newFinally).fromAST(ast)

        case vd @ ValDef(_, name, _, _, _, _) =>
          val freshName = namer.fresh(name)
          context += name -> freshName
          super.transform(vd).asInstanceOf[ValDef].copy(name = freshName).fromAST(vd)

        case fd @ FunctionDef(_, name, _, _, _, _, _) =>
          val newDef = super.transform(fd).asInstanceOf[FunctionDef]
          context.get(name) match {
            case Some(freshName) => newDef.copy(name = freshName).fromAST(newDef)
            case None => newDef
          }

        case b @ Bind(name, _) =>
          val freshName = namer.fresh(name)
          context += name -> freshName
          super.transform(b).asInstanceOf[Bind].copy(name = freshName).fromAST(b)

        case Ident(name) => context.get(name) match {
          case Some(newName) => Ident(newName).fromAST(ast)
          case None => ast
        }

        case _ => super.transform(ast)
      })
    }
  }
}
