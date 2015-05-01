package devsearch.normalized

import devsearch.ast

sealed trait Definition {
  def name: String
  def definitions: List[Definition]
}

trait CodeDefinition extends Definition {
  val graph: Graph with ControlFlowGraph
}

trait AstExtraction extends ControlFlowGraphs { self =>

  def mkNode = new Node
  class Node extends devsearch.normalized.Node {
    private[AstExtraction] var _statements: List[Statement] = Nil
    def statements: List[Statement] = _statements

    private[AstExtraction] var _locked: Boolean = false
    def locked: Boolean = _locked
    private[AstExtraction] def lock: this.type = {
      _locked = true
      this
    }

    private[AstExtraction] def add(stmt: Statement): this.type = {
      if(!locked) _statements :+= stmt
      this
    }

    private[AstExtraction] var _result: Value = Literal("null")
    def result: Value = _result
    private[AstExtraction] def setResult(result: Value): this.type = {
      if (!locked) _result = result
      this
    }

    def withStatements(stmts: List[Statement]): Node = new Node {
      override def statements = stmts
      override def locked = Node.this.locked
      override def result = Node.this.result
    }
  }

  class Definition(
    val name: String
  ) extends devsearch.normalized.Definition {
    private var _defs: List[Definition] = Nil
    def definitions: List[Definition] = _defs
    private[AstExtraction] def add(definition: Definition): Unit = { _defs :+= definition }
  }

  class CodeDefinition(
    name: String
  ) extends Definition(name) with devsearch.normalized.CodeDefinition {
    val graph = new Graph
  }

  case class NormalizationError(msg: String) extends RuntimeException(msg)

  def extract(devAst: ast.Statement): Definition = {

    // Note that all names generated here won't need to be freshened during the final
    // single-assignment pass since these should only be assigned once as they are
    // programmatically generated during code transformation
    val namer = new Namer("$")

    type Assignable = Either[Value => List[Statement], Identifier]

    def assign(node: Node, optAssign: Option[Assignable], expr: Expr): Node = {
      optAssign match {
        case Some(Left(cons)) => expr match {
          case value: Value =>
            cons(value).foreach(node.add(_))
          case _ =>
            val id = Identifier(namer.fresh("x"))
            node.add(Assign(id, expr))
            cons(id).foreach(node.add(_))
        }
        case _ =>
          val id = optAssign match {
            case Some(Right(id)) => id
            case _ => Identifier(namer.fresh("x"))
          }
          node.add(Assign(id, expr))
      }

      node.statements.lastOption match {
        case Some(Assign(id, _)) => node.setResult(id)
        case Some(FieldAssign(_, _, value)) => node.setResult(value)
        case Some(IndexAssign(_, _, value)) => node.setResult(value)
        case _ => throw NormalizationError("Missformed assignment statement")
      }
    }

    def emptyOrNew(node: Node)(implicit scope: Scope { type Def <: CodeDefinition }): Node = {
      if (node.statements.isEmpty && scope.definition.graph.next(node).isEmpty) node else {
        val n = newNode
        connect(node, n, "setup-cond")
        n
      }
    }

    def toResult(node: Node)(implicit scope: Scope { type Def <: CodeDefinition }): Node = {
      if (node.result == Identifier("$noResult")) {
        val previous = scope.definition.graph.prev(node).collect {
          case Edge(p, _, _, _) if !p.locked => toResult(p).result
        }

        if (previous.isEmpty) {
          node.setResult(Literal("null"))
        } else if (previous.size == 1) {
          node.setResult(previous.head)
        } else {
          assign(node, None, Phi(previous.toList))
        }
      }
      node
    }

    def newNode(implicit scope: Scope { type Def <: CodeDefinition }): Node = scope.definition.graph.newNode

    def connect(n1: Node, n2: Node, reason: String, guard: Option[(Value, Boolean)] = None)
               (implicit scope: Scope { type Def <: CodeDefinition }) = {
      scope.definition.graph.connect(n1, n2, reason, guard)
    }

    trait Scope {
      type Def <: Definition

      def loopEnd: Node
      def loopCondition: Node
      def catchStart: Node
      def definition: Def
      def named: List[(String, Option[Scope])]

      def namedScope(name: String): Scope = named.find(_._1 == name).map(_._2).flatten.getOrElse {
        throw NormalizationError("Can't access named scope outside of parent loop")
      }

      def inLoop(cond: Node, end: Node) = new Scope {
        type Def = Scope.this.Def
        val loopEnd: Node = end
        val loopCondition: Node = cond
        lazy val catchStart: Node = Scope.this.catchStart
        lazy val definition: Def = Scope.this.definition
        val named: List[(String, Option[Scope])] = Scope.this.named match {
          case (name, None) :: scopes => (name, Some(this)) :: scopes
          case scopes => scopes
        }
      }

      def inTry(catchs: Node) = new Scope {
        type Def = Scope.this.Def
        lazy val loopEnd: Node = Scope.this.loopEnd
        lazy val loopCondition: Node = Scope.this.loopCondition
        val catchStart: Node = catchs
        lazy val definition: Def = Scope.this.definition
        val named: List[(String, Option[Scope])] = Scope.this.named
      }

      def inClass(n: String) = new Scope {
        type Def = Definition
        lazy val loopEnd: Node = Scope.this.loopEnd
        lazy val loopCondition: Node = Scope.this.loopCondition
        def catchStart: Node = throw NormalizationError("Can't throw exceptions in class definition!")
        val named: List[(String, Option[Scope])] = Scope.this.named

        val definition: Def = {
          val newDef = new Definition(n)
          Scope.this.definition.add(newDef)
          newDef
        }
      }

      def inDef(n: String, params: List[(Identifier, Type)]) = new Scope {
        type Def = CodeDefinition
        lazy val loopEnd: Node = Scope.this.loopEnd
        lazy val loopCondition: Node = Scope.this.loopCondition
        val named: List[(String, Option[Scope])] = Scope.this.named

        val definition: Def = {
          val newDef = new CodeDefinition(n)
          for ((id, tpe) <- params) {
            newDef.graph.firstNode.add(Assign(id, New(tpe, Seq.empty)))
          }
          Scope.this.definition.add(newDef)
          newDef
        }

        val catchStart: Node = definition.graph.lastNode
      }

      def withNamed(nme: String) = new Scope {
        type Def = Scope.this.Def

        lazy val loopEnd: Node = Scope.this.loopEnd
        lazy val loopCondition: Node = Scope.this.loopCondition
        lazy val catchStart: Node = Scope.this.catchStart
        lazy val definition: Def = Scope.this.definition
        val named: List[(String, Option[Scope])] = (nme -> None) :: Scope.this.named
      }
    }

    object Scope {
      def empty = new Scope {
        type Def = CodeDefinition
        def loopEnd: Node = throw NormalizationError("Can't access loopEnd outside of loop")
        def loopCondition: Node = throw NormalizationError("Cann't access loopCondition outside of loop")
        val definition = new CodeDefinition("$program")
        val catchStart: Node = definition.graph.lastNode
        val named: List[(String, Option[Scope])] = Nil
      }
    }

    def simpleType(tpe: ast.AST): Type = tpe match {
      case ast.ClassType(_, "List" | "LinkedList" | "ArrayList", _, _) => ListType
      case ast.ClassType(_, "Map" | "HashMap" | "LinkedHashMap", _, _) => MapType
      case ast.ClassType(_, name, _, _) => ReferenceType(name)
      case p: ast.PrimitiveType => PrimitiveType(p)
      case f: ast.FunctionType => ReferenceType(namer.fresh("$fun"))
      case _ => UnknownType
    }

    def recStmt(current: Node, stmt: ast.Statement)(implicit scope: Scope { type Def <: CodeDefinition }): Node = recStmts(current, stmt :: Nil)(scope)

    def recStmts(current: Node, stmts: List[ast.Statement])(implicit scope: Scope { type Def <: CodeDefinition }): Node = stmts match {
      case Nil => current

      case ast.NamedStatement(name, stmt) :: rest =>
        recStmts(current, stmt :: rest)(scope.withNamed(name))

      case ast.While(cond, body) :: rest =>
        val condHead = emptyOrNew(current)
        val condLast = recExpr(condHead, cond)

        val restHead = newNode
        val restLast = recStmts(restHead, rest)
        connect(condLast, restHead, "while-false", Some(condLast.result -> false))

        val bodyHead = newNode
        val bodyLast = recStmt(bodyHead, body)(scope.inLoop(condHead, restHead))
        connect(condLast, bodyHead, "while-true", Some(condLast.result -> true))
        connect(bodyLast, condHead, "while-body-cond")
        restLast

      case ast.Do(cond, body) :: rest =>
        val condHead = newNode
        val condLast = recExpr(condHead, cond)

        val restHead = newNode
        val restLast = recStmts(restHead, rest)
        connect(condLast, restHead, "do-false", Some(condLast.result -> false))

        val bodyHead = emptyOrNew(current)
        val bodyLast = recStmt(bodyHead, body)(scope.inLoop(condHead, restHead))
        connect(bodyLast, condHead, "do-body-cond")
        connect(condLast, bodyHead, "do-true", Some(condLast.result -> true))
        restLast

      case ast.Break(label) :: rest => // rest is dead code!
        val targetScope = label match {
          case Some(name) => scope.namedScope(name)
          case None => scope
        }
        connect(current, targetScope.loopEnd, "break")
        current.lock

      case ast.Continue(label) :: rest => // rest is dead code!
        val targetScope = label match {
          case Some(name) => scope.namedScope(name)
          case None => scope
        }
        connect(current, targetScope.loopCondition, "continue")
        current.lock

      case ast.For(vals, inits, cond, updates, body) :: rest =>
        val initLast = recStmts(current, vals ++ inits)

        val condHead = emptyOrNew(initLast)
        val condLast = recExpr(condHead, cond)

        val restHead = newNode
        val restLast = recStmts(restHead, rest)
        connect(condLast, restHead, "for-false", Some(condLast.result -> false))

        val updatesHead = newNode
        val nscope = scope.inLoop(updatesHead, restHead)
        val updatesLast = recStmts(updatesHead, updates)(nscope)
        connect(updatesLast, condHead, "for-updates-cond")

        val bodyHead = newNode
        val bodyLast = recStmt(bodyHead, body)(nscope)
        connect(condLast, bodyHead, "for-true", Some(condLast.result -> true))
        connect(bodyLast, updatesHead, "for-body-updates")
        restLast

      case ast.Return(expr) :: rest => // rest is dead code!
        val retLast = recExpr(current, expr)
        connect(retLast, scope.definition.graph.lastNode, "return")
        retLast.lock

      case ast.If(cond, thenn, elze) :: rest =>
        val condLast = recExpr(current, cond)

        val thenHead = newNode
        val thenLast = recStmt(thenHead, thenn)
        connect(condLast, thenHead, "if-true", Some(condLast.result -> true))

        val elseHead = newNode
        val elseLast = recStmt(elseHead, elze)
        connect(condLast, elseHead, "if-false", Some(condLast.result -> false))

        val restHead = newNode
        val restLast = recStmts(restHead, rest)
        connect(thenLast, restHead, "if-true-join")
        connect(elseLast, restHead, "if-false-join")
        restLast

      case ast.Switch(selector, entries) :: rest =>
        val selLast = recExpr(current, selector)
        val selID = selLast.result

        val restHead = newNode
        val nscope = scope.inLoop(restHead, restHead)
        entries.foreach { case (guard, block) =>
          val guardHead = newNode
          val guardedNode = recPattern(guardHead, guard, selID)
          connect(selLast, guardHead, "switch-guard")

          val blockLast = recStmt(guardedNode, block)(nscope)
          connect(blockLast, restHead, "switch-join")
        }

        if (entries.isEmpty) connect(selLast, restHead, "switch-empty")
        recStmts(restHead, rest)

      case ast.Foreach(vals, iterable, body, _) :: rest =>
        val iterLast = recExpr(current, iterable)
        val it = iterLast.result

        val condNode = newNode
        assign(condNode, None, Field(it, "hasNext"))
        assign(condNode, None, Call(condNode.result, Seq.empty))
        connect(iterLast, condNode, "foreach-cond")

        val restHead = newNode
        val restLast = recStmts(restHead, rest)
        connect(condNode, restHead, "foreach-false", Some(condNode.result -> false))

        val bodyHead = newNode
        val bodyLast = recStmt(bodyHead, body)(scope.inLoop(condNode, restHead))
        connect(condNode, bodyHead, "foreach-true", Some(condNode.result -> true))
        connect(bodyLast, condNode, "foreach-body-cond")
        restLast

      case ast.Try(tryBlock, catchs, finallyBlock) :: rest =>
        val finallyHead = newNode
        val finallyLast = recStmt(finallyHead, finallyBlock)
        val restLast = recStmts(finallyLast, rest)

        val catchsNode = newNode
        val tryLast = recStmt(current, tryBlock)(scope.inTry(catchsNode))
        connect(tryLast, catchsNode, "catch-node")

        catchs.foreach { case (catcher, block) =>
          val (optId, tpe, guard) = catcher match {
            case ast.ValDef(_, name, _, tpe, _, _) => (Some(Right(Identifier(name))), simpleType(tpe), ast.Empty.NoExpr)
            case ast.ExtractionValDef(_, ex @ ast.FunctionCall(r, _, _), _, _) => (None, simpleType(r), ex)
            case ast.ExtractionValDef(_, ex @ ast.Guarded(ast.FunctionCall(r, _, _), _), _, _) => (None, simpleType(r), ex)
            case ast.ExtractionValDef(_, guard, _, _) => (None, UnknownType, guard)
          }

          val id = assign(catchsNode, optId, Catch(tpe)).result

          val catchHead = newNode
          val catchLast = recPattern(catchHead, guard, id)
          connect(catchsNode, catchHead, "catch-guard", Some(id -> true))

          val bodyLast = recStmt(catchLast, block)
          connect(bodyLast, finallyHead, "catch-join")
        }
        restLast

      case ast.Throw(expr) :: rest => // rest is dead code
        val throwLast = recExpr(current, expr)
        throwLast.add(Throw(throwLast.result))
        connect(throwLast, scope.catchStart, "throw")
        throwLast.lock

      case ast.ValDef(_, name, _, _, expr, _) :: rest =>
        val exprLast = recExpr(current, expr, Some(Right(Identifier(name))))
        recStmts(exprLast, rest)

      case ast.ExtractionValDef(_, pattern, _, expr) :: rest =>
        val exprLast = recExpr(current, expr)
        val pattLast = recPattern(exprLast, pattern, exprLast.result)
        recStmts(pattLast, rest)

      case ast.Block(stmts) :: rest =>
        recStmts(current, stmts ++ rest)

      case ast.Assert(expr, msg) :: rest =>
        val lastExpr = recExpr(current, expr)
        val failHead = newNode
        val failLast = recExpr(failHead, msg)
        failLast.add(Throw(failLast.result))
        connect(lastExpr, failHead, "assert-false", Some(lastExpr.result -> false))
        connect(failLast, scope.catchStart, "assert-throw")

        val restHead = newNode
        val restLast = recStmts(restHead, rest)
        connect(lastExpr, restHead, "assert-true", Some(lastExpr.result -> true))
        restLast

      case (_: ast.Import) :: rest =>
        recStmts(current, rest)

      case ast.Synchronize(lock, body) :: rest =>
        val lockLast = recExpr(current, lock)
        recStmts(lockLast, body :: rest)

      case ast.SuperCall(_, _, args) :: rest =>
        val (argsLast, argsResults) = args.foldLeft(current -> List.empty[Value]) { case ((last, acc), arg) =>
          val argLast = recExpr(last, arg)
          (argLast, acc :+ argLast.result)
        }
        argsLast.add(Mutator(Call(Super(), argsResults)))
        recStmts(argsLast, rest)

      case ast.ThisCall(_, _, args) :: rest =>
        val (argsLast, argsResults) = args.foldLeft(current -> List.empty[Value]) { case ((last, acc), arg) =>
          val argLast = recExpr(last, arg)
          (argLast, acc :+ argLast.result)
        }
        argsLast.add(Mutator(Call(This(), argsResults)))
        recStmts(argsLast, rest)

      case (d: ast.Definition) :: rest =>
        recDef(d)
        recStmts(current, rest)

      case (expr: ast.Expr) :: rest =>
        val exprLast = recExpr(current, expr)
        if (rest.isEmpty) exprLast else {
          exprLast.add(Mutator(exprLast.result))
          recStmts(exprLast, rest)
        }
    }

    def extractDef(stmt: ast.Statement)(implicit scope: Scope { type Def <: CodeDefinition }): Unit = {
      val defHead = scope.definition.graph.firstNode
      val defLast = stmt match {
        case expr: ast.Expr => recExpr(defHead, expr)
        case _ => recStmt(defHead, stmt)
      }
      connect(defLast, scope.definition.graph.lastNode, "last")
    }

    def recDef(definition: ast.Definition)(implicit scope: Scope): Unit = definition match {
      case ast.PackageDef(name, _, _, defs) =>
        val nscope = scope.inClass(name)
        defs.foreach(recDef(_)(nscope))

      case ast.ClassDef(_, name, _, _, _, defs, _) =>
        val nscope = scope.inClass(name)
        defs.foreach(recDef(_)(nscope))

      case ast.FunctionDef(_, name, _, _, params, _, body) =>
        extractDef(body)(scope.inDef(name, params.map(vd => Identifier(vd.name) -> simpleType(vd.tpe))))

      case ast.ConstructorDef(_, _, _, params, body, _) =>
        extractDef(body)(scope.inDef("$constructor", params.map(vd => Identifier(vd.name) -> simpleType(vd.tpe))))

      case ast.ValDef(_, name, _, _, expr, _) =>
        extractDef(expr)(scope.inDef(name, Nil))

      case ast.ExtractionValDef(_, pattern, _, expr) =>
        val dscope = scope.inDef(namer.fresh("$pat"), Nil)
        val defHead = dscope.definition.graph.firstNode
        val defLast = recExpr(defHead, expr)(dscope)
        val patternLast = recPattern(defLast, pattern, defLast.result)(dscope)
        connect(patternLast, dscope.definition.graph.lastNode, "last")(dscope)

      case ast.Initializer(_, _, body) =>
        extractDef(body)(scope.inDef("$init", Nil))

      case (_: ast.EnumDef) | (_: ast.EnumConstantDef) | (_: ast.AnnotationDef) | (_: ast.TypeDef) => // ignore
      case ast.Empty.NoDef => // ignore
    }

    def recPattern(current: Node, expr: ast.Expr, selector: Value)(implicit scope: Scope { type Def <: CodeDefinition }): Node = expr match {
      case ast.Bind(name, e) =>
        val lastExpr = recPattern(current, e, selector)
        assign(lastExpr, Some(Right(Identifier(name))), lastExpr.result)

      case ast.FunctionCall(receiver, _, args) =>
        val id = assign(current, None, Unapply(simpleType(receiver), selector)).result

        val node = newNode
        val binders = args.map(a => a match {
          case ast.Bind(name, expr) => Identifier(name) -> expr
          case _ => Identifier(namer.fresh("$binder")) -> a
        })
        node.add(MultiAssign(binders.map(_._1), id))
        connect(current, node, "match-unapply", Some(id -> true))

        binders.foldLeft(node) { case (node, (id, expr)) =>
          recPattern(node, expr, id)
        }

      case ast.Guarded(expr, guard) =>
        val exprLast = recPattern(current, expr, selector)
        val guardLast = recExpr(exprLast, guard)

        val node = newNode
        connect(guardLast, node, "match-guard", Some(guardLast.result -> true))
        node

      case ast.Ident(ast.Names.DEFAULT) =>
        val node = newNode
        connect(current, node, "match-default", Some(Default -> true))
        node

      case ast.Wildcard =>
        current

      case _ =>
        val exprLast = recExpr(current, expr)
        val id = assign(exprLast, None, BinaryOp(selector, "==", exprLast.result)).result
        val node = newNode
        connect(current, node, "match-guard", Some(id -> true))
        node
    }

    def recExpr(current: Node, expr: ast.Expr, assignID: Option[Assignable] = None)(implicit scope: Scope { type Def <: CodeDefinition }): Node = expr match {
      case ast.Block(stmts) =>
        toResult(recStmts(current, stmts))

      case ast.UnaryOp(e, op, postfix) =>
        val opLast = recExpr(current, e)
        assign(opLast, assignID, UnaryOp(opLast.result, op, postfix))

      case ast.BinaryOp(left, op, right) =>
        val leftLast = recExpr(current, left)
        val leftResult = leftLast.result

        val rightLast = recExpr(leftLast, right)
        assign(rightLast, assignID, BinaryOp(leftResult, op, rightLast.result))

      case ast.TernaryOp(cond, thenn, elze) =>
        recExpr(current, ast.If(cond, thenn, elze).fromAST(expr), assignID)

      case ast.FunctionCall(receiver, _, args) =>
        val receiverLast = recExpr(current, receiver)
        val receiverResult = receiverLast.result

        val (argsLast, argsResults) = args.foldLeft(receiverLast -> List.empty[Value]) { case ((last, acc), arg) =>
          val argLast = recExpr(last, arg)
          (argLast, acc :+ argLast.result)
        }

        assign(argsLast, assignID, Call(receiverResult, argsResults))

      case ast.ConstructorCall(tpe, args, _) =>
        val (argsLast, argsResults) = args.foldLeft(current -> List.empty[Value]) { case ((last, acc), arg) =>
          val argLast = recExpr(last, arg)
          (argLast, acc :+ argLast.result)
        }

        assign(argsLast, assignID, New(simpleType(tpe), argsResults))

      case ast.ArrayAccess(array, index) =>
        val arrayLast = recExpr(current, array)
        val arrayResult = arrayLast.result

        val indexLast = recExpr(arrayLast, index)
        assign(indexLast, assignID, Index(arrayResult, indexLast.result))

      case ast.ArrayLiteral(_, _, _, _) =>
        assign(current, assignID, New(ListType, Nil))

      case ast.MultiLiteral(_) =>
        assign(current, assignID, New(ListType, Nil))

      case ast.MapLiteral(_) =>
        assign(current, assignID, New(MapType, Nil))

      case ast.Assign(lhs, rhs, optOp) =>
        val lhsLast = recExpr(current, lhs)
        val nextAssign = lhsLast.statements.lastOption match {
          case Some(Assign(id, f @ (_: Field | _: Index))) =>
            val assign: Value => Statement = f match {
              case Field(id, name) => FieldAssign(id, name, _).setPos(f.pos)
              case Index(arr, idx) => IndexAssign(arr, idx, _).setPos(f.pos)
              case _ => throw NormalizationError("Only field and index should be matched...")
            }

            Left(optOp match {
              case Some(op) => (result: Value) => {
                val tmpID = Identifier(namer.fresh("x"))
                List(Assign(tmpID, BinaryOp(id, op, result)), assign(tmpID))
              }
              case None =>
                // remove assignment of field to id since we're going to assign directly to the field
                lhsLast._statements = lhsLast.statements.init
                (result: Value) => List(assign(result))
            })

          case _ => lhsLast.result match {
            case id: Identifier => Right(id)
            case _ => throw NormalizationError("Can't assign to " + lhsLast.result)
          }
        }

        val rhsLast = recExpr(lhsLast, rhs, Some(nextAssign))
        assignID.foreach { id => assign(rhsLast, Some(id), rhsLast.result) }
        rhsLast

      case ast.Cast(expr, _) =>
        recExpr(current, expr)

      case ast.ClassAccess(tpe) =>
        recExpr(current, ast.MethodAccess(tpe, "class", Nil).fromAST(expr))

      case ast.Ident(name) => assignID match {
        case Some(_) => assign(current, assignID, Identifier(name))
        case _ => current.setResult(Identifier(name))
      }

      case ast.MethodAccess(tpe, name, _) =>
        assign(current, assignID, simpleType(tpe) match {
          case ReferenceType(str) => Field(Identifier(str), name)
          case ListType => Field(Identifier("List"), name)
          case MapType => Field(Identifier("Map"), name)
          case _ => Field(Identifier("Type"), name)
        })

      case ast.FieldAccess(receiver, name, _) =>
        val recLast = recExpr(current, receiver)
        assign(recLast, assignID, Field(recLast.result, name))

      case ast.InstanceOf(expr, tpe) =>
        val exprLast = recExpr(current, expr)
        assign(exprLast, assignID, InstanceOf(exprLast.result, simpleType(tpe)))

      case ast.SimpleLiteral(tpe, value) =>
        current.setResult(Literal(value))

      case ast.NullLiteral | ast.Wildcard =>
        current.setResult(Literal("null"))

      case ast.VoidLiteral =>
        current.setResult(Literal("Unit"))

      case ast.This(_) =>
        current.setResult(This())

      case ast.Super(_) =>
        current.setResult(Super())

      case ifExpr : ast.If =>
        toResult(recStmt(current, ifExpr))

      case switchExpr : ast.Switch =>
        toResult(recStmt(current, switchExpr))

      case foreachExpr : ast.Foreach =>
        toResult(recStmt(current, foreachExpr))

      case tryExpr : ast.Try =>
        toResult(recStmt(current, tryExpr))

      case throwExpr : ast.Throw =>
        toResult(recStmt(current, throwExpr))

      case fl @ ast.FunctionLiteral(params, tpe, body) =>
        val block = body match {
          case b: ast.Block => b
          case _ => ast.Block(body :: Nil).setPos(body.pos)
        }

        val freshName = namer.fresh("$fun")
        recDef(ast.FunctionDef(ast.Modifiers.NoModifiers, freshName, Nil, Nil, params, tpe, block).fromAST(fl))
        assign(current, assignID, Identifier(freshName))

      case a: ast.Annotation => 
        current

      case (_: ast.Bind) | (_: ast.Guarded) =>
        throw NormalizationError("Shoulnd't be encountered outside of pattern trees: " + expr)

      case ast.Empty.NoExpr =>
        current
    }

    val emptyScope = Scope.empty

    devAst match {
      case d: ast.Definition =>
        recDef(d)(emptyScope)
        emptyScope.definition.definitions match {
          case List(d) => d
          case defs => throw NormalizationError("Weird definition hierarchy: " + defs)
        }

      case s: ast.Statement =>
        extractDef(s)(emptyScope)
        emptyScope.definition
    }
  }
}

