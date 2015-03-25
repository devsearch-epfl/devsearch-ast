package devsearch.parsers

import devsearch.ast
import devsearch.ast.{Statement => Stmt, Type => Typ, WildcardType => _, PrimitiveType => _, _}

import com.github.javaparser.ast._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.visitor._
import com.github.javaparser.ast.comments._

import scala.collection.JavaConversions._

object JavaParser extends Parser {

  def parse(source: Source): AST = {
    try {
      val cu = com.github.javaparser.JavaParser.parse(source.toStream)
      new JavaASTVisitor(source).translate[AST](cu)
    } catch {
      case io: java.io.IOException => throw ParsingFailedError(io)
    }
  }

  private def mapSafe[A,B](list: java.util.List[A])(f: A => B) = {
    Option(list).toList.flatten.flatMap(element => Option(element).map(f).toList)
  }

  private def flatMapSafe[A,B](list: java.util.List[A])(f: A => List[B]) = {
    Option(list).toList.flatten.flatMap(element => Option(element).toList.flatMap(f))
  }

  private class JavaASTVisitor(source: Source) extends GenericVisitor[List[AST], Null] {
    import Empty._

    def translate[A <: AST : EmptyProvider](node: Node): A =
      Option(node).toList.flatMap(_.accept(this, null)).headOption.map(_.asInstanceOf[A]) getOrElse Empty[A]

    private def translateList[A <: AST](node: Node): List[A] = 
      Option(node).toList.flatMap(_.accept(this, null).map(_.asInstanceOf[A]))

    private def translateList[A <: AST](nodes: java.util.List[_ <: Node]): List[A] =
      flatMapSafe(nodes)(_.accept(this, null).map(_.asInstanceOf[A]))

    private def extractName(n: NameExpr): String = Option(n) match {
      case Some(q: QualifiedNameExpr) => (Option(q.getQualifier), Option(q.getName)) match {
        case (Some(qualifier), Some(field)) => extractName(qualifier) + "." + field
        case (Some(qualifier), None) => extractName(qualifier)
        case (None, Some(field)) => field
        case (None, None) => Names.default
      }
      case Some(n) => Option(n.getName) getOrElse Names.default
      case None => Names.default
    }

    private def extractName(s: String): String = Option(s) getOrElse Names.default

    private def extractModifiers(m: Int): Modifiers = {
      (if (ModifierSet.isPublic(m))       Modifiers.PUBLIC       else Modifiers.NoModifiers) |
      (if (ModifierSet.isPrivate(m))      Modifiers.PRIVATE      else Modifiers.NoModifiers) |
      (if (ModifierSet.isProtected(m))    Modifiers.PROTECTED    else Modifiers.NoModifiers) |
      (if (ModifierSet.isStatic(m))       Modifiers.STATIC       else Modifiers.NoModifiers) |
      (if (ModifierSet.isFinal(m))        Modifiers.FINAL        else Modifiers.NoModifiers) |
      (if (ModifierSet.isSynchronized(m)) Modifiers.SYNCHRONIZED else Modifiers.NoModifiers) |
      (if (ModifierSet.isVolatile(m))     Modifiers.VOLATILE     else Modifiers.NoModifiers) |
      (if (ModifierSet.isTransient(m))    Modifiers.TRANSIENT    else Modifiers.NoModifiers) |
      (if (ModifierSet.isNative(m))       Modifiers.NATIVE       else Modifiers.NoModifiers) |
      (if (ModifierSet.isAbstract(m))     Modifiers.ABSTRACT     else Modifiers.NoModifiers) |
      (if (ModifierSet.isStrictfp(m))     Modifiers.STRICT       else Modifiers.NoModifiers)
    }

    private def arrayType(tpe: Typ, arrayCount: Int): Typ = {
      if (arrayCount > 0) ArrayType(arrayType(tpe, arrayCount - 1)).setPos(tpe.pos)
      else tpe
    }

    private def extractPosition(node: Node): Position = {
      val (startLine, startCol, endLine, endCol) =
        (node.getBeginLine, node.getBeginColumn, node.getEndLine, node.getEndColumn)
      source.position(startLine, startCol, endLine, endCol)
    }

    private def extractComment(node: Node): Option[String] = {
      val comments = Option(node.getComment).toList ++ (node match {
        case doc: DocumentableNode => Option(doc.getJavaDoc)
        case _ => None
      })
      
      comments.flatMap(c => Option(c.getContent)) match {
        case Nil => None
        case list => Some(list.mkString("\n\n"))
      }
    }

    private def inNode[A <: AST](node: Node)(ast: => A): A = {
      val positioned = ast.setPos(extractPosition(node))
      extractComment(node).foreach(positioned.setComment)
      positioned
    }

    private def inNodes[A <: AST](node: Node)(ast: => List[A]): List[A] = (extractComment(node), ast) match {
      case (Some(comment), x :: xs) => x.prependComment(comment) :: xs
      case (_, list) => list
    }

    //- Compilation Unit ----------------------------------
    
    def visit(cu: CompilationUnit, a: Null) = List(inNode(cu) {
      val (name,annotations) = Option(cu.getPackage).map { p =>
        val name = extractName(p.getName)
        val annotations = translateList[Annotation](p.getAnnotations)
        (name, annotations)
      }.getOrElse(Names.default -> Nil)

      PackageDef(name, annotations, translateList[Import](cu.getImports), translateList[Definition](cu.getTypes))
    })

    def visit(i: ImportDeclaration, a: Null) = List(inNode(i) {
      Import(extractName(i.getName), i.isAsterisk, i.isStatic)
    })

    def visit(tp: TypeParameter, a: Null) = List(inNode(tp) {
      TypeDef(extractName(tp.getName), translateList[Annotation](tp.getAnnotations), Nil, translateList[Typ](tp.getTypeBound))
    })
    
    //- Body ----------------------------------------------

    private def extractTypeDeclaration(td: TypeDeclaration): (String, Modifiers, List[Annotation], List[Definition]) = {
      val name = extractName(td.getName)
      val modifiers = extractModifiers(td.getModifiers)
      val annotations = translateList[Annotation](td.getAnnotations)
      val members = translateList[Definition](td.getMembers)
      (name, modifiers, annotations, members)
    }

    def visit(decl: ClassOrInterfaceDeclaration, a: Null) = List(inNode(decl) {
      val (name, modifiers, annotations, members) = extractTypeDeclaration(decl)
      ClassDef(modifiers, name, annotations,
        translateList[TypeDef](decl.getTypeParameters),
        translateList[ClassType](decl.getExtends) ++ translateList[ClassType](decl.getImplements),
        members, decl.isInterface)
    })

    def visit(decl: EnumDeclaration, a: Null) = List(inNode(decl) {
      val (name, modifiers, annotations, members) = extractTypeDeclaration(decl)
      EnumDef(modifiers, name, annotations,
        translateList[ClassType](decl.getImplements), members,
        translateList[EnumConstantDef](decl.getEntries))
    })

    def visit(decl: EmptyTypeDeclaration, a: Null) = Nil

    def visit(decl: EnumConstantDeclaration, a: Null) = List(inNode(decl) {
      EnumConstantDef(extractName(decl.getName), translateList[Annotation](decl.getAnnotations),
        translateList[Expr](decl.getArgs), translateList[Definition](decl.getClassBody))
    })

    def visit(decl: AnnotationDeclaration, a: Null) = List(inNode(decl) {
      val (name, modifiers, annotations, members) = extractTypeDeclaration(decl)
      AnnotationDef(modifiers, name, annotations, members)
    })

    def visit(decl: AnnotationMemberDeclaration, a: Null) = List(inNode(decl) {
      ValDef(extractModifiers(decl.getModifiers), extractName(decl.getName),
        translateList[Annotation](decl.getAnnotations), translate[Typ](decl.getType),
        translate[Expr](decl.getDefaultValue))
    })

    private def translateVariables(nodes: java.util.List[VariableDeclarator], modifiers: Modifiers, annotations: List[Annotation], tpe: Typ): List[ValDef] = {
      mapSafe(nodes) { vd =>
        val idOpt = Option(vd.getId)
        val name = idOpt.map(id => extractName(id.getName)) getOrElse Names.default
        val arrayCount = idOpt.map(_.getArrayCount) getOrElse 0
        val finalTpe = arrayType(tpe, arrayCount)
        val init = translate[Expr](vd.getInit)
        inNode(vd)(ValDef(modifiers, name, annotations, finalTpe, init))
      }
    }

    def visit(decl: FieldDeclaration, a: Null) = {
      val modifiers = extractModifiers(decl.getModifiers)
      val annotations = translateList[Annotation](decl.getAnnotations)
      val tpe = translate[Typ](decl.getType)
      inNodes(decl)(translateVariables(decl.getVariables, modifiers, annotations, tpe))
    }

    def visit(decl: ConstructorDeclaration, a: Null) = List(inNode(decl) {
      ConstructorDef(extractModifiers(decl.getModifiers), extractName(decl.getNameExpr),
        translateList[Annotation](decl.getAnnotations), translateList[TypeDef](decl.getTypeParameters),
        translateList[ValDef](decl.getParameters),
        Option(decl.getThrows).toList.flatten.map(extractName),
        translate[Block](decl.getBlock))
    })

    def visit(decl: MethodDeclaration, a: Null) = List(inNode(decl) {
      FunctionDef(extractModifiers(decl.getModifiers), extractName(decl.getNameExpr),
        translateList[Annotation](decl.getAnnotations), translateList[TypeDef](decl.getTypeParameters),
        translateList[ValDef](decl.getParameters), translate[Typ](decl.getType),
        Option(decl.getThrows).toList.flatten.map(extractName),
        translate[Block](decl.getBody))
    })

    def visit(param: Parameter, a: Null) = List(inNode(param) {
      val idOpt = Option(param.getId)
      val name = idOpt.map(id => extractName(id.getName)) getOrElse Names.default
      val arrayCount = idOpt.map(_.getArrayCount) getOrElse 0
      ValDef(extractModifiers(param.getModifiers), name, translateList[Annotation](param.getAnnotations),
        arrayType(translate[Typ](param.getType), arrayCount), Empty[Expr], varArgs = param.isVarArgs)
    })

    def visit(param: MultiTypeParameter, a: Null) = Nil
    
    def visit(decl: EmptyMemberDeclaration, a: Null) = Nil
    
    def visit(init: InitializerDeclaration, a: Null) = List(inNode(init) {
      Initializer(init.isStatic, translateList[Annotation](init.getAnnotations), translate[Block](init.getBlock))
    })

    //- Type ----------------------------------------------

    def visit(tpe: ClassOrInterfaceType, a: Null) = List(inNode(tpe) {
      val name = extractName(tpe.getName)
      if (name == "String") PrimitiveTypes.String
      else ClassType(extractName(tpe.getName), translate[Typ](tpe.getScope),
        translateList[Annotation](tpe.getAnnotations), translateList[Typ](tpe.getTypeArgs))
    })

    def visit(tpe: PrimitiveType, a: Null) = List {
      Option(tpe.getType) match {
        case Some(PrimitiveType.Primitive.Boolean) => PrimitiveTypes.Boolean
        case Some(PrimitiveType.Primitive.Char)    => PrimitiveTypes.Char
        case Some(PrimitiveType.Primitive.Byte)    => PrimitiveTypes.Byte
        case Some(PrimitiveType.Primitive.Short)   => PrimitiveTypes.Short
        case Some(PrimitiveType.Primitive.Int)     => PrimitiveTypes.Int
        case Some(PrimitiveType.Primitive.Long)    => PrimitiveTypes.Long
        case Some(PrimitiveType.Primitive.Float)   => PrimitiveTypes.Float
        case Some(PrimitiveType.Primitive.Double)  => PrimitiveTypes.Double
        case _                                     => PrimitiveTypes.Null
      }
    }

    def visit(tpe: ReferenceType, a: Null) = List(inNode(tpe)(arrayType(translate[Typ](tpe.getType), tpe.getArrayCount)))

    def visit(tpe: VoidType, a: Null) = List(PrimitiveTypes.Void)

    def visit(tpe: WildcardType, a: Null) = List(inNode(tpe) {
      val superType = Option(tpe.getExtends).map(translate[Typ](_)) getOrElse AnyType
      val subType = Option(tpe.getSuper).map(translate[Typ](_)) getOrElse BottomType
      ast.WildcardType(subType, superType)
    })

    //- Expression ----------------------------------------

    def visit(expr: ArrayAccessExpr, a: Null) = List(inNode(expr) {
      ArrayAccess(translate[Expr](expr.getName), translate[Expr](expr.getIndex))
    })

    def visit(expr: ArrayCreationExpr, a: Null) = List(inNode(expr) {
      val annotations = flatMapSafe(expr.getArraysAnnotations)(list => translateList[Annotation](list))
      ArrayLiteral(translate[Typ](expr.getType), annotations, translateList[Expr](expr.getDimensions), translateList[Expr](expr.getInitializer))
    })

    def visit(expr: ArrayInitializerExpr, a: Null) = inNodes(expr)(translateList[Expr](expr.getValues))

    def visit(expr: AssignExpr, a: Null) = List(inNode(expr) {
      Assign(translate[Expr](expr.getTarget), translate[Expr](expr.getValue),
        Option(expr.getOperator) match {
          case Some(AssignExpr.Operator.assign)         => None
          case Some(AssignExpr.Operator.plus)           => Some("+")
          case Some(AssignExpr.Operator.minus)          => Some("-")
          case Some(AssignExpr.Operator.star)           => Some("*")
          case Some(AssignExpr.Operator.slash)          => Some("/")
          case Some(AssignExpr.Operator.and)            => Some("&")
          case Some(AssignExpr.Operator.or)             => Some("|")
          case Some(AssignExpr.Operator.xor)            => Some("^")
          case Some(AssignExpr.Operator.rem)            => Some("%")
          case Some(AssignExpr.Operator.lShift)         => Some("<<")
          case Some(AssignExpr.Operator.rSignedShift)   => Some(">>")
          case Some(AssignExpr.Operator.rUnsignedShift) => Some(">>>")
          case _                                        => None
        }
      )
    })

    def visit(expr: BinaryExpr, a: Null) = List(inNode(expr) {
      BinaryOp(translate[Expr](expr.getLeft), Option(expr.getOperator) match {
        case Some(BinaryExpr.Operator.or)             => "||"
        case Some(BinaryExpr.Operator.and)            => "&&"
        case Some(BinaryExpr.Operator.binOr)          => "|"
        case Some(BinaryExpr.Operator.binAnd)         => "&"
        case Some(BinaryExpr.Operator.xor)            => "^"
        case Some(BinaryExpr.Operator.equals)         => "=="
        case Some(BinaryExpr.Operator.notEquals)      => "!="
        case Some(BinaryExpr.Operator.less)           => "<"
        case Some(BinaryExpr.Operator.greater)        => ">"
        case Some(BinaryExpr.Operator.lessEquals)     => "<="
        case Some(BinaryExpr.Operator.greaterEquals)  => ">="
        case Some(BinaryExpr.Operator.lShift)         => "<<"
        case Some(BinaryExpr.Operator.rSignedShift)   => ">>"
        case Some(BinaryExpr.Operator.rUnsignedShift) => ">>>"
        case Some(BinaryExpr.Operator.plus)           => "+"
        case Some(BinaryExpr.Operator.minus)          => "-"
        case Some(BinaryExpr.Operator.times)          => "*"
        case Some(BinaryExpr.Operator.divide)         => "/"
        case Some(BinaryExpr.Operator.remainder)      => "%"
        case _                                        => Names.noop
      }, translate[Expr](expr.getRight))
    })

    def visit(expr: CastExpr, a: Null) = List(inNode(expr) {
      Cast(translate[Expr](expr.getExpr), translate[Typ](expr.getType))
    })

    def visit(expr: ClassExpr, a: Null) = List(inNode(expr) {
      ClassAccess(translate[Typ](expr.getType))
    })

    def visit(expr: ConditionalExpr, a: Null) = List(inNode(expr) {
      TernaryOp(translate[Expr](expr.getCondition), translate[Expr](expr.getThenExpr), translate[Expr](expr.getElseExpr))
    })

    def visit(expr: EnclosedExpr, a: Null) = List(inNode(expr) {
      translate[Expr](expr.getInner)
    })

    def visit(expr: FieldAccessExpr, a: Null) = List(inNode(expr) {
      FieldAccess(translate[Expr](expr.getScope), extractName(expr.getFieldExpr), translateList[Typ](expr.getTypeArgs))
    })
    
    def visit(expr: InstanceOfExpr, a: Null) = List(inNode(expr) {
      InstanceOf(translate[Expr](expr.getExpr), translate[Typ](expr.getType))
    })

    def visit(expr: StringLiteralExpr, a: Null)          = List(inNode(expr)(SimpleLiteral(PrimitiveTypes.String,  Option(expr.getValue) getOrElse "")))
    def visit(expr: IntegerLiteralExpr, a: Null)         = List(inNode(expr)(SimpleLiteral(PrimitiveTypes.Int,     Option(expr.getValue) getOrElse "")))
    def visit(expr: LongLiteralExpr, a: Null)            = List(inNode(expr)(SimpleLiteral(PrimitiveTypes.Long,    Option(expr.getValue) getOrElse "")))
    def visit(expr: CharLiteralExpr, a: Null)            = List(inNode(expr)(SimpleLiteral(PrimitiveTypes.Char,    Option(expr.getValue) getOrElse "")))
    def visit(expr: DoubleLiteralExpr, a: Null)          = List(inNode(expr)(SimpleLiteral(PrimitiveTypes.Double,  Option(expr.getValue) getOrElse "")))
    def visit(expr: BooleanLiteralExpr, a: Null)         = List(inNode(expr)(SimpleLiteral(PrimitiveTypes.Boolean, expr.getValue.toString)))
    def visit(expr: IntegerLiteralMinValueExpr, a: Null) = List(inNode(expr)(SimpleLiteral(PrimitiveTypes.Int,     "MIN_VALUE")))
    def visit(expr: LongLiteralMinValueExpr, a: Null)    = List(inNode(expr)(SimpleLiteral(PrimitiveTypes.Long,    "MIN_VALUE")))
    def visit(expr: NullLiteralExpr, a: Null)            = List(NullLiteral)
    
    def visit(expr: MethodCallExpr, a: Null) = List(inNode(expr) {
      FunctionCall(translate[Expr](expr.getScope), extractName(expr.getNameExpr),
        translateList[Typ](expr.getTypeArgs), translateList[Expr](expr.getArgs))
    })

    def visit(expr: NameExpr, a: Null) = List(inNode(expr)(Ident(extractName(expr))))

    def visit(expr: ObjectCreationExpr, a: Null) = List(inNode(expr) {
      ConstructorCall(translate[Expr](expr.getScope), translate[Typ](expr.getType).asInstanceOf[ClassType],
        translateList[Typ](expr.getTypeArgs), translateList[Expr](expr.getArgs),
        translateList[Definition](expr.getAnonymousClassBody))
    })

    private def translatePath(expr: Expr): Typ = expr match {
      case FieldAccess(receiver, name, tparams) => ClassType(name, translatePath(receiver), Nil, tparams).copyFrom(expr)
      case MethodAccess(receiver, name, tparams) => ClassType(name, receiver, Nil, tparams).copyFrom(expr)
      case Ident(name) => ClassType(name, Empty[Typ], Nil, Nil).copyFrom(expr)
      case _ => Empty[Typ]
    }

    def visit(expr: ThisExpr, a: Null) = List(inNode(expr)(This(translatePath(translate[Expr](expr.getClassExpr)))))

    def visit(expr: SuperExpr, a: Null) = List(inNode(expr)(Super(translatePath(translate[Expr](expr.getClassExpr)))))

    def visit(expr: UnaryExpr, a: Null) = List(inNode(expr) {
      val (op, fix) = Option(expr.getOperator) match {
        case Some(UnaryExpr.Operator.positive)     => ("+",        false)
        case Some(UnaryExpr.Operator.negative)     => ("-",        false)
        case Some(UnaryExpr.Operator.preIncrement) => ("++",       false)
        case Some(UnaryExpr.Operator.preDecrement) => ("--",       false)
        case Some(UnaryExpr.Operator.not)          => ("!",        false)
        case Some(UnaryExpr.Operator.inverse)      => ("~",        false)
        case Some(UnaryExpr.Operator.posIncrement) => ("++",       true)
        case Some(UnaryExpr.Operator.posDecrement) => ("--",       true)
        case _                                     => (Names.noop, false)
      }
      UnaryOp(translate[Expr](expr.getExpr), op, fix)
    })

    def visit(expr: VariableDeclarationExpr, a: Null) = {
      val modifiers = extractModifiers(expr.getModifiers)
      val annotations = translateList[Annotation](expr.getAnnotations)
      val tpe = translate[Typ](expr.getType)
      inNodes(expr)(translateVariables(expr.getVars, modifiers, annotations, tpe))
    }

    def visit(annot: MarkerAnnotationExpr, a: Null) = List(inNode(annot) {
      Annotation(extractName(annot.getName))
    })

    def visit(annot: SingleMemberAnnotationExpr, a: Null) = List(inNode(annot) {
      Annotation(extractName(annot.getName), Map(Names.default -> translate[Expr](annot.getMemberValue)))
    })

    def visit(annot: NormalAnnotationExpr, a: Null) = List(inNode(annot) {
      Annotation(extractName(annot.getName), mapSafe(annot.getPairs) { p =>
        val key = Option(p.getName) getOrElse Names.default
        val value = translate[Expr](p.getValue)
        key -> value
      }.toMap)
    })

    def visit(expr: LambdaExpr, a: Null) = List(inNode(expr) {
      FunctionLiteral(translateList[ValDef](expr.getParameters), translate[Stmt](expr.getBody))
    })

    def visit(expr: MethodReferenceExpr, a: Null) = List(inNode(expr) {
      val name = extractName(expr.getIdentifier)
      val tparams = translateList[Typ](expr.getTypeParameters)
      Option(expr.getScope) match {
        case Some(tr: TypeExpr) =>
          MethodAccess(translate[Typ](tr.getType), name, tparams)
        case _ =>
          FieldAccess(translate[Expr](expr.getScope), name, tparams)
      }
    })

    //- Statements ----------------------------------------

    def visit(stmt: ExplicitConstructorInvocationStmt, a: Null) = List(inNode(stmt) {
      val path = translatePath(translate[Expr](stmt.getExpr))
      val tparams = translateList[Typ](stmt.getTypeArgs)
      val args = translateList[Expr](stmt.getArgs)
      if (stmt.isThis) ThisCall(path, tparams, args)
      else SuperCall(path, tparams, args)
    })

    def visit(stmt: TypeDeclarationStmt, a: Null) = inNodes(stmt)(translateList[Definition](stmt.getTypeDeclaration))

    def visit(stmt: AssertStmt, a: Null) = List(inNode(stmt) {
      Assert(translate[Expr](stmt.getCheck), translate[Expr](stmt.getMessage))
    })

    def visit(stmt: BlockStmt, a: Null) = List(inNode(stmt) {
      Block(translateList[AST](stmt.getStmts))
    })

    def visit(stmt: LabeledStmt, a: Null) = List(inNode(stmt) {
      NamedStatement(extractName(stmt.getLabel), translate[Stmt](stmt.getStmt))
    })

    def visit(stmt: EmptyStmt, a: Null) = Nil

    def visit(stmt: ExpressionStmt, a: Null) = inNodes(stmt)(translateList[Expr](stmt.getExpression))

    def visit(stmt: SwitchStmt, a: Null) = List(inNode(stmt) {
      Switch(translate[Expr](stmt.getSelector), mapSafe(stmt.getEntries) { entry =>
        val block = translateList[AST](entry.getStmts) match {
          case list @ (x :: xs) => Block(list).setPos(x.pos)
          case _ => Empty[Block]
        }
        translate[Expr](entry.getLabel) -> block
      })
    })

    def visit(stmt: BreakStmt, a: Null) = List(inNode(stmt)(Break(Option(stmt.getId))))

    def visit(stmt: ContinueStmt, a: Null) = List(inNode(stmt)(Continue(Option(stmt.getId))))

    def visit(stmt: ReturnStmt, a: Null) = List(inNode(stmt)(Return(translate[Expr](stmt.getExpr))))

    def visit(stmt: IfStmt, a: Null) = List(inNode(stmt) {
      If(translate[Expr](stmt.getCondition), translate[Stmt](stmt.getThenStmt), translate[Stmt](stmt.getElseStmt))
    })

    def visit(stmt: WhileStmt, a: Null) = List(inNode(stmt) {
      While(translate[Expr](stmt.getCondition), translate[Stmt](stmt.getBody))
    })

    def visit(stmt: DoStmt, a: Null) = List(inNode(stmt) {
      Do(translate[Expr](stmt.getCondition), translate[Stmt](stmt.getBody))
    })

    def visit(stmt: ForeachStmt, a: Null) = List(inNode(stmt) {
      Foreach(translateList[ValDef](stmt.getVariable), translate[Expr](stmt.getIterable), translate[Stmt](stmt.getBody))
    })

    def visit(stmt: ForStmt, a: Null) = List(inNode(stmt) {
      val initializers = translateList[AST](stmt.getInit)
      val defs = initializers.collect { case d : ValDef => d }
      val inits = initializers.collect { case e : Expr => e }
      For(defs, inits, translate[Expr](stmt.getCompare),
        translateList[Expr](stmt.getUpdate), translate[Stmt](stmt.getBody))
    })

    def visit(stmt: ThrowStmt, a: Null) = List(inNode(stmt)(Throw(translate[Expr](stmt.getExpr))))

    def visit(stmt: SynchronizedStmt, a: Null) = List(inNode(stmt) {
      Synchronize(translate[Expr](stmt.getExpr), translate[Stmt](stmt.getBlock))
    })

    def visit(stmt: TryStmt, a: Null) = {
      val resources = translateList[ValDef](stmt.getResources)
      val tryBlock = translate[Block](stmt.getTryBlock)
      val finallyBlock = translate[Block](stmt.getFinallyBlock)
      val catchs: List[(ValDef, Block)] = flatMapSafe(stmt.getCatchs) { clause =>
        val cases = Option(clause.getExcept).toList.flatMap { except =>
          val modifiers = extractModifiers(except.getModifiers)
          val annotations = translateList[Annotation](except.getAnnotations)

          val idOpt = Option(except.getId)
          val name = idOpt.map(id => extractName(id.getName)) getOrElse Names.default
          val arrayCount = idOpt.map(_.getArrayCount) getOrElse 0
          translateList[Typ](except.getTypes).map { tpe =>
            val finalTpe = arrayType(tpe, arrayCount)
            ValDef(modifiers, name, annotations, finalTpe, Empty[Expr]).setPos(tpe.pos)
          }
        }

        val block = translate[Block](clause.getCatchBlock)
        cases.map(c => c -> block)
      }

      resources ++ List(inNode(stmt)(Try(tryBlock, catchs, finallyBlock)))
    }

    def visit(decl: TypeExpr, a: Null) = Nil
    def visit(decl: CatchClause, a: Null) = Nil
    def visit(decl: SwitchEntryStmt, a: Null) = Nil
    def visit(decl: MemberValuePair, a: Null) = Nil
    def visit(decl: QualifiedNameExpr, a: Null) = Nil
    def visit(decl: JavadocComment, a: Null) = Nil
    def visit(decl: VariableDeclaratorId, a: Null) = Nil
    def visit(decl: VariableDeclarator, a: Null) = Nil
    def visit(decl: BlockComment, a: Null) = Nil
    def visit(decl: LineComment, a: Null) = Nil
    def visit(decl: PackageDeclaration, a: Null) = Nil
  }
}


