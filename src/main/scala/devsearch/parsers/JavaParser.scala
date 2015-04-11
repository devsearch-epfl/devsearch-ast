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

  def language = "Java"

  def parse(source: Source): AST = {
    val wrapped = wrappers.view.map { f =>
      try {
        val src = new ContentsSource(source.path, f(source.contents.mkString))
        val cu = com.github.javaparser.JavaParser.parse(src.toStream)
        new JavaASTVisitor(src).translate[AST](cu)
      } catch {
        case io: java.io.IOException => throw ParsingFailedError(io)
        case pe: com.github.javaparser.ParseException => pe
      }
    }

    wrapped.collectFirst { case ast: AST => ast } match {
      case Some(ast) => ast
      case None => throw ParsingFailedError(wrapped.last.asInstanceOf[Throwable])
    }
  }

  private val wrappers: List[String => String] = List(
    s => s,
    s => s"class ${Names.DEFAULT} { $s }",
    s => s"class ${Names.DEFAULT} { public void ${Names.DEFAULT}() { $s } }"
  )

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
        case (None, None) => Names.DEFAULT
      }
      case Some(n) => Option(n.getName) getOrElse Names.DEFAULT
      case None => Names.DEFAULT
    }

    private def extractName(s: String): String = Option(s) getOrElse Names.DEFAULT

    private def extractModifiers(m: Int): Modifiers = {
      (if (ModifierSet.isPublic(m))       Modifiers.PUBLIC       else Modifiers.NoModifiers) |
      (if (ModifierSet.isPrivate(m))      Modifiers.PRIVATE      else Modifiers.NoModifiers) |
      (if (ModifierSet.isProtected(m))    Modifiers.PROTECTED    else Modifiers.NoModifiers) |
      (if (ModifierSet.isStatic(m))       Modifiers.STATIC       else Modifiers.NoModifiers) |
      (if (ModifierSet.isFinal(m))        Modifiers.FINAL        else Modifiers.NoModifiers) |
      (if (ModifierSet.isSynchronized(m)) Modifiers.SYNCHRONIZED else Modifiers.NoModifiers) |
      (if (ModifierSet.isNative(m))       Modifiers.NATIVE       else Modifiers.NoModifiers) |
      (if (ModifierSet.isAbstract(m))     Modifiers.ABSTRACT     else Modifiers.NoModifiers)
    }

    private def arrayType(tpe: Typ, arrayCount: Int, pos: Position): Typ = if (arrayCount <= 0) tpe else {
      val arrTpe = ArrayType(arrayType(tpe, arrayCount - 1, pos))
      if (tpe.pos != NoPosition) arrTpe.setPos(tpe.pos) else arrTpe.setPos(pos)
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
      }.getOrElse(Names.DEFAULT -> Nil)

      PackageDef(name, annotations, translateList[Import](cu.getImports), translateList[Definition](cu.getTypes))
    })

    def visit(i: ImportDeclaration, a: Null) = List(inNode(i) {
      Import(extractName(i.getName), i.isAsterisk, i.isStatic)
    })

    def visit(tp: TypeParameter, a: Null) = List(inNode(tp) {
      TypeDef(Modifiers.NoModifiers, extractName(tp.getName),
        translateList[Annotation](tp.getAnnotations), Nil, Nil, translateList[Typ](tp.getTypeBound))
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
      val abstractedMembers = members.map {
        case fd: FunctionDef if decl.isInterface => fd.copy(modifiers = fd.modifiers | Modifiers.ABSTRACT).fromAST(fd)
        case vd: ValDef if decl.isInterface => vd.copy(modifiers = vd.modifiers | Modifiers.STATIC | Modifiers.FINAL).fromAST(vd)
        case m => m
      }
      ClassDef(modifiers, name, annotations,
        translateList[TypeDef](decl.getTypeParameters),
        translateList[ClassType](decl.getExtends) ++ translateList[ClassType](decl.getImplements),
        abstractedMembers, if (decl.isInterface) TraitSort else ClassSort)
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
        val name = idOpt.map(id => extractName(id.getName)) getOrElse Names.DEFAULT
        val arrayCount = idOpt.map(_.getArrayCount) getOrElse 0
        val finalTpe = arrayType(tpe, arrayCount, extractPosition(vd))
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
      val throwsAnnots = Option(decl.getThrows).toList.flatten.map(extractName).map { name =>
        inNode(decl)(Annotation(Names.THROWS_ANNOTATION, Map(Names.DEFAULT -> inNode(decl)(Ident(name)))))
      }
      ConstructorDef(extractModifiers(decl.getModifiers),
        translateList[Annotation](decl.getAnnotations) ++ throwsAnnots,
        translateList[TypeDef](decl.getTypeParameters),
        translateList[ValDef](decl.getParameters),
        translate[Block](decl.getBlock))
    })

    def visit(decl: MethodDeclaration, a: Null) = List(inNode(decl) {
      val throwsAnnots = Option(decl.getThrows).toList.flatten.map(extractName).map { name =>
        inNode(decl)(Annotation(Names.THROWS_ANNOTATION, Map(Names.DEFAULT -> inNode(decl)(Ident(name)))))
      }
      FunctionDef(extractModifiers(decl.getModifiers), extractName(decl.getNameExpr),
        translateList[Annotation](decl.getAnnotations) ++ throwsAnnots, translateList[TypeDef](decl.getTypeParameters),
        translateList[ValDef](decl.getParameters), translate[Typ](decl.getType),
        translate[Block](decl.getBody))
    })

    def visit(param: Parameter, a: Null) = List(inNode(param) {
      val idOpt = Option(param.getId)
      val name = idOpt.map(id => extractName(id.getName)) getOrElse Names.DEFAULT
      val arrayCount = idOpt.map(_.getArrayCount) getOrElse 0
      ValDef(extractModifiers(param.getModifiers), name, translateList[Annotation](param.getAnnotations),
        arrayType(translate[Typ](param.getType), arrayCount, extractPosition(param)), Empty[Expr], varArgs = param.isVarArgs)
    })

    def visit(param: MultiTypeParameter, a: Null) = Nil
    
    def visit(decl: EmptyMemberDeclaration, a: Null) = Nil
    
    def visit(init: InitializerDeclaration, a: Null) = List(inNode(init) {
      Initializer(init.isStatic, translateList[Annotation](init.getAnnotations), translate[Block](init.getBlock))
    })

    //- Type ----------------------------------------------

    def visit(tpe: ClassOrInterfaceType, a: Null) = List(inNode(tpe) {
      val name = extractName(tpe.getName)
      if (name == "String") PrimitiveTypes.String else {
        def translateScope(cot: ClassOrInterfaceType): (Expr, List[Annotation]) = Option(cot) match {
          case Some(tpe) =>
            val (scope, annotations) = translateScope(tpe.getScope)
            val newAnnotations = translateList[Annotation](tpe.getAnnotations)
            val newScope = inNode(tpe)(FieldAccess(scope, extractName(tpe.getName), translateList[Typ](tpe.getTypeArgs)))
            newScope -> (annotations ++ newAnnotations)
          case _ => Empty[Expr] -> Nil
        }

        val (scope, annotations) = translateScope(tpe.getScope)
        ClassType(scope, extractName(tpe.getName),
          annotations ++ translateList[Annotation](tpe.getAnnotations), translateList[Typ](tpe.getTypeArgs))
      }
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

    def visit(tpe: ReferenceType, a: Null) = List(inNode(tpe)(arrayType(translate[Typ](tpe.getType), tpe.getArrayCount, extractPosition(tpe))))

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
        case _                                        => Names.NOOP
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
      val name = extractName(expr.getNameExpr)
      val receiver = translate[Expr](expr.getScope)
      val caller = inNode(expr)(if (receiver == Empty[Expr]) Ident(name) else FieldAccess(receiver, name, Nil))
      FunctionCall(caller, translateList[Typ](expr.getTypeArgs), translateList[Expr](expr.getArgs))
    })

    def visit(expr: NameExpr, a: Null) = List(inNode(expr)(Ident(extractName(expr))))

    def visit(expr: ObjectCreationExpr, a: Null) = List(inNode(expr) {
      val classTpe = translate[Typ](expr.getType).asInstanceOf[ClassType]
      val tpe = classTpe.copy(scope = translate[Expr](expr.getScope), tparams = translateList[Typ](expr.getTypeArgs)).fromAST(classTpe)
      ConstructorCall(tpe, translateList[Expr](expr.getArgs), translateList[Definition](expr.getAnonymousClassBody))
    })

    def visit(expr: ThisExpr, a: Null) = List(inNode(expr)(This(translate[Expr](expr.getClassExpr))))

    def visit(expr: SuperExpr, a: Null) = List(inNode(expr)(Super(translate[Expr](expr.getClassExpr))))

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
        case _                                     => (Names.NOOP, false)
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
      Annotation(extractName(annot.getName), Map(Names.DEFAULT -> translate[Expr](annot.getMemberValue)))
    })

    def visit(annot: NormalAnnotationExpr, a: Null) = List(inNode(annot) {
      Annotation(extractName(annot.getName), mapSafe(annot.getPairs) { p =>
        val key = Option(p.getName) getOrElse Names.DEFAULT
        val value = translate[Expr](p.getValue)
        key -> value
      }.toMap)
    })

    def visit(expr: LambdaExpr, a: Null) = List(inNode(expr) {
      FunctionLiteral(translateList[ValDef](expr.getParameters), Empty[Typ], translate[Stmt](expr.getBody))
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
      val path = translate[Expr](stmt.getExpr)
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
      Block(translateList[Stmt](stmt.getStmts))
    })

    def visit(stmt: LabeledStmt, a: Null) = List(inNode(stmt) {
      NamedStatement(extractName(stmt.getLabel), translate[Stmt](stmt.getStmt))
    })

    def visit(stmt: EmptyStmt, a: Null) = Nil

    def visit(stmt: ExpressionStmt, a: Null) = inNodes(stmt)(translateList[Expr](stmt.getExpression))

    def visit(stmt: SwitchStmt, a: Null) = List(inNode(stmt) {
      Switch(translate[Expr](stmt.getSelector), mapSafe(stmt.getEntries) { entry =>
        val block = translateList[Stmt](entry.getStmts) match {
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
          val name = idOpt.map(id => extractName(id.getName)) getOrElse Names.DEFAULT
          val arrayCount = idOpt.map(_.getArrayCount) getOrElse 0
          translateList[Typ](except.getTypes).map { tpe =>
            val finalTpe = arrayType(tpe, arrayCount, extractPosition(except))
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


