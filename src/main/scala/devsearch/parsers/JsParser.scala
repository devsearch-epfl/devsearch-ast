package devsearch.parsers

import devsearch.ast._
import devsearch.ast.Modifiers._
import devsearch.ast.Empty._

import scala.util.parsing.combinator._

object JsParser extends Parser {

  def language = Languages.JavaScript

  def parse(source: Source) = new SourceParser(source).parse

  class SourceParser(source: Source) extends RegexParsers with PackratParsers {

    val comment = """//[^\n]*\n|//[^\n]*$|/\*(.|[\r\n])*?\*/""".r

    var parsingRegex: Boolean = false
    var partialComment: Option[String] = None
    var lineFeed: Boolean = false

    override protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int = {
      val whiteSpace = """[ \t]+""".r
      val lineSpace  = """[\n\x0B\f\r]+""".r
      val anySpace   = """[ \t\n\x0B\f\r]+""".r

      def parseComments(offset: Int): Int = {
        val subSeq = source.subSequence(offset, source.length)

        val whiteOffset = (whiteSpace findPrefixMatchOf subSeq).map(_.end) getOrElse 0
        val anyOffset = (anySpace findPrefixMatchOf subSeq).map(_.end) getOrElse 0

        val spaceOffset = offset + (whiteOffset max anyOffset)
        val spaceLF = whiteOffset != anyOffset

        val (commentLF, commentOffset) = comment findPrefixMatchOf (source.subSequence(spaceOffset, source.length)) match {
          case Some(matched) =>
            val commentLF = (lineSpace findFirstIn source.subSequence(spaceOffset, spaceOffset + matched.end)).isDefined
            partialComment = partialComment.map(_ + "\n" + matched.toString) orElse Some(matched.toString)
            (commentLF, spaceOffset + matched.end)
          case None =>
            (false, spaceOffset)
        }

        lineFeed = lineFeed || spaceLF || commentLF
        if (commentOffset == offset) offset else parseComments(commentOffset)
      }

      if (!parsingRegex) parseComments(offset) else offset
    }

    def consumeComment[T <: Commentable](ast: T): ast.type = {
      partialComment.foreach(ast.appendComment(_))
      partialComment = None
      ast
    }

    def convertPosition(pos: scala.util.parsing.input.Position): devsearch.ast.Position =
      new SimplePosition(source, pos.line - 1, pos.column - 1)

    def withPos[T <% devsearch.ast.Positional with Commentable](_p: => PackratParser[T]): PackratParser[T] = new PackratParser[T] {
      lazy val p = _p
      def apply(in: Input) = {
        val offset = in.offset
        val start = handleWhiteSpace(in.source, offset)
        p(in.drop (start - offset)) match {
          case Success(t, in1) =>
            Success(consumeComment(t.setPos(convertPosition(in.pos))).asInstanceOf[T], in1)
          case ns: NoSuccess => ns
        }
      }
    }

    implicit class PositionedFun[T,U <: devsearch.ast.Positional with Commentable](f: T => U) extends (T => U) with devsearch.ast.Positional with Commentable {
      def apply(arg: T) = {
        val positioned = f(arg).setPos(pos)
        comment.foreach(c => positioned.setComment(c))
        positioned
      }
    }

    implicit class PositionedFun2[T,U,V <: devsearch.ast.Positional with Commentable](f: (T,U) => V) extends ((T,U) => V) with devsearch.ast.Positional with Commentable {
      def apply(x: T, y: U) = {
        val positioned = f(x, y).setPos(pos)
        comment.foreach(c => positioned.setComment(c))
        positioned
      }
    }

    lazy val ASI = new PackratParser[String] {
      def apply(in: Input) = {
        val offset = in.offset
        val source = in.source
        val start = handleWhiteSpace(source, offset)

        val (semi, semiOffset) = if (start < source.length && source.charAt(start) == ';') {
          (true, start + 1)
        } else if (start < source.length && source.charAt(start) == '}') {
          (true, start)
        } else {
          (false, start)
        }

        if (lineFeed || semi || semiOffset == source.length) {
          lineFeed = false
          Success(";", in.drop(semiOffset - offset))
        } else {
          Failure("semi or line-feed expected but " + source.charAt(in.offset) + " found", in)
        }
      }
    }

    lazy val BitwiseANDExpressionNoIn = withPos(EqualityExpressionNoIn * withPos(BitwiseANDOperator ^^ makeBinaryOp))

    lazy val Elision = rep1(",") ^^ { _ map (a => devsearch.ast.NullLiteral) }

    lazy val Stmt = withPos(
      StmtsBlock           |
      VariableStatement    |
      EmptyStatement       |
      LabelledStatement    |
      IfStatement          |
      IterationStatement   |
      ContinueStatement    |
      BreakStatement       |
      ImportStatement      |
      ReturnStatement      |
      WithStatement        |
      SwitchStatement      |
      ThrowStatement       |
      TryStatement         |
      DebuggerStatement    |
      FunctionDeclaration  |
      ExpressionStatement  )

    lazy val DebuggerStatement = withPos("debugger" ~ ";" ^^ { _ => Ident("$debugger") })

    lazy val VariableDeclarationNoIn = withPos(Identifier ~ opt(InitialiserNoIn) ^^ { case id ~ i => ValDef(NoModifiers, id, Nil, NoType, i getOrElse Empty[Expr]) })

    lazy val LogicalANDExpression = withPos(BitwiseORExpression * withPos(LogicalANDOperator ^^ makeBinaryOp))

    lazy val ArgumentList = rep1sep(AssignmentExpression, ",")

    lazy val LogicalOROperator = "||"

    lazy val PostfixOperator = "++" | "--"

    lazy val ExpressionStatement = Expression <~ ASI

    lazy val CaseClauses = rep1(CaseClause)

    lazy val Stmts: PackratParser[List[Statement]] = rep1(Stmt) ^^ { stmts =>
      stmts.flatMap(x => x match {
        case Block(inner) => inner
        case _ => List(x)
      })
    }

    lazy val BitwiseORExpressionNoIn = withPos(BitwiseXORExpressionNoIn * withPos(BitwiseOROperator ^^ makeBinaryOp))

    lazy val CaseBlock = ("{" ~> opt(CaseClauses)) ~ DefaultClause ~ (opt(CaseClauses) <~ "}") ^^ { case c1 ~ d ~ c2 => c1.getOrElse(Nil) ++ List(d) ++ c2.getOrElse(Nil) } |
      "{" ~> opt(CaseClauses) <~ "}" ^^ { _.getOrElse(Nil) }

    lazy val AssignmentOperator = "="    |
                                  "*="   |
                                  "/="   |
                                  "%="   |
                                  "+="   |
                                  "-="   |
                                  "<<="  |
                                  ">>="  |
                                  ">>>=" |
                                  "&="   |
                                  "^="   |
                                  "|="  

    lazy val FunctionExpression = withPos("function" ~> opt(Identifier) ~
      withPos(("(" ~> opt(FormalParameterList) <~ ")") ~ StmtsBlock ^^ {
        case params ~ body => FunctionLiteral(params.toList.flatten, NoType, body)
      }) ^^ {
        case Some(name) ~ fun => Block(List(ValDef(NoModifiers, name, Nil, NoType, fun).setPos(fun.pos), Ident(name).setPos(fun.pos)))
        case None ~ fun => fun
      })

    lazy val Finally = withPos("finally" ~> StmtsBlock)

    lazy val CaseClause = ("case" ~> Expression) ~ withPos(":" ~> opt(Stmts) ^^ { ss => Block(ss.toList.flatten) }) ^^ { case e ~ ss => e -> ss }

    lazy val EmptyStatement = ";" ^^^ { NoStmt }

    lazy val ReturnStatement = withPos("return" ~> opt(Expression) <~ ASI ^^ { e => Return(e getOrElse NoExpr) })

    lazy val PostfixExpression = withPos(LeftHandSideExpression ~ PostfixOperator ^^ { case e ~ op => UnaryOp(e, op, true) } | LeftHandSideExpression)

    lazy val AdditiveOperator = "+" | "-"

    lazy val MemberExpressionPart: PackratParser[Expr => Expr] = withPos(
      "[" ~> Expression <~ "]" ^^ { x => (y: Expr) => ArrayAccess(y, x) } |
      "." ~> Field             ^^ { x => (y: Expr) => FieldAccess(y, x, Nil) })

    lazy val BitwiseANDExpression = withPos(EqualityExpression * withPos(BitwiseANDOperator ^^ makeBinaryOp))

    lazy val EqualityExpression = withPos(RelationalExpression * withPos(EqualityOperator ^^ makeBinaryOp))

    lazy val VariableDeclarationList = rep1sep(VariableDeclaration, ",")

    lazy val MultiplicativeExpression = withPos(UnaryExpression * withPos(MultiplicativeOperator ^^ makeBinaryOp))

    lazy val ConditionalExpressionNoIn = withPos(
      (LogicalORExpressionNoIn <~ "?") ~ (AssignmentExpression <~ ":") ~ AssignmentExpressionNoIn ^^ { case c ~ i ~ e => TernaryOp(c, i, e) }
    | LogicalORExpressionNoIn)

    lazy val BreakStatement = withPos("break" ~> opt(Identifier) <~ ASI ^^ { Break(_) })

    lazy val VariableDeclarationListNoIn = rep1sep(VariableDeclarationNoIn, ",")

    def combineMember(soFar: Expr, constructor: Expr => Expr) = constructor(soFar)

    lazy val MemberExpressionForIn = withPos(
      FunctionExpression |
      PrimaryExpression ~ rep(MemberExpressionPart) ^^ { case start ~ mods => mods.foldLeft(start)(combineMember) })

    lazy val AssignmentExpression: PackratParser[Expr] = withPos(
      LeftHandSideExpression ~ (
        ("?" ~> AssignmentExpression) ~ (":" ~> AssignmentExpression) ^^ { case i ~ e => ("ternary", i, e) }
      | AssignmentOperator ~ AssignmentExpression ^^ { case op ~ rhs => ("assign", op, rhs) }) ^^ {
        case lhs ~ (("ternary", i: Expr, e)) => TernaryOp(lhs, i, e)
        case lhs ~ (("assign", op: String, rhs)) => Assign(lhs, rhs, if (op == "=") None else Some(op.init))
      } | ConditionalExpression)

    lazy val EqualityOperator = "===" | "!==" | "==" | "!="

    lazy val MultiplicativeOperator = "*" | "/" | "%"

    lazy val LogicalORExpressionNoIn = withPos(LogicalANDExpressionNoIn * withPos(LogicalOROperator ^^ makeBinaryOp))

    lazy val ImportStatement = withPos("import" ~> Name ~ (opt("." ~> "*") <~ ASI) ^^ { case names ~ wild => Import(names.mkString("."), wild.isDefined, false) })

    lazy val Identifier = new PackratParser[String] {
      val ident = """([A-Za-z\$_\xA0-\uFFFF]|\\(x|u)[0-9a-fA-F]{2,4})([A-Za-z0-9\$_\xA0-\uFFFF]|\\(x|u)[0-9a-fA-F]{2,4})*""".r
      val keywords = Set("break", "do", "instanceof", "typeof", "case", "else", "new", "var", "catch", "finally", "return",
                         "void", "continue", "for", "switch", "while", "debugger", "function", "this", "with", "default", "if",
                         "throw", "delete", "in", "try", "null", "undefined")

      def apply(in: Input) = {
        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)
        (ident findPrefixMatchOf (source.subSequence(start, source.length))) match {
          case Some(matched) =>
            val result = source.subSequence(start, start + matched.end).toString
            if (keywords(result)) Failure("keyword found", in.drop(start - offset))
            else Success(result, in.drop(start + matched.end - offset))
          case None =>
            val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'" 
            Failure("string matching regex `"+ident+"' expected but "+found+" found", in.drop(start - offset))
        }
      }
    }

    lazy val Field = """([A-Za-z\$_\xA0-\uFFFF]|\\(x|u)[0-9a-fA-F]{2,4})([A-Za-z0-9\$_\xA0-\uFFFF]|\\(x|u)[0-9a-fA-F]{2,4})*""".r

    lazy val StmtsBlock: PackratParser[Block] = withPos("{" ~> opt(Stmts) <~ "}" ^^ { stmts => Block(stmts.toList.flatten) })

    lazy val MemberExpression = withPos(
      (FunctionExpression | PrimaryExpression) ~ rep(MemberExpressionPart) ^^ { case start ~ mods => mods.foldLeft(start)(combineMember) } |
      AllocationExpression)

    lazy val ThrowStatement = withPos("throw" ~> Expression <~ ASI ^^ { Throw(_) })

    lazy val RelationalExpression = withPos(ShiftExpression * withPos(RelationalOperator ^^ makeBinaryOp))

    lazy val InitialiserNoIn = withPos("=" ~> AssignmentExpressionNoIn)

    lazy val VariableStatement = withPos("var" ~> VariableDeclarationList <~ ASI ^^ { stmtBlock(_) })

    lazy val BitwiseXOROperator = "^"

    lazy val CallExpressionForIn = withPos(MemberExpressionForIn ~ Arguments ~ rep(CallExpressionPart) ^^ {
      case exp ~ args ~ parts => parts.foldLeft[Expr](FunctionCall(exp, Nil, args.toList.flatten).setPos(exp.pos))(combineMember)
    })

    lazy val CallExpression = withPos(MemberExpression ~ Arguments ~ rep(CallExpressionPart) ^^ {
      case exp ~ args ~ parts => parts.foldLeft[Expr](FunctionCall(exp, Nil, args.toList.flatten).setPos(exp.pos))(combineMember)
    })

    lazy val Literal: PackratParser[Expr] = withPos(
      RegularExpressionLiteral |
      HexIntegerLiteral        |
      DecimalLiteral           |
      StringLiteral            |
      BooleanLiteral           |
      NullLiteral)

    lazy val HexIntegerLiteral = withPos("""0[xX][0-9A-Fa-f]+""".r ^^ { SimpleLiteral(PrimitiveTypes.Special("Hex"), _) })
    
    lazy val BooleanLiteral = withPos(("true" | "false") ^^ { SimpleLiteral(PrimitiveTypes.Boolean, _) })

    lazy val NullLiteral = ("null" | "undefined") ^^^ { devsearch.ast.NullLiteral }

    lazy val Program = opt(Stmts) ^^ (_.toList.flatten match {
      case Nil => NoStmt
      case List(x) => x
      case xs => Block(xs).setPos(xs.head.pos)
    })

    lazy val VariableDeclaration = withPos(Identifier ~ opt(Initialiser) ^^ { case id ~ i => ValDef(NoModifiers, id, Nil, NoType, i getOrElse Empty[Expr]) })

    lazy val ContinueStatement = withPos("continue" ~> opt(Identifier) <~ ASI ^^ { Continue(_) })

    lazy val SwitchStatement = withPos(("switch" ~> "(" ~> Expression <~ ")") ~ CaseBlock ^^ { case e ~ cb => Switch(e, cb) })

    lazy val BitwiseXORExpressionNoIn = withPos(BitwiseANDExpressionNoIn * withPos(BitwiseXOROperator ^^ makeBinaryOp))

    lazy val RelationalExpressionNoIn = withPos(ShiftExpression * withPos(RelationalNoInOperator ^^ makeBinaryOp))

    lazy val LogicalANDOperator = "&&"

    lazy val PropertyName = ((StringLiteral | DecimalLiteral) ^^ { _.value }) | Field

    lazy val StringLiteral = withPos(new PackratParser[SimpleLiteral] {
      lazy val dq = "([^\\\\\"]+|\\\\([bfnrtv'\"\\\\/]|[0-3]?[0-7]{1,2}|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|\\s+))*".r
      lazy val sq = "([^\\\\']+|\\\\([bfnrtv'\"\\\\/]|[0-3]?[0-7]{1,2}|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|\\s+))*".r

      def apply(in: Input) = {
        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)

        val opt = if (start < source.length && source.charAt(start) == '"') Some('"', dq)
          else if (start < source.length && source.charAt(start) == ''') Some(''', sq)
          else None

        opt match {
          case Some((c, r)) =>
            val end = r findPrefixMatchOf source.subSequence(start + 1, source.length) match {
              case Some(matched) => matched.end
              case None => 0
            }

            if (source.length > start + end + 1 && source.charAt(start + end + 1) == c) {
              Success(SimpleLiteral(PrimitiveTypes.String, source.subSequence(start, start + end + 2).toString), in.drop(start + end + 2 - offset))
            } else {
              val found = if (start + end + 1 == source.length) "end of source" else "`"+source.charAt(start + end + 1)+"'"
              Failure("`"+c+"' expected but "+found+" found", in.drop(start - offset))
            }
          case None =>
            val found = if (start == source.length) "end of source" else "`"+source.charAt(start)+"'"
            Failure("`\"' or `'' expected but "+found+" found", in.drop(start - offset))
        }
      }
    })

    lazy val RegularExpressionLiteral = withPos(
      (("/" ^^ { s => parsingRegex = true; s }) ~> RegularExpressionBody) ~ ("/" ~> opt(RegularExpressionFlags)) ^^ { case body ~ flags =>
        parsingRegex = false
        val args = List(body) ++ flags.toList
        FunctionCall(Ident(Names.REGEXP).setPos(body.pos), Nil, args)
      })

    lazy val RegularExpressionBody = withPos(
      ("[^\\n*\\\\/\\[]".r | "\\\\[^\\n\\\\*/\\[]".r | ("[" ~> "([^\\n\\\\*/\\[\\]]|\\\\[^\\n\\\\*/\\[])+".r <~ "]")) ~
      ("[^\\n\\\\/\\[]".r | "\\\\[^\\n\\\\*/\\[]".r | ("[" ~> "([^\\n\\\\*/\\[\\]]|\\\\[^\\n\\\\*/\\[])+".r <~ "]")).* ^^ {
        case a ~ ss => SimpleLiteral(PrimitiveTypes.String, a + ss.mkString)
      })

    lazy val RegularExpressionFlags = withPos(Identifier ^^ { SimpleLiteral(PrimitiveTypes.String, _) })

    lazy val DecimalIntegerLiteral = "0" | """[1-9][0-9]*""".r 

    lazy val DecimalLiteral = withPos(
      DecimalIntegerLiteral ~ "." ~ """[0-9]*""".r ~ """([Ee][+-]?[0-9]+)?""".r ^^ { case a~b~c~d => SimpleLiteral(PrimitiveTypes.Double, a+b+c+d) } |
      "." ~ """[0-9]+""".r ~ """([Ee][+-]?[0-9]+)?""".r                         ^^ { case a~b~c   => SimpleLiteral(PrimitiveTypes.Double, a+b+c)   } |
      DecimalIntegerLiteral ~ """([Ee][+-]?[0-9]+)?""".r                        ^^ { case a~b     => SimpleLiteral(PrimitiveTypes.Double, a+b)     })

    lazy val ArgumentMemberExpressionParts = Arguments ~ rep(MemberExpressionPart) ^^ {
      case args ~ parts => (e: Expr) => parts.foldLeft[Expr](FunctionCall(e, Nil, args.toList.flatten))(combineMember)
    }

    def extractType(expr: Expr): ClassType = expr match {
      case FieldAccess(receiver, name, _) => ClassType(receiver, name, Nil, Nil).fromAST(expr)
      case ArrayAccess(receiver, SimpleLiteral(PrimitiveTypes.String, value)) => ClassType(receiver, value, Nil, Nil).fromAST(expr)
      case ArrayAccess(receiver, index) => ClassType(receiver, index.toString, Nil, Nil).fromAST(expr)
      case _ => ClassType(NoExpr, expr.toString, Nil, Nil).fromAST(expr)
    }

    lazy val AllocationExpression: PackratParser[Expr] = withPos(("new" ~> MemberExpression) ~ rep(ArgumentMemberExpressionParts) ^^ {
      case start ~ Nil =>
        ConstructorCall(extractType(start), Nil, Nil)
      case start ~ (args :: parts) =>
        parts.foldLeft[Expr](args(ConstructorCall(extractType(start), Nil, Nil)) match {
          case fc @ FunctionCall(ConstructorCall(tpe, Nil, Nil), Nil, args) => ConstructorCall(tpe, args, Nil).fromAST(fc)
          case cc => cc
        })(combineMember)
    })

    lazy val Catch = withPos(("catch" ~> "(" ~> Identifier) ^^ {
      id => ValDef(NoModifiers, id, Nil, NoType, NoExpr)
    }) ~ (")" ~> StmtsBlock) ^^ { case vd ~ block => vd -> block }

    lazy val TryStatement = withPos(
      "try" ~> StmtsBlock ~ Finally              ^^ { case b ~ f     => Try(b, Nil, f) }
    | "try" ~> StmtsBlock ~ Catch ~ opt(Finally) ^^ { case b ~ c ~ f => Try(b, List(c), f getOrElse NoStmt) })

    lazy val FormalParameterList = rep1sep(withPos(Identifier ^^ { ValDef(NoModifiers, _, Nil, NoType, NoExpr) }), ",")

    lazy val BitwiseORExpression = withPos(BitwiseXORExpression * withPos(BitwiseOROperator ^^ makeBinaryOp))

    lazy val Expression: PackratParser[Expr] = withPos(rep1sep(AssignmentExpression, ",") ^^ { exprBlock(_) })

    lazy val AdditiveExpression = withPos(MultiplicativeExpression * withPos(AdditiveOperator ^^ makeBinaryOp))

    lazy val ConditionalExpression = withPos(
      (LogicalORExpression ~ ("?" ~> AssignmentExpression) ~ (":" ~> AssignmentExpression)) ^^ { case c ~ i ~ e => TernaryOp(c, i, e) }
    | LogicalORExpression)

    lazy val UnaryExpression: PackratParser[Expr] = withPos(
      PostfixExpression
    | rep1(UnaryOperator) ~ PostfixExpression ^^ { case ops ~ e => ops.foldRight(e)((op:String, soFar:Expr) => UnaryOp(soFar, op, false)) })

    lazy val LeftHandSideExpression: PackratParser[Expr] = withPos(CallExpression | MemberExpression)

    lazy val FunctionDeclaration = withPos("function" ~> Identifier ~ ("(" ~> opt(FormalParameterList) <~ ")") ~ StmtsBlock ^^ {
      case name ~ params ~ body => FunctionDef(NoModifiers, name, Nil, Nil, params.toList.flatten, NoType, body)
    })

    lazy val Initialiser = withPos("=" ~> AssignmentExpression)

    lazy val CallExpressionPart = withPos(
      Arguments                ^^ { x => (y: Expr) => FunctionCall(y, Nil, x.toList.flatten) }
    | "[" ~> Expression <~ "]" ^^ { x => (y: Expr) => ArrayAccess(y, x) }
    | "." ~> Field             ^^ { x => (y: Expr) => FieldAccess(y, x, Nil) })

    lazy val RelationalNoInOperator = "<=" | ">=" | "<" | ">" | "instanceof"  

    lazy val AssignmentExpressionNoIn: PackratParser[Expr] = withPos(
      LeftHandSideExpression ~ (
        ("?" ~> AssignmentExpression) ~ (":" ~> AssignmentExpressionNoIn) ^^ { case i ~ e => ("ternary", i, e) }
      | AssignmentOperator ~ AssignmentExpressionNoIn ^^ { case op ~ rhs => ("assign", op, rhs) }) ^^ {
        case lhs ~ (("ternary", i: Expr, e)) => TernaryOp(lhs, i, e)
        case lhs ~ (("assign", op: String, rhs)) => Assign(lhs, rhs, if (op == "=") None else Some(op.init))
      } | ConditionalExpressionNoIn)

    lazy val PrimaryExpression = withPos(
      "this" ^^ { _ => This(NoExpr) }
    | ObjectLiteral
    | "(" ~> Expression <~ ")"
    | Identifier ^^ { Ident(_) }
    | ArrayLiteral
    | Literal)

    lazy val LogicalANDExpressionNoIn = withPos(BitwiseORExpressionNoIn * withPos(LogicalANDOperator ^^ makeBinaryOp))

    lazy val PropertyNameAndValueList = rep1sep(PropertyNameAndValue, ",") <~ opt(",") | "," ^^^ { Nil }

    lazy val Arguments = "(" ~> opt(ArgumentList) <~ ")"

    lazy val ObjectLiteral = withPos("{" ~> opt(PropertyNameAndValueList) <~ "}" ^^ { elems => ConstructorCall(NoType, Nil, elems.toList.flatten) })

    lazy val ExpressionNoIn: PackratParser[Expr] = rep1sep(AssignmentExpressionNoIn, ",") ^^ { exprBlock(_) }

    lazy val LeftHandSideExpressionForIn = withPos(CallExpressionForIn | MemberExpressionForIn  )

    lazy val ElementList = opt(Elision) ~> rep1sep(AssignmentExpression, Elision)

    lazy val LabelledStatement: PackratParser[Statement] = withPos((Identifier <~ ":") ~ Stmt ^^ { case i ~ s => NamedStatement(i, s) })

    lazy val DefaultClause = withPos("default" ^^^ Ident(Names.DEFAULT)) ~ (":" ~> opt(Stmts)) ^^ {
      case d ~ stmts => d -> (stmts.toList.flatten match {
        case Nil => NoStmt
        case (b: Block) :: Nil => b
        case stmts => Block(stmts).setPos(stmts.head.pos)
      })
    }

    lazy val BitwiseOROperator = "|"

    lazy val UnaryOperator = "delete" |
                             "void"   |
                             "typeof" |
                             "++"     |
                             "--"     |
                             "+"      |
                             "-"      |
                             "~"      |
                             "!"  

    lazy val RelationalOperator = "<="         |
                                  ">="         |
                                  "<"          |
                                  ">"          |
                                  "instanceof" |
                                  "in"

    lazy val ShiftExpression = withPos(AdditiveExpression * withPos(ShiftOperator ^^ makeBinaryOp))

    lazy val Name = rep1sep(Identifier, ".")

    lazy val BitwiseANDOperator = "&"

    lazy val ArrayLiteral = withPos(
      "[" ~> opt(Elision) <~ "]"          ^^ { elems => devsearch.ast.ArrayLiteral(NoType, Nil, Nil, elems.toList.flatten) } |
      "[" ~> ElementList ~ Elision <~ "]" ^^ { case a ~ b => devsearch.ast.ArrayLiteral(NoType, Nil, Nil, a ++ b) } |
      "[" ~> opt(ElementList) <~ "]"      ^^ { elems => devsearch.ast.ArrayLiteral(NoType, Nil, Nil, elems.toList.flatten) })

    lazy val EqualityExpressionNoIn = withPos(RelationalExpressionNoIn * withPos(EqualityOperator ^^ makeBinaryOp))

    lazy val ShiftOperator = ">>>" | ">>" | "<<"

    lazy val PropertyNameAndValue = withPos(
      (PropertyName <~ ":") ~ AssignmentExpression ^^ { case a ~ b => ValDef(NoModifiers, a, Nil, NoType, b) } |
      ("get" ~> PropertyName) ~ (("(" ~ ")") ~> StmtsBlock) ^^ { case n ~ block => FunctionDef(NoModifiers, n, Nil, Nil, Nil, NoType, block) } |
      ("set" ~> PropertyName) ~ ("(" ~> withPos(Identifier ^^ { ValDef(NoModifiers, _, Nil, NoType, NoExpr) }) <~ ")") ~ StmtsBlock ^^ {
        case n ~ arg ~ block => FunctionDef(NoModifiers, n, Nil, Nil, List(arg), NoType, block)
      })

    lazy val LogicalORExpression = withPos(LogicalANDExpression * withPos(LogicalOROperator ^^ makeBinaryOp))

    lazy val BitwiseXORExpression = withPos(BitwiseANDExpression * withPos(BitwiseXOROperator ^^ makeBinaryOp))

    lazy val WithStatement: PackratParser[Statement] = withPos(withPos("with" ^^ { Ident(_) }) ~ ("(" ~> Expression <~ ")") ~ Stmt ^^ {
      case id ~ e ~ s => FunctionCall(id, Nil, List(e, s match {
        case e: Expr => e
        case s => Block(List(s)).fromAST(s)
      }))
    })

    lazy val IterationStatement: PackratParser[Statement] = withPos(
      ("do" ~> Stmt) ~ (("while" ~> "(") ~> Expression <~ (")" <~ ASI)) ^^ { case s ~ e => Do(e ,s) } |
      "while" ~> "(" ~> Expression ~ (")" ~> Stmt) ^^ { case e ~ s => While(e, s) } |
      (("for" ~> "(" ~> opt(ExpressionNoIn)) ~ (";" ~> opt(Expression) <~ ";") ~ (opt(Expression) <~ ")") ~ Stmt) ^^ {
        case e1 ~ e2 ~ e3 ~ s => For(Nil, e1.toList, e2 getOrElse NoExpr, e3.toList, s)
      } |
      (("for" ~> "(" ~> "var" ~> VariableDeclarationList) ~ (";" ~> opt(Expression)) ~ (";" ~> opt(Expression) <~ ")") ~ Stmt) ^^ {
        case decls ~ e2 ~ e3 ~ s => For(decls, Nil, e2 getOrElse NoExpr, e3.toList, s)
      } |
      (("for" ~> "(" ~> "var" ~> VariableDeclarationNoIn) ~ ("in" ~> Expression <~ ")") ~ Stmt) ^^ {
        case decl ~ e ~ s => Foreach(List(decl), e, s, false)
      } |
      (("for" ~> "(" ~> LeftHandSideExpressionForIn) ~ ("in" ~> Expression <~ ")") ~ Stmt) ^^ {
        case lhs ~ e ~ s => Foreach(List(ValDef(NoModifiers, Names.DEFAULT, Nil, NoType, NoExpr).setPos(lhs.pos)), e, s match {
          case Block(ss) => Block(Assign(lhs, Ident(Names.DEFAULT).setPos(lhs.pos), None).setPos(lhs.pos) +: ss).fromAST(s).setPos(lhs.pos)
          case _ => Block(Assign(lhs, Ident(Names.DEFAULT).setPos(lhs.pos), None).setPos(lhs.pos) :: s :: Nil).setPos(s.pos)
        })
      })

    lazy val IfStatement: PackratParser[Expr] = withPos(("if" ~> "(" ~> Expression <~ ")") ~ Stmt ~ opt("else" ~> Stmt) ^^ {
      case cond ~ i ~ e => If(cond, i, e getOrElse NoStmt)
    })

    def makeBinaryOp(op: String) = (fact1: Expr, fact2: Expr) => BinaryOp(fact1, op, fact2)

    def stmtBlock(stmts: List[Statement]): Statement = stmts match {
      case Nil => Empty[Statement]
      case List(x) => x
      case ss => Block(ss)
    }

    def exprBlock(exprs: List[Expr]): Expr = exprs match {
      case Nil => Empty[Expr]
      case List(x) => x
      case ss => Block(ss)
    }

    def parse: AST = parseAll(Program, source.contents.mkString) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw ParsingFailedError(
        new RuntimeException(failure.msg + " @ line %d:%d".format(failure.next.pos.line, failure.next.pos.column) + "\n" + failure.next.pos.longString)
      )
    }
  }
}
