package devsearch.parsers

import devsearch.ast
import devsearch.ast.{Source, Names, AST}

import scala.tools.nsc.interactive._
import scala.reflect.internal.util.{Position => _, _}

import org.scalamacros.paradise.parser.Tokens._
import org.scalamacros.paradise.quasiquotes._

object ScalaParser extends ScalaParserLike {
  def acceptHoles = false
}

object QueryParser extends ScalaParserLike {
  def acceptHoles = true
}

abstract class ScalaParserLike extends Parser {

  def acceptHoles: Boolean
  def language = Languages.Scala

  private lazy val compiler = {
    val settings = new scala.tools.nsc.Settings
    val scalaLib = Option(scala.Predef.getClass.getProtectionDomain.getCodeSource).map(_.getLocation.getPath).getOrElse {
      throw ParsingFailedError(new RuntimeException("couldn't find scala library"))
    }

    settings.classpath.value = scalaLib
    settings.usejavacp.value = false

    new scala.tools.nsc.interactive.Global(settings, new scala.tools.nsc.reporters.Reporter {
      def info0(pos: scala.reflect.internal.util.Position, msg: String, severity: Severity, force: Boolean) = ()
    })
  }

  private lazy val queryCompiler = new QueryCompiler

  def parse(source: Source): AST = try {
    queryCompiler.parse(source)
  } catch {
    case abort: scala.reflect.macros.runtime.AbortMacroException => throw ParsingFailedError(abort)
  }

  private class QueryCompiler extends Quasiquotes { self =>
    object c extends {
      val universe: compiler.type = compiler
      val expandee = universe.EmptyTree
      val callsiteTyper = universe.analyzer.newTyper(universe.analyzer.NoContext)
    } with scala.reflect.macros.runtime.Context {
      val prefix = Expr[Nothing](universe.EmptyTree)(TypeTag.Nothing)
    }

    import global._
    import compat.{gen, build}
    import build.implodePatDefs
    import scala.collection.mutable.{ListBuffer, Set => MutableSet}

    class Comment(val value: String)

    object parser extends Parser {
      // We're not going to use the entryPoint as this limits robust parsing
      def entryPoint = parser => EmptyTree

      // Quick fix to make sure we have range positions everywhere even if they don't make sense...
      // Without this, we get crashes in the Paradise parser in TreeGen.mkFor where range positions are expected
      override lazy val compat = new { val global: compiler.type = compiler } with scala.quasiquotes.SymbolTableCompat {
        override lazy val gen = new { val global: compiler.type = compiler } with scala.quasiquotes.TreeGen {
          import global._
          override def mkFor(enums: List[Tree], sugarBody: Tree)(implicit fresh: quasiquotes.FreshNameCreator): Tree = {
            def rangePos(t: Tree): t.type = t.pos match {
              case NoPosition => t
              case range: RangePosition => t
              case p => t.setPos(new RangePosition(p.source, p.point, p.point, p.point))
            }
            rangePos(super.mkFor(enums.map(rangePos(_)), sugarBody))
          }
        }
      }

      def parse(source: SourceFile): Tree = {
        try {
          val (parser, tree) = if (acceptHoles) {
            val parser = new RobustParser(source)
            val tree = parser.smartParse()
            (parser, tree)
          } else {
            val parser = new HoleParser(source)
            val tree = parser.parse()
            (parser, tree)
          }

          // assign comments as tree attachments
          (new Traverser {
            var comments: Seq[(Int, String)] = {
              val comments = parser.comments.toSeq.map(p => (p._1._1, p._1._2, p._2))
              val grouped = comments.groupBy(_._2).mapValues(_.sortBy(_._1).map(_._3).mkString("\n"))
              grouped.toSeq.sortBy(_._1)
            }

            override def traverse(tree: Tree): Unit = {
              while (comments.nonEmpty && tree.pos.start > comments.head._1) {
                tree.updateAttachment(new Comment(comments.head._2))
                comments = comments.tail
              }
            }
          }).traverse(tree)

          tree

        } catch {
          case mi: MalformedInput => c.abort(NoPosition, mi.msg)
        }
      }

      class HoleParser(source0: SourceFile) extends QuasiquoteParser(source0) {
        lazy val dfltName = newTermName(Names.DEFAULT)

        object dummyHole extends Hole {
          val tree = EmptyTree
          val pos = NoPosition
          val rank = Rank.NoDot
        }

        holeMap.update(c.fresh[TermName]("_"), dummyHole)

        override def isHole: Boolean = acceptHoles && in.token == USCORE
        override def isHole(name: Name): Boolean = acceptHoles && name.toString == "_"

        override def ident(skipIt: Boolean): Name =
          if (acceptHoles && in.name.toString == "_") {
            in.nextToken()
            dfltName 
          } else super.ident(skipIt)

        def reflectedPkgQualId(): Tree = {
          import scala.reflect.runtime.{universe => ru}
          val m = ru.runtimeMirror(getClass.getClassLoader)
          val im = m.reflect(this)

          val sym = ru.typeOf[HoleParser].member(ru.newTermName("pkgQualId")).asMethod
          im.reflectMethod(sym).apply().asInstanceOf[Tree]
        }

        def queryCode(): Tree = checkNoEscapingPlaceholders {
          def topstats(): List[Tree] = {
            val ts = new ListBuffer[Tree]
            while (in.token == SEMI) in.nextToken()
            val start = in.offset
            if (in.token == PACKAGE) {
              in.nextToken()
              if (in.token == OBJECT) {
                ts ++= joinComment(List(makePackageObject(start, objectDef(in.offset, NoMods))))
                if (in.token != EOF) {
                  acceptStatSep()
                  ts ++= topStatSeq()
                }
              } else {
                in.flushDoc
                val pkg = reflectedPkgQualId()

                if (in.token == EOF) {
                  ts += makePackaging(start, pkg, List())
                } else if (isStatSep) {
                  in.nextToken()
                  ts += makePackaging(start, pkg, topstats())
                } else {
                  ts += inBraces(makePackaging(start, pkg, templateOrTopStatSeq()))
                  acceptStatSepOpt()
                  ts ++= templateOrTopStatSeq()
                }
              }
            } else {
              ts ++= templateOrTopStatSeq()
            }
            ts.toList
          }

          resetPackage()
          gen.mkTreeOrBlock(topstats())
        }

        override def parseStartRule = () => queryCode()

        private var _comments: Map[(Int, Int), String] = Map.empty
        def comments: Map[(Int, Int), String] = _comments
        def comment(value: String, start: Int, end: Int): Unit = {
          _comments += (start, end) -> value
        }
      }

      import scala.tools.nsc.ast.parser._
      import scala.reflect.internal.Chars.{ LF, CR }

      /** Based on scalac's UnitParser */
      class RobustParser(source0: SourceFile, patches: List[BracePatch]) extends HoleParser(source0) {
        def this(source0: SourceFile) = this(source0, Nil)

        override def newScanner() = new RobustScanner(this, patches)

        private var smartParsing = false
        @inline private def withSmartParsing[T](body: => T): T = {
          val saved = smartParsing
          smartParsing = true
          try body
          finally smartParsing = saved
        }

        def withPatches(patches: List[BracePatch]) = new RobustParser(source0, patches)

        val syntaxErrors = new ListBuffer[(Int, String)]
        val incompleteErrors = MutableSet.empty[String]

        override def syntaxError(offset: Offset, msg: String) {
          if (smartParsing) syntaxErrors += ((offset, msg))
          else super.syntaxError(offset, msg)
        }

        override def incompleteInputError(msg: String) {
          val braceMsg = List("{","}","(",")","[","]").exists(b => msg.startsWith("'" + b + "'"))
          if (smartParsing && (braceMsg || !incompleteErrors(msg))) {
            syntaxErrors += ((source.content.length - 1, msg))
            incompleteErrors += msg
          } else super.incompleteInputError(msg)
        }

        /** parse unit. If there are inbalanced braces, try to correct them and reparse. */
        def smartParse(): Tree = withSmartParsing {
          val firstTry = parse()
          if (syntaxErrors.isEmpty) firstTry
          else in.healBraces() match {
            case Nil      =>
              val (offset, msg) = syntaxErrors.head
              throw new MalformedInput(offset, msg)
            case patches  =>
              (this withPatches patches).parse()
          }
        }
      }

      /** Based on scalac's UnitScanner */
      class RobustScanner(parser: HoleParser, patches: List[BracePatch]) extends SourceFileScanner(parser.source) {
        def this(parser: HoleParser) = this(parser, Nil)

        private var bracePatches: List[BracePatch] = patches

        lazy val parensAnalyzer = new ParensAnalyzer(parser, Nil)

        override def parenBalance(token: Token) = parensAnalyzer.balance(token)

        override def healBraces(): List[BracePatch] = {
          var patches: List[BracePatch] = List()
          if (!parensAnalyzer.tabSeen) {
            var bal = parensAnalyzer.balance(RBRACE)
            while (bal < 0) {
              patches = new ParensAnalyzer(parser, patches).insertRBrace()
              bal += 1
            }
            while (bal > 0) {
              patches = new ParensAnalyzer(parser, patches).deleteRBrace()
              bal -= 1
            }
          }
          patches
        }

        /** Insert or delete a brace, if a patch exists for this offset */
        override def applyBracePatch(): Boolean = {
          if (bracePatches.isEmpty || bracePatches.head.off != offset) false
          else {
            val patch = bracePatches.head
            bracePatches = bracePatches.tail
            //        println("applying brace patch "+offset)//DEBUG
            if (patch.inserted) {
              next copyFrom this
              // TODO: warn ??
              // error(offset, "Missing closing brace `}' assumed here")
              token = RBRACE
              true
            } else {
              // TODO: warn ??
              // error(offset, "Unmatched closing brace '}' ignored here")
              fetchToken()
              false
            }
          }
        }

        override def skipComment(): Boolean = {
          val SU = '\u001A'
          if (ch == '/' || ch == '*') {
            val comment = new StringBuilder("/")
            def appendToComment() = comment.append(ch)

            if (ch == '/') {
              do {
                appendToComment()
                nextChar()
              } while ((ch != CR) && (ch != LF) && (ch != SU))
            } else {
              var openComments = 1
              appendToComment()
              nextChar()
              appendToComment()
              while (openComments > 0) {
                do {
                  do {
                    if (ch == '/') {
                      nextChar(); appendToComment()
                      if (ch == '*') {
                        nextChar(); appendToComment()
                        openComments += 1
                      }
                    }
                    if (ch != '*' && ch != SU) {
                      nextChar(); appendToComment()
                    }
                  } while (ch != '*' && ch != SU)
                  while (ch == '*') {
                    nextChar(); appendToComment()
                  }
                } while (ch != '/' && ch != SU)
                if (ch == '/') nextChar()
                else incompleteInputError("unclosed comment")
                openComments -= 1
              }
            }

            parser.comment(comment.toString, offset, charOffset - 2)
            true
          } else {
            false
          }
        }
      }

      /** Based on scalac's ParensAnalyzer */
      class ParensAnalyzer(parser: HoleParser, patches: List[BracePatch]) extends RobustScanner(parser, patches) {
        val balance = scala.collection.mutable.Map(RPAREN -> 0, RBRACKET -> 0, RBRACE -> 0)

        init()

        /** The offset of the first token on this line, or next following line if blank
         */
        val lineStart = new scala.collection.mutable.ArrayBuffer[Int]

        /** The list of matching top-level brace pairs (each of which may contain nested brace pairs).
         */
        val bracePairs: List[BracePair] = {

          var lineCount = 1
          var lastOffset = 0
          var indent = 0
          val oldBalance = scala.collection.mutable.Map[Int, Int]()
          def markBalance() = for ((k, v) <- balance) oldBalance(k) = v
          markBalance()

          def scan(bpbuf: ListBuffer[BracePair]): (Int, Int) = {
            if (token != NEWLINE && token != NEWLINES) {
              while (lastOffset < offset) {
                if (buf(lastOffset) == LF) lineCount += 1
                lastOffset += 1
              }
              while (lineCount > lineStart.length) {
                lineStart += offset
                // reset indentation unless there are new opening brackets or
                // braces since last ident line and at the same time there
                // are no new braces.
                if (balance(RPAREN) >= oldBalance(RPAREN) &&
                  balance(RBRACKET) >= oldBalance(RBRACKET) ||
                balance(RBRACE) != oldBalance(RBRACE)) {
                  indent = column(offset)
                  markBalance()
                }
              }
            }

            token match {
              case LPAREN =>
                balance(RPAREN) -= 1; nextToken(); scan(bpbuf)
              case LBRACKET =>
                balance(RBRACKET) -= 1; nextToken(); scan(bpbuf)
              case RPAREN =>
                balance(RPAREN) += 1; nextToken(); scan(bpbuf)
              case RBRACKET =>
                balance(RBRACKET) += 1; nextToken(); scan(bpbuf)
              case LBRACE =>
                balance(RBRACE) -= 1
                val lc = lineCount
                val loff = offset
                val lindent = indent
                val bpbuf1 = new ListBuffer[BracePair]
                nextToken()
                val (roff, rindent) = scan(bpbuf1)
                if (lc != lineCount)
                  bpbuf += BracePair(loff, lindent, roff, rindent, bpbuf1.toList)
                scan(bpbuf)
              case RBRACE =>
                balance(RBRACE) += 1
                val off = offset; nextToken(); (off, indent)
              case EOF =>
                (-1, -1)
              case _ =>
                nextToken(); scan(bpbuf)
            }
          }

          val bpbuf = new ListBuffer[BracePair]
          while (token != EOF) {
            val (roff, rindent) = scan(bpbuf)
            if (roff != -1) {
              val current = BracePair(-1, -1, roff, rindent, bpbuf.toList)
              bpbuf.clear()
              bpbuf += current
            }
          }
          def bracePairString(bp: BracePair, indent: Int): String = {
            val rangeString = {
              import bp._
              val lline = line(loff)
              val rline = line(roff)
              val tokens = List(lline, lindent, rline, rindent) map (n => if (n < 0) "??" else "" + n)
              "%s:%s to %s:%s".format(tokens: _*)
            }
            val outer  = (" " * indent) + rangeString
            val inners = bp.nested map (bracePairString(_, indent + 2))

            if (inners.isEmpty) outer
            else inners.mkString(outer + "\n", "\n", "")
          }
          def bpString    = bpbuf.toList map ("\n" + bracePairString(_, 0)) mkString ""
          def startString = lineStart.mkString("line starts: [", ", ", "]")

          log(s"\n$startString\n$bpString")
          bpbuf.toList
        }

        var tabSeen = false

        def line(offset: Offset): Int = {
          def findLine(lo: Int, hi: Int): Int = {
            val mid = (lo + hi) / 2
            if (offset < lineStart(mid)) findLine(lo, mid - 1)
            else if (mid + 1 < lineStart.length && offset >= lineStart(mid + 1)) findLine(mid + 1, hi)
            else mid
          }
          if (offset <= 0) 0
          else findLine(0, lineStart.length - 1)
        }

        def column(offset: Offset): Int = {
          var col = 0
          var i = offset - 1
          while (i >= 0 && buf(i) != CR && buf(i) != LF) {
            if (buf(i) == '\t') tabSeen = true
            col += 1
          i -= 1
          }
          col
        }

        def insertPatch(patches: List[BracePatch], patch: BracePatch): List[BracePatch] = patches match {
          case List() => List(patch)
          case bp :: bps => if (patch.off < bp.off) patch :: patches
          else bp :: insertPatch(bps, patch)
        }

        def insertRBrace(): List[BracePatch] = {
          def insert(bps: List[BracePair]): List[BracePatch] = bps match {
            case List() => patches
            case (bp @ BracePair(loff, lindent, roff, rindent, nested)) :: bps1 =>
              if (lindent <= rindent) insert(bps1)
              else {
                //           println("patch inside "+bp+"/"+line(loff)+"/"+lineStart(line(loff))+"/"+lindent"/"+rindent)//DEBUG
                val patches1 = insert(nested)
                if (patches1 ne patches) patches1
                else {
                  var lin = line(loff) + 1
                  while (lin < lineStart.length && column(lineStart(lin)) > lindent)
                    lin += 1
                    if (lin < lineStart.length) {
                      val patches1 = insertPatch(patches, BracePatch(lineStart(lin), inserted = true))
                      //println("patch for "+bp+"/"+imbalanceMeasure+"/"+new ParensAnalyzer(unit, patches1).imbalanceMeasure)
                      /*if (improves(patches1))*/
                     patches1
                     /*else insert(bps1)*/
                    // (this test did not seem to work very well in practice)
                    } else patches
                }
              }
          }
          insert(bracePairs)
        }

        def deleteRBrace(): List[BracePatch] = {
          def delete(bps: List[BracePair]): List[BracePatch] = bps match {
            case List() => patches
            case BracePair(loff, lindent, roff, rindent, nested) :: bps1 =>
              if (lindent >= rindent) delete(bps1)
              else {
                val patches1 = delete(nested)
                if (patches1 ne patches) patches1
                else insertPatch(patches, BracePatch(roff, inserted = false))
              }
          }
          delete(bracePairs)
        }

        // don't emit deprecation warnings about identifiers like `macro` or `then`
        // when skimming through the source file trying to heal braces
        override def emitIdentifierDeprecationWarnings = false

        override def error(offset: Offset, msg: String) {}
      }
    }

    object Normalizer extends (ast.AST => ast.AST) {

      private object PrimitiveExtractor {
        def unapply(tpe: ast.Type): Option[String] = tpe match {
          case ast.ClassType(ast.Ident("scala"), name, Nil, Nil) => Some(name)
          case ast.ClassType(ast.Empty.NoExpr, name, Nil, Nil) => Some(name)
          case _ => None
        }
      }

      private val FUNCTION_NAME: String = tpnme.QUASIQUOTE_FUNCTION.toString
      private def isOperator(op: String): Boolean = op != newTermName(op).encode.toString

      def apply(a: ast.AST): ast.AST = a.transform {
        case PrimitiveExtractor("Unit") => Some(ast.PrimitiveTypes.Void)
        case PrimitiveExtractor("String") => Some(ast.PrimitiveTypes.String)
        case PrimitiveExtractor("Boolean") => Some(ast.PrimitiveTypes.Boolean)
        case PrimitiveExtractor("Char") => Some(ast.PrimitiveTypes.Char)
        case PrimitiveExtractor("Byte") => Some(ast.PrimitiveTypes.Byte)
        case PrimitiveExtractor("Short") => Some(ast.PrimitiveTypes.Short)
        case PrimitiveExtractor("Int") => Some(ast.PrimitiveTypes.Int)
        case PrimitiveExtractor("Long") => Some(ast.PrimitiveTypes.Long)
        case PrimitiveExtractor("Float") => Some(ast.PrimitiveTypes.Float)
        case PrimitiveExtractor("Double") => Some(ast.PrimitiveTypes.Double)
        case ct @ ast.ClassType(ast.Empty.NoExpr, FUNCTION_NAME, Nil, tparams) if tparams.nonEmpty =>
          Some(ast.FunctionType(tparams.init, tparams.last).fromAST(ct))
        case fc @ ast.FunctionCall(ast.FieldAccess(e1, op, Nil), Nil, List(e2)) if isOperator(op) =>
          if (op.endsWith("=")) {
            val init: String = op.init
            Some(ast.Assign(e1, e2, if (init != "") Some(init) else None).fromAST(fc))
          } else Some(ast.BinaryOp(e1, op, e2).fromAST(fc))
        case _ => None
      }
    }

    def parse(source: Source) = {
      val sourceFile = new scala.reflect.internal.util.BatchSourceFile(NoFile, source.contents)
      val tree = parser.parse(sourceFile)

      def p2p(pos: Position): ast.Position = pos match {
        case r: RangePosition =>
          val (startLine,startCol) = source.offsetCoords(r.start)
          val (endLine,endCol) = source.offsetCoords(r.end)
          source.position(startLine, startCol, endLine, endCol)
        case NoPosition =>
          source.position(0)
        case p =>
          source.position(p.point)
      }

      def withPos[T <: ast.AST](tree: Tree)(t: ast.Position => T): T = {
        val pos = p2p(tree.pos)
        val res = t(pos).setPos(pos)
        val comment = tree.attachments.get[Comment].map(_.value)
        comment.foreach(res.appendComment)
        res
      }

      def withPoss[T <: ast.AST](tree: Tree)(t: ast.Position => List[T]): List[T] = {
        val pos = p2p(tree.pos)
        val res = t(pos).map(_.setPos(pos))
        val comment = tree.attachments.get[Comment].map(_.value)
        if (res.nonEmpty && comment.isDefined) res.head.appendComment(comment.get)
        res
      }

      def extractName(tree: Tree): String = tree match {
        case r: RefTree => extractName(r.qualifier) match {
          case "" => r.name.decode
          case q => q + "." + r.name.decode
        }
        case EmptyTree => ""
        case _ => tree.toString
      }

      def extractModifiers(mods: Modifiers, pos: ast.Position): (ast.Modifiers, List[ast.Annotation]) = {
        import ast.Modifiers
        import Modifiers.NoModifiers

        val modifiers = (if (mods.hasFlag(Flag.PROTECTED)) Modifiers.PROTECTED else NoModifiers) |
          (if (mods.privateWithin != tpnme.EMPTY || mods.hasFlag(Flag.PRIVATE)) Modifiers.PRIVATE else NoModifiers) |
          (if (mods.hasFlag(Flag.FINAL) || !mods.hasFlag(Flag.MUTABLE)) Modifiers.FINAL else NoModifiers) |
          (if (mods.hasFlag(Flag.DEFERRED) || mods.hasFlag(Flag.ABSTRACT)) Modifiers.ABSTRACT else NoModifiers)

        def annotationArgs(args: List[ast.Expr]): Map[String, ast.Expr] = Map(args.map {
          case ast.Assign(ast.Ident(name), rhs, _) => name -> rhs
          case ast.Assign(lhs, rhs, _) => lhs.toString -> rhs
          case expr => Names.DEFAULT -> expr
        } : _*)

        val annots = (if (mods.hasFlag(Flag.OVERRIDE))
            List(ast.Annotation(Names.OVERRIDE_ANNOTATION).setPos(pos))
          else
            Nil) ++ mods.annotations.flatMap(a => extractExpr(a) match {
              case cc @ ast.ConstructorCall(ct @ ast.ClassType(ast.Empty.NoExpr, name, Nil, Nil), args, Nil) =>
                List(ast.Annotation(name, annotationArgs(args)).fromAST(ct).fromAST(cc))
              case cc @ ast.ConstructorCall(ct : ast.ClassType, args, Nil) =>
                List(ast.Annotation(ct.toString, annotationArgs(args)).fromAST(ct).fromAST(cc))
              case _ => Nil
            })

        (modifiers, annots)
      }

      def extractTypeDef(td: TypeDef): ast.TypeDef = withPos(td) { pos =>
        val name = td.name.decode
        val tparams = td.tparams.map(extractTypeDef)
        val (modifiers, annotations) = extractModifiers(td.mods, pos)
        td.rhs match {
          case TypeBoundsTree(lo, hi) =>
            val (low, high) = (extractType(lo), extractType(hi))
            ast.TypeDef(modifiers, name, annotations, tparams, List(low), List(high)).setPos(pos)
          case tpt =>
            ast.TypeDef(modifiers, name, annotations, tparams, Nil, List(extractType(tpt))).setPos(pos)
        }
      }

      def extractTemplate(template: Template): (Option[String], List[ast.Type], List[ast.Definition], List[ast.Statement]) = {
        val parents = template.parents.map(extractType)
        val selfTpes = if (template.self.name == nme.WILDCARD && template.self.tpt.isInstanceOf[TypeTree]) Nil else {
          val vd = extractDef(template.self).asInstanceOf[ast.ValDef]
          List(vd.copy(modifiers = vd.modifiers | ast.Modifiers.PRIVATE | ast.Modifiers.FINAL).fromAST(vd))
        }
        val body = template.body.flatMap(extractStmts)
        val (definitions, statements) = body.partition(_.isInstanceOf[ast.Definition])
        (template.attachments.get[Comment].map(_.value), parents, selfTpes ++ definitions.map(_.asInstanceOf[ast.Definition]), statements)
      }

      def wrapStatements(tree: Tree, asts: List[ast.Statement]): ast.Block = asts match {
        case Nil => ast.Empty.NoStmt
        case (b: ast.Block) :: Nil => b
        case stmts =>
          val p = if (stmts.head.pos != ast.NoPosition) stmts.head.pos else p2p(tree.pos)
          ast.Block(stmts).setPos(p)
      }

      def extractType(tpt: Tree): ast.Type = withPos(tpt)(pos => tpt match {
        case TypeTree() => ast.Empty.NoType

        case Ident(name) =>
          if (name.decode == Names.DEFAULT) ast.Empty.NoType
          else ast.ClassType(ast.Empty.NoExpr, name.decode, Nil, Nil)

        case Select(scope, name) =>
          ast.ClassType(extractExpr(scope), name.decode, Nil, Nil)

        case AppliedTypeTree(tpt, args) => extractType(tpt) match {
          case ct @ ast.ClassType(scope, name, annotations, params) =>
            ast.ClassType(scope, name, annotations, params ++ args.map(extractType)).fromAST(ct)
          case tpe => tpe.appendComment(args.map(_.toString).mkString("[",",","]"))
        }

        case CompoundTypeTree(templ) =>
          val (comment, parents, defs, init) = extractTemplate(templ)
          ast.ComplexType(parents, defs, ast.Empty.NoExpr).appendComment(comment)

        case SingletonTypeTree(tpe) =>
          ast.ComplexType(Nil, Nil, extractExpr(tpe))

        case ExistentialTypeTree(tpt, clauses) =>
          ast.ComplexType(extractType(tpt) :: Nil, clauses.map(extractDef), ast.Empty.NoExpr)

        case SelectFromTypeTree(qualifier, name) =>
          ast.ComplexType(extractType(qualifier) :: Nil, Nil, ast.Ident(name.decode).setPos(pos))

        case Annotated(annot, arg) =>
          extractType(arg).appendComment(annot.toString)

        case o => throw ParsingFailedError(new RuntimeException(s"Unknown type tree [${o.getClass}]: $o"))
      })

      def extractDef(tree: Tree): ast.Definition = withPos(tree)(pos => tree match {
        case PackageDef(pid, stats) =>
          val (imports, defs) = stats.flatMap(extractStmts).partition(_.isInstanceOf[ast.Import])
          ast.PackageDef(extractName(pid), Nil, imports.map(_.asInstanceOf[ast.Import]), defs.map(_.asInstanceOf[ast.Definition]))

        case ClassDef(mods, name, tparams, impl) =>
          val sort = if (mods.hasFlag(Flag.TRAIT) || mods.hasFlag(Flag.INTERFACE)) ast.TraitSort else ast.ClassSort
          val (modifiers, annotations) = extractModifiers(mods, pos)
          val (comment, parentTypes, definitions, body) = extractTemplate(impl)
          val parents = parentTypes.collect { case tpe : ast.ClassType => tpe }
          val init = if (body.isEmpty) Nil else List(ast.Initializer(false, Nil, ast.Block(body).setPos(pos)).setPos(pos))
          ast.ClassDef(modifiers, name.decode, annotations, tparams.map(extractTypeDef), parents, init ++ definitions, sort).appendComment(comment)

        case ModuleDef(mods, name, impl) =>
          val (modifiers, annotations) = extractModifiers(mods, pos)
          val (comment, parentTypes, definitions, body) = extractTemplate(impl)
          val parents = parentTypes.collect { case tpe : ast.ClassType => tpe }

          val init = if (body.isEmpty) Nil else List(ast.Initializer(true, Nil, ast.Block(body).setPos(pos)).setPos(pos))
          val staticDefs = init ++ definitions.map {
            case cd: ast.ClassDef => cd.copy(modifiers = cd.modifiers | ast.Modifiers.STATIC).fromAST(cd)
            case fd: ast.FunctionDef => fd.copy(modifiers = fd.modifiers | ast.Modifiers.STATIC).fromAST(fd)
            case vd: ast.ValDef => vd.copy(modifiers = vd.modifiers | ast.Modifiers.STATIC).fromAST(vd)
            case d => d
          }
          ast.ClassDef(modifiers | ast.Modifiers.FINAL, name.decode, annotations, Nil, parents, staticDefs).appendComment(comment)

        case build.SyntacticPatDef(mods, pat, tpt, rhs) =>
          val (modifiers, annotations) = extractModifiers(mods, pos)
          val pattern = extractExpr(pat)
          val value = extractExpr(rhs)
          extractType(tpt) match {
            case ast.Empty.NoType => ast.ExtractionValDef(modifiers, pattern, annotations, value)
            case tpe => ast.ExtractionValDef(modifiers, ast.InstanceOf(pattern, tpe).setPos(pattern.pos), annotations, value)
          }

        case ValDef(mods, name, tpt, rhs) =>
          val (modifiers, annotations) = extractModifiers(mods, pos)
          val (tpe, varArgs) = extractType(tpt) match {
            case ast.ClassType(ast.FieldAccess(ast.Ident("_root_"), "scala", Nil), "<repeated>", Nil, List(tpe)) =>
              (tpe, true)
            case tpe =>
              (tpe, false)
          }
          ast.ValDef(modifiers, name.decode, annotations, tpe, extractExpr(rhs), varArgs)

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          val (modifiers, annotations) = extractModifiers(mods, pos)
          val params = vparamss.flatten.map(vd => extractDef(vd).asInstanceOf[ast.ValDef])
          val body = wrapStatements(tree, extractStmts(rhs))
          if (name.decode == "<init>")
            ast.ConstructorDef(modifiers, annotations, tparams.map(extractTypeDef), params, body)
          else
            ast.FunctionDef(modifiers, name.decode, annotations, tparams.map(extractTypeDef), params, extractType(tpt), body)

        case td @ TypeDef(mods, name, tparams, rhs) => extractTypeDef(td)

        case o => throw ParsingFailedError(new RuntimeException(s"Unknown definition tree [${o.getClass}]: $o"))
      })

      def extractStmts(tree: Tree): List[ast.Statement] = withPoss(tree)(pos => tree match {
        case Import(expr, selectors) =>
          val name = extractName(expr)
          selectors.map { s =>
            val (path, asterisk) = if (s.name == nme.WILDCARD) {
              (name, true)
            } else {
              (name + "." + s.name, false)
            }
            ast.Import(path, asterisk, true)
          }

        case build.SyntacticFor(_, _) => List(extractExpr(tree))

        case build.SyntacticForYield(_, _) => List(extractExpr(tree))

        case Apply(fun, args) => List(extractExpr(fun) match {
          case a @ ast.FieldAccess(s @ ast.Super(qual), nme.CONSTRUCTOR, tparams) =>
            val call = ast.SuperCall(qual, tparams, args.map(extractExpr)).fromAST(a)
            s.comment.foreach(call.appendComment)
            call
          case a @ ast.FieldAccess(s @ ast.This(qual), nme.CONSTRUCTOR, tparams) =>
            val call = ast.ThisCall(qual, tparams, args.map(extractExpr)).fromAST(a)
            s.comment.foreach(call.appendComment)
            call
          case caller => extractApply(tree, caller, args.map(extractExpr))
        })

        case LabelDef(nm1, _, ife @ If(cond, Block(stats, Apply(Ident(nm2), Nil)), _)) if nm1 == nm2 =>
          List(ast.While(extractExpr(cond), wrapStatements(ife, stats.flatMap(extractStmts))))

        case LabelDef(nm1, _, b @ Block(stats, If(cond, Apply(Ident(nm2), Nil), _))) if nm1 == nm2 =>
          List(ast.Do(extractExpr(cond), wrapStatements(b, stats.flatMap(extractStmts))))

        case ld : LabelDef => throw ParsingFailedError(new RuntimeException(s"Unexpected label-def: $ld"))

        case t: TermTree => extractExpr(t) match {
          case ast.Empty.NoExpr => Nil
          case expr => List(expr)
        }

        case r: RefTree => extractExpr(r) match {
          case ast.Empty.NoExpr => Nil
          case expr => List(expr)
        }

        case d: DefTree => extractDef(d) match {
          case ast.Empty.NoDef => Nil
          case definition => List(definition)
        }

        case Annotated(annot, arg) =>
          extractStmts(arg) match {
            case x :: xs => x.appendComment(annot.toString) :: xs
            case _ => Nil
          }

        case o => throw ParsingFailedError(new RuntimeException(s"Unknown statement tree [${o.getClass}]: $o"))
      })

      def extractApply(tree: Tree, fun: ast.Expr, args: List[ast.Expr]): ast.Expr = withPos(tree)(pos => fun match {
        case a @ ast.FieldAccess(cc : ast.ConstructorCall, "<init>", tparams) =>
          cc.copy(args = cc.args ++ args).fromAST(cc).fromAST(a)
        case a @ ast.FieldAccess(receiver, name, tparams) =>
          ast.FunctionCall(ast.FieldAccess(receiver, name, Nil).fromAST(a), tparams, args)
        case caller => ast.FunctionCall(caller, Nil, args)
      })

      def extractExpr(tree: Tree): ast.Expr = {

        def extractFors(enums: List[Tree], body: ast.Expr, generator: Boolean): ast.Expr = enums match {
          case Nil => body
          case (a @ Apply(Ident(nme.LARROWkw), (b @ Bind(name, expr)) :: iterable :: Nil)) :: xs =>
            val inner = extractFors(xs, body, generator)
            val valDef = withPos(b)(_ => ast.ValDef(ast.Modifiers.FINAL, name.decode, Nil, ast.Empty.NoType, ast.Empty.NoExpr))
            withPos(a)(pos => expr match {
              case id @ Ident(nme.WILDCARD) =>
                ast.Foreach(valDef :: Nil, extractExpr(iterable), inner, generator)
              case _ =>
                val extractor = ast.ExtractionValDef(ast.Modifiers.FINAL, extractExpr(expr), Nil, ast.Empty.NoExpr).setPos(pos)
                val block = inner match {
                  case ast.Block(stmts) => ast.Block(extractor :: stmts).fromAST(inner)
                  case stmt => ast.Block(extractor :: stmt :: Nil).fromAST(inner)
                }
                ast.Foreach(valDef :: Nil, extractExpr(iterable), block, generator)
            })
          case (a @ Assign(e1, e2)) :: xs => withPos(a) { _ =>
            val inner = extractFors(xs, body, generator)
            val rhs = extractExpr(e2)
            ast.Block(List(extractExpr(e1) match {
              case b @ ast.Bind(name, ast.Wildcard) =>
                ast.ValDef(ast.Modifiers.FINAL, name, Nil, ast.Empty.NoType, rhs).fromAST(b)
              case expr =>
                ast.ExtractionValDef(ast.Modifiers.FINAL, expr, Nil, rhs).fromAST(expr)
            }, inner))
          }

          case _ :: xs => extractFors(xs, body, generator)
        }

        withPos(tree)(pos => tree match {
          case Block(stats, expr) =>
            ast.Block(stats.flatMap(extractStmts) ++ extractStmts(expr).flatMap {
              case ast.Block(stmts) => stmts
              case s => List(s)
            })

          case Try(block, catches, finalizer) =>
            ast.Try(wrapStatements(block, extractStmts(block)), catches.map { case c @ CaseDef(pat, guard, body) =>
              val guarded = withPos(c)(_ => ast.Guarded(extractExpr(pat), extractExpr(guard)))
              val valDef = ast.ExtractionValDef(ast.Modifiers.NoModifiers, guarded, Nil, ast.Empty.NoExpr).setPos(guarded.pos)
              val block = wrapStatements(c, extractStmts(body))
              valDef -> block
            }, wrapStatements(finalizer, extractStmts(finalizer)))

          case Function(vparams, body) =>
            ast.FunctionLiteral(vparams.map(p => extractDef(p).asInstanceOf[ast.ValDef]), ast.Empty[ast.Type], extractExpr(body))

          case Assign(left, right) =>
            ast.Assign(extractExpr(left), extractExpr(right), None)

          case If(cond, thenn, elsep) =>
            ast.If(extractExpr(cond), extractExpr(thenn), extractExpr(elsep))

          case Match(selector, cases) =>
            ast.Switch(extractExpr(selector), cases.map { case c @ CaseDef(pat, guard, body) =>
              val guarded = withPos(c)(_ => ast.Guarded(extractExpr(pat), extractExpr(guard)))
              val block = wrapStatements(c, extractStmts(body))
              guarded -> block
            })

          case build.SyntacticFor(enums, body) => extractFors(enums, extractExpr(body), false)

          case build.SyntacticForYield(enums, body) => extractFors(enums, extractExpr(body), true)

          case Apply(fun, args) => extractApply(tree, extractExpr(fun), args.map(extractExpr))

          case TypeApply(fun, args) =>
            val tpArgs = args.map(extractType)
            extractExpr(fun) match {
              case a : ast.FieldAccess => a.copy(tparams = a.tparams ++ tpArgs).fromAST(a)
              case n : ast.ConstructorCall => n.copy(tpe = n.tpe.copy(tparams = n.tpe.tparams ++ tpArgs).fromAST(n.tpe)).fromAST(n)
              case caller => ast.FunctionCall(caller, tpArgs, Nil)
            }

          case Select(receiver, name) =>
            ast.FieldAccess(extractExpr(receiver), name.decode, Nil)

          case Super(qual, mix) =>
            ast.Super(extractExpr(qual) match {
              case ast.This(id) => id
              case q => q
            })

          case This(qual) =>
            ast.This(if (qual == tpnme.EMPTY) ast.Empty.NoExpr else ast.Ident(qual.decode).setPos(pos))

          case Ident(name) =>
            if (name == nme.WILDCARD) ast.Wildcard
            else ast.Ident(name.decode)

          case Bind(name, body) =>
            ast.Bind(name.decode, extractExpr(body))

          case Literal(c) if c.tag == UnitTag => ast.VoidLiteral

          case Literal(c) if c.tag == NullTag => ast.NullLiteral

          case Literal(c) => ast.SimpleLiteral(c.tag match {
            case NoTag => ast.PrimitiveTypes.Special("no type")
            case BooleanTag => ast.PrimitiveTypes.Boolean
            case ByteTag => ast.PrimitiveTypes.Byte
            case ShortTag => ast.PrimitiveTypes.Short
            case CharTag => ast.PrimitiveTypes.Char
            case IntTag => ast.PrimitiveTypes.Int
            case LongTag => ast.PrimitiveTypes.Long
            case FloatTag => ast.PrimitiveTypes.Float
            case DoubleTag => ast.PrimitiveTypes.Double
            case StringTag => ast.PrimitiveTypes.String
            case ClazzTag => ast.PrimitiveTypes.Special("class")
            case EnumTag => ast.PrimitiveTypes.Special("enum")
          }, c.stringValue)

          case New(tpt) => ast.ConstructorCall(extractType(tpt).asInstanceOf[ast.ClassType], Nil, Nil)

          case Throw(expr) => ast.Throw(extractExpr(expr))

          case Alternative(trees) => ast.MultiLiteral(trees.map(extractExpr))

          case Annotated(annot, arg) =>
            extractExpr(arg).appendComment(annot.toString)

          case Typed(expr, tpt) =>
            val (pat, guard) = extractExpr(expr) match {
              case b @ ast.Bind(name, _) => b -> ast.Ident(name).setPos(b.pos)
              case id: ast.Ident => id -> id
              case w @ ast.Wildcard => w -> w
              case expr =>
                val bound = ast.Bind("typed", expr)
                val p = if (expr.pos != ast.NoPosition) expr.pos else pos
                bound.setPos(p) -> ast.Ident("typed").setPos(p)
            }
            val p = if (guard.pos != ast.NoPosition) guard.pos else pos
            ast.Guarded(pat, ast.InstanceOf(guard, extractType(tpt)).setPos(p))

          case Star(elem) => ast.UnaryOp(extractExpr(elem), "*", true)

          case AssignOrNamedArg(left, right) =>
            ast.Assign(extractExpr(left), extractExpr(right), None)

          // While loops can take place in expression positions in Scala
          case ld: LabelDef => ast.Block(extractStmts(ld) :+ ast.VoidLiteral)

          case EmptyTree => ast.Empty.NoExpr

          case o => throw ParsingFailedError(new RuntimeException(s"Unknown expression tree [${o.getClass}]: $o"))
        })
      }

      Normalizer(tree match {
        case t: TypTree => extractType(t)
        case _ => extractStmts(tree) match {
          case List(x) => x
          case Nil => ast.Empty[ast.Statement]
          case list => ast.Block(list).setPos(p2p(tree.pos))
        }
      })
    }
  }

}
