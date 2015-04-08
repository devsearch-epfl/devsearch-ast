package devsearch.parsers

import devsearch.ast
import devsearch.ast.{Source, Names, AST}

import scala.tools.nsc.interactive._
import scala.tools.nsc.ast.parser.{Parsers => ScalaParser}
import scala.reflect.internal.util.{Position => _, _}

import org.scalamacros.paradise.parser.Tokens._
import org.scalamacros.paradise.quasiquotes._

object QueryParser extends Parser {

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

    object parser extends Parser {
      def entryPoint = parser => Q(implodePatDefs(gen.mkTreeOrBlock(parser.templateOrTopStatSeq())))

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
          scala.util.Try {
            val parser = new HoleParser(source)
            parser.checkNoEscapingPlaceholders { parser.parseRule(entryPoint) }
          }.orElse(scala.util.Try {
            new HoleParser(source).compilationUnit()
          }).get
        } catch {
          case mi: MalformedInput =>
            if (posMap.isEmpty) c.abort(NoPosition, "Unparsable stuff: " + mi.msg)
            else c.abort(correspondingPosition(mi.offset), mi.msg)
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

        override def isHole: Boolean = in.token == USCORE
        override def isHole(name: Name): Boolean = name.toString == "_"

        override def ident(skipIt: Boolean): Name =
          if (in.name.toString == "_") {
            in.nextToken()
            dfltName 
          } else super.ident(skipIt)
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

      def extractPosition(pos: Position): ast.Position = pos match {
        case r: RangePosition =>
          val (startLine,startCol) = source.offsetCoords(r.start)
          val (endLine,endCol) = source.offsetCoords(r.end)
          source.position(startLine, startCol, endLine, endCol)
        case NoPosition => source.position(0)
        case _ => source.position(pos.point)
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

        val annots = (if (mods.hasFlag(Flag.OVERRIDE))
            List(ast.Annotation(Names.OVERRIDE_ANNOTATION).setPos(pos))
          else
            Nil) ++ mods.annotations.flatMap(a => extractExpr(a) match {
              case _ => Nil
            })

        (modifiers, annots)
      }

      def extractTypeDef(td: TypeDef): ast.TypeDef = {
        val name = td.name.decode
        val pos = extractPosition(td.pos)
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

      def extractTemplate(template: Template): (List[ast.Type], List[ast.Definition], List[ast.Statement]) = {
        val parents = template.parents.map(extractType)
        val selfTpes = if (template.self.name == nme.WILDCARD && template.self.tpt == NoType) Nil else {
          val vd = extractDef(template.self).asInstanceOf[ast.ValDef]
          List(vd.copy(modifiers = vd.modifiers | ast.Modifiers.PRIVATE | ast.Modifiers.FINAL))
        }
        val body = template.body.flatMap(extractStmts)
        val (definitions, statements) = body.partition(_.isInstanceOf[ast.Definition])
        (parents, selfTpes ++ definitions.map(_.asInstanceOf[ast.Definition]), statements)
      }

      def wrapStatements(asts: List[ast.Statement]): ast.Block = asts match {
        case Nil => ast.Empty.NoStmt
        case (b: ast.Block) :: Nil => b
        case stmts => ast.Block(stmts).setPos(stmts.head.pos)
      }

      def extractType(tpt: Tree): ast.Type = {
        val pos = extractPosition(tpt.pos)

        val extracted = tpt match {
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
            val (parents, defs, init) = extractTemplate(templ)
            ast.ComplexType(parents, defs, ast.Empty.NoExpr)

          case SingletonTypeTree(tpe) =>
            ast.ComplexType(Nil, Nil, extractExpr(tpe))

          case ExistentialTypeTree(tpt, clauses) =>
            ast.ComplexType(extractType(tpt) :: Nil, clauses.map(extractDef), ast.Empty.NoExpr)

          case SelectFromTypeTree(qualifier, name) =>
            val pos = extractPosition(tree.pos)
            ast.ComplexType(extractType(qualifier) :: Nil, Nil, ast.Ident(name.decode).setPos(pos))

          case Annotated(annot, arg) =>
            extractType(arg).appendComment(annot.toString)

          case o => throw ParsingFailedError(new RuntimeException(s"Unknown type tree [${o.getClass}]: $o"))
        }

        extracted.setPos(pos)
      }

      def extractDef(tree: Tree): ast.Definition = {
        val pos = extractPosition(tree.pos)

        val extracted: ast.Definition = tree match {
          case PackageDef(pid, stats) =>
            val (imports, defs) = stats.flatMap(extractStmts).partition(_.isInstanceOf[ast.Import])
            ast.PackageDef(extractName(pid), Nil, imports.map(_.asInstanceOf[ast.Import]), defs.map(_.asInstanceOf[ast.Definition]))

          case ClassDef(mods, name, tparams, impl) =>
            val sort = if (mods.hasFlag(Flag.TRAIT) || mods.hasFlag(Flag.INTERFACE)) ast.TraitSort else ast.ClassSort
            val (modifiers, annotations) = extractModifiers(mods, pos)
            val (parentTypes, definitions, body) = extractTemplate(impl)
            val parents = parentTypes.collect { case tpe : ast.ClassType => tpe }
            val init = if (body.isEmpty) Nil else List(ast.Initializer(false, Nil, ast.Block(body).setPos(pos)).setPos(pos))
            ast.ClassDef(modifiers, name.decode, annotations, tparams.map(extractTypeDef), parents, init ++ definitions, sort)

          case ModuleDef(mods, name, impl) =>
            val (modifiers, annotations) = extractModifiers(mods, pos)
            val (parentTypes, definitions, body) = extractTemplate(impl)
            val parents = parentTypes.collect { case tpe : ast.ClassType => tpe }

            val init = if (body.isEmpty) Nil else List(ast.Initializer(true, Nil, ast.Block(body).setPos(pos)).setPos(pos))
            val staticDefs = init ++ definitions.map {
              case cd: ast.ClassDef => cd.copy(modifiers = cd.modifiers | ast.Modifiers.STATIC).fromAST(cd)
              case fd: ast.FunctionDef => fd.copy(modifiers = fd.modifiers | ast.Modifiers.STATIC).fromAST(fd)
              case vd: ast.ValDef => vd.copy(modifiers = vd.modifiers | ast.Modifiers.STATIC).fromAST(vd)
              case d => d
            }
            ast.ClassDef(modifiers | ast.Modifiers.FINAL, name.decode, annotations, Nil, parents, staticDefs)

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
            val body = wrapStatements(extractStmts(rhs))
            ast.FunctionDef(modifiers, name.decode, annotations, tparams.map(extractTypeDef), params, extractType(tpt), body)

          case td @ TypeDef(mods, name, tparams, rhs) => extractTypeDef(td)

          case o => throw ParsingFailedError(new RuntimeException(s"Unknown definition tree [${o.getClass}]: $o"))
        }

        extracted.setPos(pos)
      }

      def extractStmts(tree: Tree): List[ast.Statement] = {
        val pos = extractPosition(tree.pos)

        val extracted: List[ast.Statement] = tree match {
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

          case Apply(fun, args) => List(extractExpr(fun) match {
            case a @ ast.FieldAccess(s @ ast.Super(qual), nme.CONSTRUCTOR, tparams) =>
              val call = ast.SuperCall(qual, tparams, args.map(extractExpr)).fromAST(a)
              s.comment.foreach(call.appendComment)
              call
            case a @ ast.FieldAccess(s @ ast.This(qual), nme.CONSTRUCTOR, tparams) =>
              val call = ast.ThisCall(qual, tparams, args.map(extractExpr)).fromAST(a)
              s.comment.foreach(call.appendComment)
              call
            case _ => extractExpr(tree)
          })

          case LabelDef(nm1, _, If(cond, Block(stats, Apply(Ident(nm2), Nil)), _)) if nm1 == nm2 =>
            List(ast.While(extractExpr(cond), wrapStatements(stats.flatMap(extractStmts))))

          case LabelDef(nm1, _, Block(stats, If(cond, Apply(Ident(nm2), Nil), _))) if nm1 == nm2 =>
            List(ast.Do(extractExpr(cond), wrapStatements(stats.flatMap(extractStmts))))

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
        }

        extracted.map(_.setPos(pos))
      }

      def extractExpr(tree: Tree): ast.Expr = {
        val pos = extractPosition(tree.pos)

        def extractFors(enums: List[Tree], body: ast.Expr, generator: Boolean): ast.Expr = enums match {
          case Nil => body
          case Apply(Ident(nme.LARROWkw), Bind(name, expr) :: iterable :: Nil) :: xs =>
            val inner = extractFors(xs, body, generator)
            val valDef = ast.ValDef(ast.Modifiers.FINAL, name.decode, Nil, ast.Empty.NoType, ast.Empty.NoExpr)
            expr match {
              case Ident(nme.WILDCARD) =>
                ast.Foreach(valDef :: Nil, extractExpr(iterable), inner, generator)
              case _ =>
                val extractor = ast.ExtractionValDef(ast.Modifiers.FINAL, extractExpr(expr), Nil, ast.Empty.NoExpr)
                val block = inner match {
                  case ast.Block(stmts) => ast.Block(extractor :: stmts).fromAST(inner)
                  case stmt => ast.Block(extractor :: stmt :: Nil).fromAST(inner)
                }
                ast.Foreach(valDef :: Nil, extractExpr(iterable), block, generator)
            }
          case (a @ Assign(e1, e2)) :: xs =>
            val inner = extractFors(xs, body, generator)
            val rhs = extractExpr(e2)
            ast.Block(List(extractExpr(e1) match {
              case b @ ast.Bind(name, ast.Wildcard) =>
                ast.ValDef(ast.Modifiers.FINAL, name, Nil, ast.Empty.NoType, rhs).fromAST(b)
              case expr =>
                ast.ExtractionValDef(ast.Modifiers.FINAL, expr, Nil, rhs).fromAST(expr)
            }, inner)).setPos(extractPosition(a.pos))

          case _ :: xs => extractFors(xs, body, generator)
        }

        val extracted: ast.Expr = tree match {
          case Block(stats, expr) =>
            ast.Block(stats.flatMap(extractStmts) ++ extractStmts(expr).flatMap {
              case ast.Block(stmts) => stmts
              case s => List(s)
            })

          case Try(block, catches, finalizer) =>
            ast.Try(wrapStatements(extractStmts(block)), catches.map { case c @ CaseDef(pat, guard, body) =>
              val guarded = ast.Guarded(extractExpr(pat), extractExpr(guard)).setPos(extractPosition(c.pos))
              val block = wrapStatements(extractStmts(body))
              guarded -> block
            }, wrapStatements(extractStmts(finalizer)))

          case Function(vparams, body) =>
            ast.FunctionLiteral(vparams.map(p => extractDef(p).asInstanceOf[ast.ValDef]), ast.Empty[ast.Type], extractExpr(body))

          case Assign(left, right) =>
            ast.Assign(extractExpr(left), extractExpr(right), None)

          case If(cond, thenn, elsep) =>
            ast.If(extractExpr(cond), extractExpr(thenn), extractExpr(elsep))

          case Match(selector, cases) =>
            ast.Switch(extractExpr(selector), cases.map { case c @ CaseDef(pat, guard, body) =>
              val guarded = ast.Guarded(extractExpr(pat), extractExpr(guard)).setPos(extractPosition(c.pos))
              val block = wrapStatements(extractStmts(body))
              guarded -> block
            })

          case build.SyntacticFor(enums, body) => extractFors(enums, extractExpr(body), false)

          case build.SyntacticForYield(enums, body) => extractFors(enums, extractExpr(body), true)

          case Apply(fun, args) => extractExpr(fun) match {
            case a @ ast.FieldAccess(receiver, name, tparams) =>
              ast.FunctionCall(ast.FieldAccess(receiver, name, Nil).fromAST(a), tparams, args.map(extractExpr))
            case n : ast.ConstructorCall => n.copy(args = n.args ++ args.map(extractExpr)).fromAST(n)
            case caller => ast.FunctionCall(caller, Nil, args.map(extractExpr))
          }

          case TypeApply(fun, args) =>
            val tpArgs = args.map(extractType)
            extractExpr(fun) match {
              case a : ast.FieldAccess => a.copy(tparams = a.tparams ++ tpArgs)
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
            val bound @ ast.Bind(name, _) = extractExpr(expr) match {
              case b: ast.Bind => b
              case expr => ast.Bind(Names.DEFAULT, expr).setPos(expr.pos)
            }
            ast.Guarded(bound, ast.InstanceOf(ast.Ident(name).setPos(bound.pos), extractType(tpt)).setPos(bound.pos))

          case Star(elem) => ast.UnaryOp(extractExpr(elem), "*", true)

          // While loops can take place in expression positions in Scala
          case ld: LabelDef => ast.Block(extractStmts(ld) :+ ast.VoidLiteral)

          case EmptyTree => ast.Empty.NoExpr

          case o => throw ParsingFailedError(new RuntimeException(s"Unknown expression tree [${o.getClass}]: $o"))
        }

        extracted.setPos(pos)
      }

      Normalizer(tree match {
        case d: DefTree => extractDef(d)
        case t: TermTree => extractStmts(t) match {
          case List(x) => x
          case Nil => ast.Empty[ast.Statement]
          case list => ast.Block(list).setPos(extractPosition(tree.pos))
        }
        case t: TypTree => extractType(t)
      })
    }
  }

}
