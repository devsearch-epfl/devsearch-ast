package devsearch.parsers

import spray.json._
import scala.sys.process._
import scala.reflect._

import devsearch.ast._
import Modifiers._

object GoParser extends DevMineParser("parser-go")

abstract class DevMineParser(parserPath: String) extends Parser {

  private val parser = {
    val stream = getClass.getResourceAsStream("/parsers/" + parserPath)
    val fileName = System.getProperty("java.io.tmpdir") + "/devsearch-parsers" + (new java.util.Random).nextInt + "/" + parserPath
    val file = new java.io.File(fileName)

    var output : java.io.FileOutputStream = null
    try {
      file.getParentFile.mkdirs()
      output = new java.io.FileOutputStream(file)

      val buffer = new Array[Byte](4096)
      var readBytes = if (stream != null) stream.read(buffer) else 0
      while (readBytes > 0) {
        output.write(buffer, 0, readBytes)
        readBytes = stream.read(buffer)
      }

      file.setExecutable(true)
      fileName
    } catch {
      case io: java.io.IOException => throw ParsingFailedError(io)
    } finally {
      if (output != null) output.close()
      if (stream != null) stream.close()
    }
  }

  def parse(source: Source): AST = {
    val dirname = source.path.split("/").init.mkString("/")
    val json = Seq(parser, dirname).!!
    extractJSON(json.parseJson, SimplePosition(source, 0, 0))
  }

  private implicit class Wrapper(jss: List[JsValue]) {
    def as[A <: JsValue : ClassTag, B](f: A => B) = jss.flatMap(js => classTag[A].unapply(js).toList map f)

    def asList[A <: JsValue : ClassTag, B](f: A => Iterable[B]) = jss.flatMap(js => classTag[A].unapply(js).toList flatMap f)

    def asArray = jss flatMap {
      case JsArray(elements) => elements
      case _ => Nil
    }
  }

  private implicit class OptionWrapper(o: Option[JsValue]) extends Wrapper(o.toList)

  private def extractJSON(js: JsValue, pos: Position): AST = js match {
    case JsObject(fields) =>
      val packages = fields.get("packages").asArray.asList { (o: JsObject) =>
        val name = extractName(o)
        o.fields.get("source_files").asArray.map(extractPackage(_, pos)).collect {
          case p: PackageDef => p.copy(name = name).fromAST(p)
        }
      }

      assert(packages.size <= 1)
      packages.headOption match {
        case Some(p) => p
        case None => Empty[AST]
      }
    case _ => Empty[AST]
  }

  private def extractName(js: JsValue): String = js match {
    case JsObject(fields) => fields.get("name").map(extractName(_)) getOrElse Names.DEFAULT
    case JsString(value) => value
    case _ => Names.DEFAULT
  }

  private def extractDoc(js: JsObject): String = 
    js.fields.get("doc").asArray.as((s: JsString) => s).mkString("\n")

  private def extractMulti(elems: List[Expr], pos: Position): Expr = {
    val elements = elems.filterNot(_ == Empty[Expr])
    if (elements.size > 1) MultiLiteral(elements).setPos(pos)
    else if (elements.size == 1) elements.head
    else Empty[Expr]
  }

  /** Litteral expression type names */
  private val IntLit    = "INT"
  private val FloatLit  = "FLOAT"
  private val ImagLit   = "IMAG"
  private val CharLit   = "CHAR"
  private val StringLit = "STRING"
  private val BoolLit   = "BOOl"
  private val NullLit   = "NIL"

  private def extractType(js: JsValue, pos: Position): Type = js match {
    case JsObject(fields) => fields.get("type").map(extractType(_, pos)) getOrElse Empty[Type]
    case JsString(IntLit) => PrimitiveTypes.Int
    case JsString(FloatLit) => PrimitiveTypes.Float
    case JsString(ImagLit) => PrimitiveTypes.Special("imaginary")
    case JsString(CharLit) => PrimitiveTypes.Char
    case JsString(StringLit) => PrimitiveTypes.String
    case JsString(BoolLit) => PrimitiveTypes.Boolean
    case JsString(NullLit) => PrimitiveTypes.Null
    case JsString(value) => TypeHint(value).setPos(pos)
    case _ => Empty[Type]
  }

  private def extractNamespace(js: JsObject, pos: Position): Expr = js.fields.get("namespace") match {
    case Some(JsString("")) => Empty[Expr]
    case Some(JsString(scope)) => Ident(scope).setPos(pos)
    case _ => Empty[Expr]
  }

  private def extractField(o: JsObject, pos: Position) =
    ValDef(NoModifiers, extractName(o), Nil, extractType(o, pos), Empty[Expr]).setPos(pos).setComment(extractDoc(o))

  private def extractPackage(js: JsValue, pos: Position) = js match {
    case obj @ JsObject(fields) =>

      val imports = fields.get("imports").asArray.as((s: JsString) => Import(s.value, false, false).setPos(pos))

      def extractStruct(o: JsObject, pos: Position) = {
        val fields = o.fields.get("fields").asArray.as((o: JsObject) => extractField(o, pos))
        ClassDef(NoModifiers, extractName(o), Nil, Nil, Nil, fields, StructSort).setComment(extractDoc(o)).setPos(pos)
      }

      def extractVisibility(o: JsObject) = o.fields.get("visibility").as((s: JsString) => s.value match {
        case "public" => PUBLIC
        case "protected" => PROTECTED
        case "private" => PRIVATE
        case "package" => PROTECTED
        case _ => NoModifiers
      }).headOption getOrElse NoModifiers

      val typeDefs = fields.get("type_specifiers").asArray.asList { o: JsObject =>
        val doc = extractDoc(o)
        val name = extractName(o)
        val tpe = o.fields.get("type").map {
          case obj @ JsObject(fields) if fields.get("expression_type") == Some(StructTypeName) => extractStruct(obj, pos)
          case js => extractType(js, pos)
        }

        tpe match {
          case Some(cd: ClassDef) => List(cd,
            TypeDef(NoModifiers, name, Nil, Nil, Nil, List(
              ClassType(Empty[Expr], cd.name, Nil, Nil).setPos(pos)
            )).setPos(pos).setComment(doc)
          )
          case Some(tpe: Type) => List(
            TypeDef(NoModifiers, name, Nil, Nil, Nil, List(tpe)).setPos(pos).setComment(doc)
          )
          case _ => Nil
        }
      }

      val structs = fields.get("structs").asArray.as((o: JsObject) => extractStruct(o, pos))

      def extractValDef(o: JsObject, pos: Position): ValDef = {
        val value = o.fields.get("value").map(extractExpr(_, pos)) getOrElse Empty[Expr]
        ValDef(extractVisibility(o), extractName(o), Nil, extractType(o, pos), value).setComment(extractDoc(o)).setPos(pos)
      }

      val constants = fields.get("constants").asArray.as { o: JsObject =>
        val valDef = extractValDef(o, pos)
        valDef.copy(modifiers = valDef.modifiers | FINAL).fromAST(valDef)
      }

      val variables = fields.get("variables").asArray.as((o: JsObject) => extractValDef(o, pos))

      def extractFunction(o: JsObject, pos: Position) = {
        val body = o.fields.get("body").asArray.flatMap(extractStmts(_, pos))
        val loc = o.fields.get("loc").as((n: JsNumber) => n.value.toInt).headOption getOrElse 0
        // XXX: functions don't have parameters for now...
        val position = LineRangePosition(pos.source, pos.line, pos.line + loc)
        FunctionDef(extractVisibility(o), extractName(o), Nil, Nil, Nil, extractType(o, pos), Block(body).setPos(position))
          .setComment(extractDoc(o))
          .setPos(position)
      }

      // XXX: we only report `distinct` functions since there is a bug in the DevMine parser-go that dupplicates
      //      returned function definitions in the package!
      val functions = fields.get("functions").asArray.as((o: JsObject) => extractFunction(o, pos)).distinct

      def extractSuperInterface(o: JsObject, pos: Position): ClassType = {
        val name = o.fields.get("interface_name").as((s: JsString) => s.value).headOption getOrElse Names.DEFAULT
        ClassType(extractNamespace(o, pos), name, Nil, Nil).setPos(pos)
      }

      def extractSuperClass(o: JsObject, pos: Position): ClassType = {
        val name = o.fields.get("class_name").as((s: JsString) => s.value).headOption getOrElse Names.DEFAULT
        ClassType(extractNamespace(o, pos), name, Nil, Nil).setPos(pos)
      }

      def extractSuperTrait(o: JsObject, pos: Position): ClassType = {
        val name = o.fields.get("trait_name").as((s: JsString) => s.value).headOption getOrElse Names.DEFAULT
        ClassType(extractNamespace(o, pos), name, Nil, Nil).setPos(pos)
      }

      val interfaces = fields.get("interfaces").asArray.as { (o: JsObject) =>
        val superTraits = o.fields.get("implemented_interfaces").asArray.as((o: JsObject) => extractSuperInterface(o, pos))
        val functions = o.fields.get("prototypes").asArray.as { (o: JsObject) =>
          FunctionDef(ABSTRACT | extractVisibility(o), extractName(o), Nil, Nil, Nil, extractType(o, pos), Empty[Block])
            .setPos(pos).setComment(extractDoc(o))
        }
        ClassDef(extractVisibility(o), extractName(o), Nil, Nil, superTraits, functions, TraitSort)
          .setPos(pos).setComment(extractDoc(o))
      }

      def extractAttribute(o: JsObject, pos: Position) = {
        val valDef = extractValDef(o, pos)
        val constant = o.fields.get("constant").as((a: JsBoolean) => a.value).headOption getOrElse false
        val static = o.fields.get("static").as((a: JsBoolean) => a.value).headOption getOrElse false
        val modifiers = (if (constant) FINAL else NoModifiers) | (if (static) STATIC else NoModifiers) | valDef.modifiers
        valDef.copy(modifiers = modifiers).fromAST(valDef)
      }

      def extractMethod(o: JsObject, pos: Position) = {
        val function = extractFunction(o, pos)
        val overrides = o.fields.get("override").as((b: JsBoolean) => b.value).headOption getOrElse false
        val annotations = function.annotations ++ (if (overrides) List(Annotation(Names.OVERRIDE_ANNOTATION).setPos(pos)) else Nil)
        function.copy(annotations = annotations).fromAST(function)
      }

      def extractClass(o: JsObject, pos: Position): ClassDef = {
        val superClasses = o.fields.get("extended_classes").asArray.as((o: JsObject) => extractSuperClass(o, pos))
        val superInterfaces = o.fields.get("implemented_interfaces").asArray.as((o: JsObject) => extractSuperInterface(o, pos))

        val fields = o.fields.get("attributes").asArray.as((o: JsObject) => extractAttribute(o, pos))

        def extractConstructor(o: JsObject, pos: Position) = {
          val doc = extractDoc(o)
          val params = o.fields.get("parameters").asArray.as((o: JsObject) => extractField(o, pos))
          val body = Block(o.fields.get("body").asArray.asList((o: JsObject) => extractStmts(o, pos))).setPos(pos)
          val modifiers = extractVisibility(o)
          val loc = o.fields.get("loc").as((n: JsNumber) => n.value.toInt).headOption getOrElse 0
          ConstructorDef(modifiers, Nil, Nil, params, body)
            .setPos(LineRangePosition(pos.source, pos.line, pos.line + loc)).setComment(doc)
        }

        val constructors = o.fields.get("constructors").asArray.as((o: JsObject) => extractConstructor(o, pos))

        val destructors = o.fields.get("destructors").asArray.as { (o: JsObject) =>
          val constr = extractConstructor(o, pos)
          constr.copy(isDestructor = true).fromAST(constr)
        }

        val methods = o.fields.get("methods").asArray.as((o: JsObject) => extractMethod(o, pos))

        val nestedClasses = o.fields.get("nested_classes").asArray.as((o: JsObject) => extractClass(o, pos))

        val superTraits = o.fields.get("mixins").asArray.as((o: JsObject) => extractSuperTrait(o, pos))

        ClassDef(extractVisibility(o), extractName(o), Nil, Nil,
          superClasses ++ superInterfaces ++ superTraits, fields ++ constructors ++ destructors ++ methods ++ nestedClasses)
            .setPos(pos).setComment(extractDoc(o))
      }

      val classes = fields.get("classes").asArray.as((o: JsObject) => extractClass(o, pos))

      def extractTrait(o: JsObject, pos: Position): ClassDef = {
        val fields = o.fields.get("attributes").asArray.as((o: JsObject) => extractAttribute(o, pos))
        val methods = o.fields.get("methods").asArray.as((o: JsObject) => extractMethod(o, pos))
        val classes = o.fields.get("classes").asArray.as((o: JsObject) => extractClass(o, pos))
        val traits = o.fields.get("traits").asArray.as((o: JsObject) => extractTrait(o, pos))
        ClassDef(NoModifiers, extractName(o), Nil, Nil, Nil, fields ++ methods ++ classes ++ traits, TraitSort).setPos(pos)
      }

      val traits = fields.get("traits").asArray.as((o: JsObject) => extractTrait(o, pos))

      val name = fields.get("path").as((s: JsString) => s.value).map(s => s.split("/").mkString(".")).headOption getOrElse Names.DEFAULT

      PackageDef(name, Nil, imports, typeDefs ++ structs ++ constants ++ variables ++ functions ++ interfaces ++ classes ++ traits).setPos(pos)

    case _ => Empty[AST]
  }

  private val UnaryExprName           = "UNARY"
  private val BinaryExprName          = "BINARY"
  private val TernaryExprName         = "TERNARY"
  private val IncDecExprName          = "INC_DEC"
  private val CallExprName            = "CALL"
  private val ConstructorCallExprName = "CONSTRUCTOR_CALL"
  private val ArrayExprName           = "ARRAY"
  private val IndexExprName           = "INDEX"
  private val BasicLitName            = "BASIC_LIT"
  private val FuncLitName             = "FUNC_LIT"
  private val ClassLitName            = "CLASS_LIT"
  private val ArrayLitName            = "ARRAY_LIT"
  private val StructTypeName          = "STRUCT_TYPE"
  private val AttrRefName             = "ATTR_REF"
  private val ValueSpecName           = "VALUE_SPEC"
  private val IdentName               = "IDENT"

  private def extractExpr(js: JsValue, pos: Position): Expr = js match {
    case obj @ JsObject(fields) =>
      val nextPos = fields.get("line").flatMap {
        case JsNumber(line) => Some(SimpleLinePosition(pos.source, line.toInt))
        case _ => None
      } getOrElse pos

      def extractFunction(js: JsValue, pos: Position): (Expr, String) = js match {
        case obj @ JsObject(fields) =>
          val name = fields.get("function_name").as((s: JsString) => s.value).headOption getOrElse Names.DEFAULT
          extractNamespace(obj, pos) -> name
        case JsString(name) => Empty[Expr] -> name
        case _ => Empty[Expr] -> Names.DEFAULT
      }

      val extracted = fields.get("expression_name").as((s: JsString) => s.value).headOption match {
        case Some(UnaryExprName) => obj.getFields("operator", "operand") match {
          case Seq(JsString(op), operand) => UnaryOp(extractExpr(operand, nextPos), op, false)
          case _ => Empty[Expr]
        }

        case Some(BinaryExprName) => obj.getFields("left_expression", "operator", "right_expression") match {
          case Seq(leftOp, JsString(op), rightOp) => BinaryOp(extractExpr(leftOp, nextPos), op, extractExpr(rightOp, nextPos))
          case _ => Empty[Expr]
        }

        case Some(TernaryExprName) => obj.getFields("condition", "then", "else") match {
          case Seq(cond, t, e) => TernaryOp(extractExpr(cond, nextPos), extractExpr(t, nextPos), extractExpr(e, nextPos))
          case _ => Empty[Expr]
        }

        case Some(IncDecExprName) => obj.getFields("operand", "operator", "is_pre") match {
          case Seq(operand, JsString(operator), JsBoolean(isPre)) => UnaryOp(extractExpr(operand, nextPos), operator, !isPre)
          case _ => Empty[Expr]
        }

        case Some(CallExprName) => obj.getFields("function", "arguments") match {
          case Seq(function, JsArray(arguments)) =>
            val (receiver, name) = extractFunction(function, nextPos)
            FunctionCall(FieldAccess(receiver, name, Nil).setPos(nextPos), Nil, arguments.toList.map(extractExpr(_, nextPos)))
          case _ => Empty[Expr]
        }

        case Some(ConstructorCallExprName) => obj.getFields("function", "arguments") match {
          case Seq(function, JsArray(arguments)) =>
            val (receiver, name) = extractFunction(function, nextPos)
            ConstructorCall(ClassType(receiver, name, Nil, Nil).setPos(nextPos), arguments.toList.map(extractExpr(_, nextPos)), Nil)
          case _ => Empty[Expr]
        }

        case Some(ArrayExprName) =>
          ArrayLiteral(extractType(obj, nextPos), Nil, Nil, Nil)

        case Some(IndexExprName) =>
          ArrayAccess(fields.get("expression").map(extractExpr(_, nextPos)) getOrElse Empty[Expr],
            fields.get("index").map(extractExpr(_, nextPos)) getOrElse Empty[Expr])

        case Some(BasicLitName) => obj.getFields("kind", "value") match {
          case Seq(kind, JsString(value)) => extractType(kind, nextPos) match {
            case tpe : PrimitiveType if tpe == PrimitiveTypes.String => SimpleLiteral(tpe, {
              val lastIdx = value.length - 1
              if (value.charAt(0) == '"' && value.charAt(lastIdx) == '"') value.substring(1, lastIdx)
              else value
            })
            case tpe : PrimitiveType => SimpleLiteral(tpe, value).setPos(nextPos)
            case th @ TypeHint(str) => SimpleLiteral(PrimitiveTypes.Special(str).setPos(nextPos), value).fromAST(th)
            case _ => Empty[Expr]
          }
          case _ => Empty[Expr]
        }

        case Some(FuncLitName) =>
          val body = Block(fields.get("body").asArray.flatMap(extractStmts(_, nextPos))).setPos(nextPos)
          val tpe = extractType(obj, nextPos)
          // XXX: function's don't have arguments for now...
          FunctionLiteral(Nil, tpe, body)

        // XXX: WTF is this??
        case Some(ClassLitName) => Empty[Expr]
        /*
          val superClasses = fields.get("extended_classes").flatMap(extractType)
          val superInterfaces = fields.get("implemented_interfaces").flatMap(extractType)
          val attributes = fields.get("attributes").flatMap(extractAttributeDef)
          val constructors = fields.get("constructors").flatMap(extractConstructorDef)
          val destructors = fields.get("destructors").flatMap(extractDestructorDef)
          val methods = fields.get("methods").flatMap(extractMethodDef)
          ClassLiteral(superClasses, superInterfaces, constructors, destructors, attributes, methods)
        */

        case Some(ArrayLitName) =>
          val elements = fields.get("elements").asArray.map(extractExpr(_, nextPos))
          val dimensions = fields.get("type").asList((o: JsObject) => o.fields.get("dimensions")).asArray
                                             .as((n: JsNumber) => SimpleLiteral(PrimitiveTypes.Int, n.value.toString).setPos(nextPos))
          val tpe = fields.get("type").asList((o: JsObject) => o.fields.get("element_type").toList).map(extractType(_, nextPos)).headOption getOrElse Empty[Type]
          ArrayLiteral(tpe, Nil, dimensions, elements)

        case Some(AttrRefName) =>
          val name = extractName(obj)
          fields.get("receiver").map(extractExpr(_, nextPos)) match {
            case Some(receiver) => FieldAccess(receiver, name, Nil)
            case None => Ident(name)
          }

        case Some(IdentName) => extractName(obj) match {
          case b @ ("true" | "false") => SimpleLiteral(PrimitiveTypes.Boolean, b)
          case name => Ident(name)
        }

        case Some(ValueSpecName) => obj.getFields("name", "type") match {
          case Seq(value, tpe) => Cast(extractExpr(value, nextPos), extractType(tpe, nextPos))
          case _ => Empty[Expr]
        }

        case _ => Empty[Expr]
      }

      extracted.setPos(nextPos)

    case _ => Empty[Expr]
  }

  private val IfStmtName        = "IF"
  private val SwitchStmtName    = "SWITCH"
  private val LoopStmtName      = "LOOP"
  private val RangeLoopStmtName = "RANGE_LOOP"
  private val AssignStmtName    = "ASSIGN"
  private val DeclStmtName      = "DECL"
  private val ReturnStmtName    = "RETURN"
  private val ExprStmtName      = "EXPR"
  private val TryStmtName       = "TRY"
  private val ThrowStmtName     = "THROW"
  private val OtherStmtName     = "OTHER"

  private def extractStmts(js: JsValue, pos: Position): List[Statement] = js match {
    case obj @ JsObject(fields) =>

      // /!\ RELATIVE TO FUNCTION POSITION!!
      val nextPos = fields.get("line").flatMap {
        case JsNumber(line) => Some(SimpleLinePosition(pos.source, pos.line + line.toInt))
        case _ => None
      } getOrElse pos

      val stmts = fields.get("statement_name").as((s: JsString) => s.value).headOption match {
        case Some(IfStmtName) =>
          val init = fields.get("initialization").toList.flatMap(extractStmts(_, nextPos))
          val cond = fields.get("condition").map(extractExpr(_, nextPos)) getOrElse Empty[Expr]
          val thenn = Block(fields.get("body").asArray.flatMap(extractStmts(_, nextPos))).setPos(nextPos)
          val elze = Block(fields.get("else").asArray.flatMap(extractStmts(_, nextPos))).setPos(nextPos)
          init :+ If(cond, thenn, elze)

        case Some(SwitchStmtName) =>
          val init = fields.get("initialization").toList.flatMap(extractStmts(_, nextPos))
          val cond = fields.get("condition").map(extractExpr(_, nextPos)) getOrElse Empty[Expr]
          val cases = fields.get("case_clauses").asArray.asList { (o: JsObject) =>
            val conds = o.fields.get("conditions").asArray.map(extractExpr(_, nextPos))
            val body = Block(o.fields.get("body").asArray.flatMap(extractStmts(_, nextPos))).setPos(nextPos)
            conds.map(c => c -> body)
          }
          val defaultCase = fields.get("default") match {
            case Some(JsArray(elements)) => Some(Wildcard -> Block(elements.toList.flatMap(extractStmts(_, nextPos))).setPos(nextPos))
            case _ => None
          }
          init :+ Switch(cond, cases ++ defaultCase)
          
        case Some(LoopStmtName) =>
          val init = fields.get("initialization").asArray.flatMap(extractStmts(_, nextPos))
          val cond = fields.get("condition").map(extractExpr(_, nextPos)) getOrElse Empty[Expr]
          val post = fields.get("post_iteration_statement").asArray.flatMap(extractStmts(_, nextPos))
          val body = fields.get("body").asArray.flatMap(extractStmts(_, nextPos))
          val elze = fields.get("else").asArray.flatMap(extractStmts(_, nextPos))
          val isPostEval = fields.get("is_post_evaluated").as((b: JsBoolean) => b.value).headOption getOrElse false

          val valDefs = init.collect { case vd : ValDef => vd }
          val initExprs = init.collect { case expr : Expr => expr }
          val remainder = init diff valDefs diff initExprs

          remainder :+ {
            val loop = For(valDefs, initExprs, cond, post.collect { case expr : Expr => expr }, Block(body).setPos(nextPos)).setPos(nextPos)
            if (elze.nonEmpty && elze != List(Empty[Statement])) {
              If(cond, loop, Block(elze).setPos(nextPos))
            } else {
              loop
            }
          }

        case Some(RangeLoopStmtName) =>
          val vars = fields.get("variables").asArray.flatMap(js => extractExpr(js, nextPos) match {
            case id @ Ident(name) => List(ValDef(NoModifiers, name, Nil, Empty[Type], Empty[Expr]).fromAST(id))
            case _ => Nil
          })
          val iterable = fields.get("iterable").map(extractExpr(_, nextPos)) getOrElse Empty[Expr]
          val body = Block(fields.get("body").asArray.flatMap(extractStmts(_, nextPos))).setPos(nextPos)
          List(Foreach(vars, iterable, body))

        case Some(AssignStmtName) =>
          val left = extractMulti(fields.get("left_hand_side").asArray.map(extractExpr(_, nextPos)), nextPos)
          val right = extractMulti(fields.get("right_hand_side").asArray.map(extractExpr(_, nextPos)), nextPos)
          List(Assign(left, right, None))

        case Some(DeclStmtName) =>
          val left = extractMulti(fields.get("left_hand_side").asArray.map(extractExpr(_, nextPos)), nextPos)
          val right = extractMulti(fields.get("right_hand_side").asArray.map(extractExpr(_, nextPos)), nextPos)
          val modifiers = fields.get("kind").map {
            case JsString("CONSTANT") => FINAL
            case _ => NoModifiers
          } getOrElse NoModifiers

          left match {
            case Ident(name) => List(ValDef(modifiers, name, Nil, Empty[Type], right))
            case _ => Nil
          }

        case Some(ReturnStmtName) =>
          val elements = fields.get("results").asArray.map(extractExpr(_, nextPos))
          val result = extractMulti(elements, nextPos)
          List(Return(result))

        case Some(ExprStmtName) =>
          fields.get("expression").map(extractExpr(_, nextPos)).toList

        case Some(TryStmtName) =>
          val body = Block(fields.get("body").asArray.flatMap(extractStmts(_, nextPos))).setPos(nextPos)
          val catches = fields.get("catch_clauses").asArray.asList { (o: JsObject) =>
            val params = o.fields.get("params").asArray.as((o: JsObject) => extractField(o, nextPos))
            val body = Block(o.fields.get("body").asArray.flatMap(extractStmts(_, nextPos))).setPos(nextPos)
            params.map(p => p -> body)
          }
          val finallyBlock = Block(fields.get("finally").asArray.flatMap(extractStmts(_, nextPos))).setPos(nextPos)
          List(Try(body, catches, finallyBlock))

        case Some(ThrowStmtName) => obj.getFields("expression") match {
          case Seq(o) => List(extractExpr(o, nextPos))
          case _ => Nil
        }
          
        case Some(OtherStmtName) =>
          fields.get("body").asArray.flatMap(extractStmts(_, nextPos))

        case _ =>
          val expr = extractExpr(js, nextPos)
          if (expr == Empty[Expr]) Nil else List(expr)
      }

      stmts.map(_.setPos(nextPos))

    case _ => Nil
  }
}
