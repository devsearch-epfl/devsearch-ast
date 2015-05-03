package devsearch.ast

/** A generic Abstract Syntax Tree format */
sealed trait AST extends Positional with Commentable with java.io.Serializable {
  def fromAST(ast: AST): this.type = {
    ast.comment.foreach(appendComment)
    setPos(ast.pos)
  }

  /** Tree foreach operator, @see [[Operators.foreach]] */
  def foreach(f: AST => Unit): Unit = Operators.foreach(f)(this)

  /** Tree foldRight operator, @see [[Operators.foldRight]] */
  def foldRight[T](f: (AST, Seq[T]) => T): T = Operators.foldRight(f)(this)

  /** Checks for match over this AST and all children, @see [[Operators.exists]] */
  def exists(matcher: AST => Boolean): Boolean = Operators.exists(matcher)(this)

  /** Collects all matches over complete AST, @see [[Operators.collect]] */
  def collect[T](matcher: AST => Set[T]): Set[T] = Operators.collect(matcher)(this)

  /** Alias for [[postMap]] */
  def transform(f: AST => Option[AST], applyRec: Boolean = false): AST = postMap(f, applyRec)

  /** Post-transform on AST, @see [[Operators.postMap]] */
  def postMap(f: AST => Option[AST], applyRec: Boolean = false): AST = Operators.postMap(f, applyRec)(this)
}

/**
 * Special trait for objects to guarantee we don't associate positions or comments with
 * case objects that are reused in multiple places in the AST
 */
sealed trait Unassignable extends AST {
  override def setPos(pos: Position) = this
  override def setComment(comment: String) = this
}


// -- Definitions ---------------------------------------------------------------------

/**
 * Definition super type
 *
 * We extend [[Statement]] here since in many languages, definitions (such as [[ValDef]], for example) can take
 * place in statement positions.
 */
sealed trait Definition extends Statement

/**
 * Package definition
 *
 * Generally at beginning of source file, describes some sort of namespace, for example `package some.package.name`
 */
case class PackageDef(name: String, annotations: List[Annotation], imports: List[Import], definitions: List[Definition]) extends Definition

/**
 * Type definition
 *
 * A type definition. This encompasses formal type parameters, C-style typeDefs and type members like in Scala.
 * By convention, if this is a type alias (like `type A = Int`), we store the aliased type as the first super bound.
 * Examples:
 * - `type A`
 * - `def test[A]` where `A` is a type parameter of `test`
 */
case class TypeDef(modifiers: Modifiers, name: String, annotations: List[Annotation], tparams: List[TypeDef],
  lowerBounds: List[Type], superBounds: List[Type]) extends Definition

/**
 * We use [[ClassDef]] to model all kinds of classes/traits/interfaces/structs/..., so we use a sort field to
 * differentiate them.
 */
sealed trait StructuralSort extends java.io.Serializable
case object ClassSort extends StructuralSort
case object TraitSort extends StructuralSort
case object StructSort extends StructuralSort

/**
 * Class definition
 *
 * A class or structural type definition. This type represents structs, classes, interfaces, traits, etc.
 */
case class ClassDef(modifiers: Modifiers, name: String, annotations: List[Annotation],
  tparams: List[TypeDef], superClasses: List[ClassType],
  definitions: List[Definition], sort: StructuralSort = ClassSort) extends Definition

/**
 * Enum definition
 *
 * The definition of an enumeration type, for example `enum { ... }`.
 */
case class EnumDef(modifiers: Modifiers, name: String, annotations: List[Annotation], superTraits: List[ClassType], definitions: List[Definition], entries: List[EnumConstantDef]) extends Definition

/**
 * Enum constant member definition
 *
 * The actual members of a particular enumeration type. For example, `TOTO` in `enum { TOTO }`.
 */
case class EnumConstantDef(name: String, annotations: List[Annotation], args: List[Expr], members: List[Definition]) extends Definition

/**
 * Annotation definition
 *
 * The definition point of a code annotation. Not to be confused with annotation uses (see [[Annotation]])!
 */
case class AnnotationDef(modifiers: Modifiers, name: String, annotations: List[Annotation], members: List[Definition]) extends Definition

/**
 * Constructor definition
 *
 * The definition of a class (or other type) constructor. Very close to a [[FunctionDef]], except there is no return type.
 */
case class ConstructorDef(modifiers: Modifiers, annotations: List[Annotation], tparams: List[TypeDef], params: List[ValDef], body: Block, isDestructor: Boolean = false) extends Definition

/**
 * Function definition
 *
 * Any kind of named function definitions, like methods or block-local functions. Opposed to [[FunctionLiteral]] since we require a name and get some
 * extra structure too. For example, `def test[A,B,C](x: (A,B)): C = { ... }`.
 */
case class FunctionDef(modifiers: Modifiers, name: String, annotations: List[Annotation], tparams: List[TypeDef], params: List[ValDef], tpe: Type, body: Block) extends Definition

/** Super-trait of value definitions, namely `ValDef` and `ExtractionValDef` */
sealed trait ValueDefinition extends Definition

/**
 * Value definition
 *
 * Generalized value definition. Everything that is a named value falls into this bin, only positioning matters.
 * A few examples:
 * - `val test = 1`
 * - `int a = 0;` (or also `int a;`
 */
case class ValDef(modifiers: Modifiers, name: String, annotations: List[Annotation], tpe: Type, rhs: Expr, varArgs: Boolean = false) extends ValueDefinition

/**
 * Extraction value definition
 *
 * Some languages enable value definitions where the right hand side term is deconstructed into multiple defining values (such as unapply assignments in Scala).
 * These cases are stored in this case class, and can also take the form of tuple extraction.
 */
case class ExtractionValDef(modifiers: Modifiers, pattern: Expr, annotations: List[Annotation], rhs: Expr) extends ValueDefinition

/**
 * Initializer statement
 *
 * A subtle definition mostly useful to catch corner cases. An example, in java, is the `static { ... }` block that can be found in singleton classes. We type this as a definition since it is clearly a statement otherwise.
 * Also, all initialization statements in Scala class blocks that would normally be placed in a constructor in Java will be found in an `Initializer` block.
 */
case class Initializer(isStatic: Boolean, annotations: List[Annotation], body: Block) extends Definition


// -- Statements ----------------------------------------------------------------------

sealed trait Statement extends AST

/** Import statement */
case class Import(name: String, asterisk: Boolean, static: Boolean) extends Statement

/** This call for reference to other constructors */
case class ThisCall(qualifier: Expr, tparams: List[Type], args: List[Expr]) extends Statement

/** Super call for reference to super constructor */
case class SuperCall(qualifier: Expr, tparams: List[Type], args: List[Expr]) extends Statement

/** Assertion statement that will throw an exception if the condition doesn't hold at runtime */
case class Assert(condition: Expr, message: Expr) extends Statement

/** Return statement */
case class Return(value: Expr) extends Statement

/**
 * Named statement
 *
 * Provides support for label definitions with go-to's if these are available in the source language.
 */
case class NamedStatement(name: String, statement: Statement) extends Statement

/** Loop break statement that can provide a label for go-to functionality */
case class Break(target: Option[String]) extends Statement

/** Loop continue statement that can also provide a label for go-to functionality */
case class Continue(target: Option[String]) extends Statement

/** While loop */
case class While(condition: Expr, body: Statement) extends Statement

/** Do-while loop */
case class Do(condition: Expr, body: Statement) extends Statement

/**
 * For loop
 *
 * A for-loop that initializes iteration variables and provides guarded iteration as well as updates.
 * For example, `for(int i=0; i<10; i++)` in Java.
 */
case class For(vals: List[ValDef], inits: List[Expr], condition: Expr, updates: List[Expr], body: Statement) extends Statement

/**
 * Thread locking construct
 *
 * Provides explicit thread-locking management for concurrent languages.
 * For example, `synchronized` in java, or `lock` in C#.
 */
case class Synchronize(lock: Expr, body: Statement) extends Statement


// -- Types ---------------------------------------------------------------------------

sealed trait Type extends AST

/**
 * Class type
 *
 * A bin for all non-primitive named types. Since we're only running above parsers here, most type parameters will fall into this bin as well.
 */
case class ClassType(scope: Expr, name: String, annotations: List[Annotation], tparams: List[Type]) extends Type

/**
 * Primitive type
 *
 * Primitive low-level types. We provide an extensive list of such type instances (see [[PrimitiveTypes]]), but
 * also provide a `Specialized(str: String)` primitive type for types that aren't included in the list.
 */
sealed trait PrimitiveType extends Type with Unassignable

/** Enumeration of primitive types */
object PrimitiveTypes {
  case object String  extends PrimitiveType
  case object Boolean extends PrimitiveType
  case object Char    extends PrimitiveType
  case object Byte    extends PrimitiveType
  case object Short   extends PrimitiveType
  case object Int     extends PrimitiveType
  case object Long    extends PrimitiveType
  case object Float   extends PrimitiveType
  case object Double  extends PrimitiveType
  case object Null    extends PrimitiveType
  case object Void    extends PrimitiveType
  case class Special(tpe: String) extends PrimitiveType
}

/**
 * Array type
 *
 * Many languages provide specialized support for arrays, so we handle the type in a specific way.
 * In java, for example, we would have `int[]` fall in this bin.
 */
case class ArrayType(base: Type) extends Type

/**
 * Function type
 *
 * Certain functional languages offer functions as first-class members, so it makes sense to have
 * a notion of function types in our pseudo type trees. For example,
 * - (A,B) => C in Scala
 */
case class FunctionType(from: List[Type], to: Type) extends Type

/**
 * Wildcard type
 *
 * Quite a few languages provide some sort of unknown type with bounds on it. For example:
 * - in Java: `? extends Object super Toto`
 * - in Scala: `_ <: Any >: Nothing`
 */
case class WildcardType(subType: Type, superType: Type) extends Type

/**
 * Any type
 *
 * The top-most type of the typing-system.
 * - `Any` in Scala
 * - `Object` in Java
 */
case object AnyType extends Type with Unassignable

/**
 * Bottom type
 *
 * The lowest possible type of the typing system. For example, the `Nothing` type in Scala..
 */
case object BottomType extends Type with Unassignable

/**
 * Type hint or unparsed type
 *
 * Some parsers don't parse type trees and some languages actually support type hints, so these
 * cases are all lumped into this `TypeHint` type.
 *
 * The `hint: String` field contains a string representation of this type and can be tokenized (or other)
 * to infer stuff about this type if useful.
 */
case class TypeHint(hint: String) extends Type

/** Complex type that encompasses refinement types, sum types, ... */
case class ComplexType(parents: List[Type], definitions: List[Definition], expr: Expr) extends Type


// -- Expressions ---------------------------------------------------------------------

/**
 * Expression super type
 *
 * We extend the [[Statement]] type here as in many languages, most expressions can be found in statement positions.
 * Since semantics are not really our focus here, this is the simplest way to ensure correct parsing for these.
 */
sealed trait Expr extends Statement

/** Identifier as in reference to local variable */
case class Ident(name: String) extends Expr

/** Binding from an identifier to an expression, like i @ _ in Scala */
case class Bind(name: String, expr: Expr) extends Expr

/**
 * Unary operation
 *
 * An operation applied on an expression that takes no other parameters... (Yeah, this explanation sucks...)
 * The operators are typically part of the language spec.
 * For example:
 * - !a
 * - i++
 */
case class UnaryOp(operand: Expr, operator: String, postfix: Boolean) extends Expr

/**
 * Binary operation
 *
 * An operation between two expression, typically with an operator that is part of the language spec.
 * For example:
 * - a + b
 * - x :: Nil
 */
case class BinaryOp(left: Expr, operator: String, right: Expr) extends Expr

/**
 * Ternary operation
 *
 * Conditional operation, like an if-statement that immediately returns one of the two ops.
 * In Java, we would have `cond ? thenn : elze`.
 */
case class TernaryOp(cond: Expr, thenn: Expr, elze: Expr) extends Expr

/**
 * Function call
 *
 * Represents any sort of function or method call.
 */
case class FunctionCall(receiver: Expr, tparams: List[Type], args: List[Expr]) extends Expr

/** Constructor call (or object creation) */
case class ConstructorCall(tpe: ClassType, args: List[Expr], body: List[Definition]) extends Expr

/** Array index access, in java, `a[i]`. */
case class ArrayAccess(array: Expr, index: Expr) extends Expr

/**
 * Array literal
 *
 * Array creation in general. We try to stay flexible to accommodate typed arrays with dimensions and arrays that immediately fill out their
 * elements. For example:
 * - `new int[10]`
 * - `[1,2,3,4]`
 */
case class ArrayLiteral(tpe: Type, annotations: List[Annotation], dimensions: List[Expr], elements: List[Expr]) extends Expr

/**
 * Pair or sequence literal
 *
 * Used for multiple returns, language supported pairs, sequences, basically anything enclosed between parentheses and with commas ;)
 * I also encode alternatives here (like A | B in Scala) because this is such a rare language feature.
 */
case class MultiLiteral(elements: List[Expr]) extends Expr

/**
 * Map or object literal
 *
 * Object literals in javascript or map literals in python fall into this category.
 */
case class MapLiteral(elements: List[(String, Expr)]) extends Expr

/**
 * Assignment expression
 *
 * Many languages accept inline assignment and dependent assignments. These are captured in this bin, so we have, for example:
 * - a += 1
 * - a = b = 1
 */
case class Assign(target: Expr, value: Expr, operator: Option[String]) extends Expr

/** Cast expression */
case class Cast(expr: Expr, tpe: Type) extends Expr

/**
 * Runtime class instance access
 *
 * For example, in Java, `obj.class`
 */
case class ClassAccess(tpe: Type) extends Expr

/**
 * Static method access
 *
 * Provides access to a class (or other structural object) method based on a static type.
 */
case class MethodAccess(tpe: Type, name: String, tparams: List[Type]) extends Expr

/** Object or other structural type field access */
case class FieldAccess(receiver: Expr, name: String, tparams: List[Type]) extends Expr

/** Instance of check */
case class InstanceOf(expr: Expr, tpe: Type) extends Expr

/**
 * Simple literal
 *
 * Literals that are part of the language definition. Each literal is bound to a primitive type and
 * we store the value in a string format to stay flexible.
 */
case class SimpleLiteral(tpe: PrimitiveType, value: String) extends Expr

/** Null literal, `null` in java */
case object NullLiteral extends Expr with Unassignable

/** Void literal, like `Unit` in scala */
case object VoidLiteral extends Expr with Unassignable

/** This expression, can be specialized with a type */
case class This(qualifier: Expr) extends Expr

/** Super expression, can be specialized with a type */
case class Super(qualifier: Expr) extends Expr

/**
 * Annotation expression
 *
 * Annotation expression for methods, classes, etc. Not to be confused with comments!
 * In Java, for example, annotations follow the @ notation.
 */
case class Annotation(name: String, params: Map[String, Expr] = Map.empty) extends Expr

/**
 * Function literal
 *
 * A function object, like a lambda. These functions are considered nameless (at least for now).
 */
case class FunctionLiteral(params: List[ValDef], tpe: Type, body: Statement) extends Expr

/**
 * Wildcard expression
 *
 * This expression is used to encode a language feature that is expressed in a wildcard position.
 * For example, we have:
 * - `default` switch statement in Java
 * - `_` in Scala
 */
case object Wildcard extends Expr with Unassignable

/**
 * Guarded expression
 *
 * Languages that support match-case expressions, or foreach statements that filter during the foreach
 * encode these "guards" in guarded expressions. For example, in Scala we have
 * - `case a if a > 0 => `
 * - `for (a <- nums if a < 2)`
 */
case class Guarded(expr: Expr, condition: Expr) extends Expr

/**
 * Basic if expression
 *
 * This is actually very close to a [[TernaryOp]], but the syntax is so different that I decided to keep them
 * separate. Maybe the two trees will be unified sometime.
 */
case class If(condition: Expr, thenStatement: Statement, elseStatement: Statement) extends Expr

/**
 * Switch expression
 *
 * A switch-case expression where a selector is matched against expressions associated to code-blocks.
 * We can easily model java switch statements as well as scala match-case expressions with this construct.
 */
case class Switch(selector: Expr, entries: List[(Expr, Block)]) extends Expr

/**
 * Foreach loop
 *
 * Typically for languages that provide a `for ... in ...` kind of construct. Not to be confused with the [[For]]
 * construct that provides guarded iteration. Since certain languages provide both constructs, we provide support
 * for both as well.
 */
case class Foreach(vals: List[ValDef], iterable: Expr, body: Statement, generator: Boolean = false) extends Expr

/** Throw expression, as in `throw new RuntimeException("Haha you failed!")` */
case class Throw(expr: Expr) extends Expr

/**
 * A basic try-catch-finally block
 *
 * The catch block matchers are kept completely generic since the formats can widely vary
 * from one language to the next.
 */
case class Try(tryBlock: Block, catchs: List[(ValueDefinition, Block)], finallyBlock: Block) extends Expr

/**
 * A flexible block structure
 *
 * We model code blocks as a list of [[Statement]]. Since expressions and definitions are also satetements
 * in our language, this is general enough for any block definition.
 */
case class Block(statements: List[Statement]) extends Expr


// -- Helpers -------------------------------------------------------------------------

/**
 * Modifiers
 *
 * Provides modifiers for fields, methods, classes, etc.
 * For example, public, private, static, etc.
 */
sealed class Modifiers(private val mask: Int) extends java.io.Serializable {
  def &(that: Modifiers) = new Modifiers(mask & that.mask)
  def |(that: Modifiers) = new Modifiers(mask | that.mask)

  override def equals(that: Any) : Boolean = that match {
    case mod: Modifiers => mask == mod.mask
    case _ => false
  }

  def isPublic       = (this & Modifiers.PUBLIC)       != Modifiers.NoModifiers
  def isPrivate      = (this & Modifiers.PRIVATE)      != Modifiers.NoModifiers
  def isProtected    = (this & Modifiers.PROTECTED)    != Modifiers.NoModifiers
  def isStatic       = (this & Modifiers.STATIC)       != Modifiers.NoModifiers
  def isFinal        = (this & Modifiers.FINAL)        != Modifiers.NoModifiers
  def isSynchronized = (this & Modifiers.SYNCHRONIZED) != Modifiers.NoModifiers
  def isNative       = (this & Modifiers.NATIVE)       != Modifiers.NoModifiers
  def isAbstract     = (this & Modifiers.ABSTRACT)     != Modifiers.NoModifiers

  override def toString: String = {
    if (this == Modifiers.NoModifiers) "NoModifiers" else {
      (if (isPublic)       List("PUBLIC")       else Nil) ++
      (if (isPrivate)      List("PRIVATE")      else Nil) ++
      (if (isProtected)    List("PROTECTED")    else Nil) ++
      (if (isStatic)       List("STATIC")       else Nil) ++
      (if (isFinal)        List("FINAL")        else Nil) ++
      (if (isSynchronized) List("SYNCHRONIZED") else Nil) ++
      (if (isNative)       List("NATIVE")       else Nil) ++
      (if (isAbstract)     List("ABSTRACT")     else Nil)
    }.mkString(" ")
  }
}

/** Exhaustive list of supported [[Modifiers]] */
object Modifiers {
  object NoModifiers extends Modifiers(0)

  object PUBLIC       extends Modifiers(1 <<  0)
  object PRIVATE      extends Modifiers(1 <<  1)
  object PROTECTED    extends Modifiers(1 <<  2)
  object STATIC       extends Modifiers(1 <<  3)
  object FINAL        extends Modifiers(1 <<  4)
  object SYNCHRONIZED extends Modifiers(1 <<  5)
  object NATIVE       extends Modifiers(1 <<  6)
  object ABSTRACT     extends Modifiers(1 <<  7)
}

object Names {
  val DEFAULT = "dft01"
  val NOOP = "$$noop"
  val REGEXP = "$$regexp"

  val OVERRIDE_ANNOTATION = "Override"
  val THROWS_ANNOTATION = "throws"
}

object Empty {
  sealed class EmptyProvider[T <: AST](val value: T)

  object NoDef extends Definition with Unassignable { override def toString = "NoDef" }
  implicit val emptyDefProvider = new EmptyProvider[Definition](NoDef)
  implicit val emptyASTProvider = new EmptyProvider[AST](NoDef)

  object NoType extends ClassType(NoExpr, Names.DEFAULT, Nil, Nil) with Unassignable { override def toString = "NoType" }
  implicit val emptyTypeProvider = new EmptyProvider[Type](NoType)

  object NoExpr extends Expr with Unassignable { override def toString = "NoExpr" }
  implicit val emptyExprProvider = new EmptyProvider[Expr](NoExpr)

  object NoStmt extends Block(Nil) with Unassignable { override def toString = "NoStmt" }
  implicit val emptyBlockProvider = new EmptyProvider[Block](NoStmt)
  implicit val emptyStmtProvider = new EmptyProvider[Statement](NoStmt)

  def apply[T <: AST : EmptyProvider] : T = implicitly[EmptyProvider[T]].value
}
