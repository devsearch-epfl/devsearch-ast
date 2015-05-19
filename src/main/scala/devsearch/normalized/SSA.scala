package devsearch.normalized

import devsearch.ast

/** SSA statement super-type.
  *
  * Statements in SSA normal form basically consist in assignments:
  *  - [[Assign]], a standard local assignment node
  *  - [[MultiAssign]], a deconstrucing assignment that extracts tuple fields
  *  - [[FieldAssign]], assignment to an object fields
  *  - [[IndexAssign]], assignment to an array index
  *
  * Two more statements are available to encode mutation and are:
  *  - [[Mutator]], a wrapper for an expression that mutates the program
  *  - [[Throw]], an exception-throwing statement that terminates the current
  *    control-flow block
  *
  * We also provide a [[rename]] utility method that is mainly used during
  * the single-assignment transformation.
  */
sealed trait Statement extends ast.Positional {

  /** Transform identifiers given the argument function `f` */
  def rename(f: Identifier => Identifier): Statement = this match {
    case Assign(id, expr) => Assign(f(id).setPos(id.pos), expr rename f).setPos(pos)
    case MultiAssign(ids, value) => MultiAssign(ids.map(id => f(id).setPos(id.pos)), value rename f).setPos(pos)
    case FieldAssign(obj, name, value) => FieldAssign(obj rename f, name, value rename f).setPos(pos)
    case IndexAssign(array, idx, value) => IndexAssign(array rename f, idx rename f, value rename f).setPos(pos)
    case Mutator(expr) => Mutator(expr rename f).setPos(pos)
    case Throw(value) => Throw(value rename f).setPos(pos)
  }
}

/** Simple assignment to a local variable */
case class Assign(id: Identifier, expr: Expr) extends Statement

/** Extraction assignment that extracts fields from a tuple value.
  *
  * Note that the assignment value is in ground-form, i.e. a [[Value]]
  */
case class MultiAssign(ids: Seq[Identifier], value: Value) extends Statement

/** Field assignment that assigns to an object field.
  *
  * Note that both the receiving object and assignment value are in ground-form, i.e. [[Value]] instances.
  */
case class FieldAssign(obj: Value, name: String, value: Value) extends Statement

/** Array index assignment.
  *
  * Both the array and index are computable values and index may be string-typed
  * in certain languages (such as JavaScript) where array and object indexing follows
  * the same syntax.
  *
  * Note that array, index and value are in ground-form, i.e. [[Value]] instances.
  */
case class IndexAssign(array: Value, index: Value, value: Value) extends Statement

/** A mutating expression that changes program state.
  *
  * This is a useful statement as the rest of SSA form is functional and pure (modulo
  * a certain pure definition of phi-functions).
  *
  * We take an [[Expr]] instance here as mutator as values themselves cannot mutate
  * the program state in general.
  */
case class Mutator(expr: Expr) extends Statement

/** Throw an expression and terminate the current control-flow block */
case class Throw(value: Value) extends Statement


/** SSA expression super-type.
  *
  * It should be noted that the [[Value]] type is a sub-type of [[Expr]] since leaf
  * expressions can appear anywhere a more complex expression is valid. Note also that
  * all fields of [[Expr]] sub-types are always [[Value]] elements and complex
  * expressions are flattened out into their simple components.
  *
  * We provide both a [[rename]] utility and [[dependencies]] that can be used to 
  * respectively map identifiers to other values and compute the set of identifiers
  * an expression depends on where identifiers represent local variables.
  */
sealed trait Expr extends ast.Positional {

  /** Transform identifiers given the argument function `f` */
  def rename(f: Identifier => Identifier): Expr = this match {
    case BinaryOp(lhs, op, rhs) => BinaryOp(lhs rename f, op, rhs rename f).setPos(pos)
    case UnaryOp(expr, op, postfix) => UnaryOp(expr rename f, op, postfix).setPos(pos)
    case Call(fun, args) => Call(fun rename f, args.map(_ rename f)).setPos(pos)
    case New(tpe, args) => New(tpe, args.map(_ rename f)).setPos(pos)
    case Index(expr, index) => Index(expr rename f, index rename f).setPos(pos)
    case InstanceOf(expr, tpe) => InstanceOf(expr rename f, tpe).setPos(pos)
    case Unapply(tpe, value) => Unapply(tpe, value rename f).setPos(pos)
    case Phi(values) => Phi(values.map(_ rename f)).setPos(pos)
    case Field(value, name) => Field(value rename f, name).setPos(pos)
    case v: Value => v rename f
    case (_: Catch) => this
  }

  /** Compute the set of depending identifiers for the current expression */
  def dependencies: Set[Identifier] = this match {
    case BinaryOp(lhs, _, rhs) => lhs.dependencies ++ rhs.dependencies
    case UnaryOp(expr, _, _) => expr.dependencies
    case Call(fun, args) => fun.dependencies ++ args.flatMap(_.dependencies)
    case New(_, args) => args.flatMap(_.dependencies).toSet
    case Index(expr, index) => expr.dependencies ++ index.dependencies
    case InstanceOf(expr, _) => expr.dependencies
    case Unapply(_, value) => value.dependencies
    case Phi(values) => values.flatMap(_.dependencies).toSet
    case Field(value, _) => value.dependencies
    case id: Identifier => Set(id)
    case (_: Value) | (_: Catch) => Set.empty
  }
}

/** Binary operation `op` between values `lhs` and `rhs` */
case class BinaryOp(lhs: Value, op: String, rhs: Value) extends Expr

/** Unary operation `op` on value `expr`, which can be a postfix operation */
case class UnaryOp(expr: Value, op: String, postfix: Boolean) extends Expr

/** Function call of value function `fun` (function fields and local functions are
  * uniformely flattened out to local variables) with arguments `args`.
  */
case class Call(fun: Value, args: Seq[Value]) extends Expr

/** Object creation with type `tpe` and arguments `args` */
case class New(tpe: Type, args: Seq[Value]) extends Expr

/** Index access of array `expr` at location `index` */
case class Index(expr: Value, index: Value) extends Expr

/** Instance of check if value `expr` is of type `tpe` */
case class InstanceOf(expr: Value, tpe: Type) extends Expr

/** Unapply call for type `tpe` and value `value`. This expression follows
  * the Scala `unapply` behavior returning an optional expression that can
  * then be extracted using [[MultiAssign]].
  */
case class Unapply(tpe: Type, value: Value) extends Expr

/** Catch expression that catches an expression of type `tpe` and
  * returns it. This will typically take place in an [[Assign]]
  * statement.
  */
case class Catch(tpe: Type) extends Expr

/** Phi expression that performs phi-selection on `values` */
case class Phi(values: Seq[Value]) extends Expr

/** Field access of field `name` in object `value` */
case class Field(value: Value, name: String) extends Expr


/** SSA value super-type.
  *
  * Values are the most simple expressions in the normal form, those that are not
  * composed from any sub-components.
  *
  * Value extends [[Expr]] since they can appear in any position that is valid for
  * an SSA expression.
  */
sealed trait Value extends Expr {

  /** Transform identifiers given the argument function `f` */
  override def rename(f: Identifier => Identifier): Value = this match {
    case id: Identifier if id != Default => f(id).setPos(id.pos)
    case _ => this
  }
}

/** Block-local variable that can only be assigned once as per SSA contract */
case class Identifier(name: String) extends Value

/** Literal value */
case class Literal(value: String) extends Value

/** Reference to `this` */
case class This() extends Value

/** Reference to `super` */
case class Super() extends Value

/** Identifier that encodes the guarding condition of the default branch of
  * switch or case control-flow elements.
  */
object Default extends Identifier("$default")


/** SSA type super-type.
  *
  * Types in our SSA normal form are rather trivial but we provide specific handling
  * of list and map types to enable easier matching between functional and imperative
  * code, see [[ListType]] and [[MapType]].
  */
sealed trait Type extends ast.Positional

/** Primitive type, @see [[ast.PrimitiveType]] */
case class PrimitiveType(tpe: ast.PrimitiveType) extends Type

/** Named type where type parameters are discarded */
case class ReferenceType(name: String) extends Type

/** Map type that encodes any structure with key-value store functionalities */
case object MapType extends Type

/** List type that encodes any structure with indexed are linked list functionalities */
case object ListType extends Type

/** Fallback type if none of the other [[Type]] sub-types are applicable */
case object UnknownType extends Type

