package devsearch.normalized

import devsearch.ast

sealed trait Statement extends ast.Positional {

  def rename(f: Identifier => Identifier): Statement = this match {
    case Assign(id, expr) => Assign(f(id).setPos(id.pos), expr rename f).setPos(pos)
    case MultiAssign(ids, value) => MultiAssign(ids.map(id => f(id).setPos(id.pos)), value rename f).setPos(pos)
    case FieldAssign(obj, name, value) => FieldAssign(obj rename f, name, value rename f).setPos(pos)
    case IndexAssign(array, idx, value) => IndexAssign(array rename f, idx rename f, value rename f).setPos(pos)
    case Mutator(expr) => Mutator(expr rename f).setPos(pos)
    case Throw(value) => Throw(value rename f).setPos(pos)
  }
}

case class Assign(id: Identifier, expr: Expr) extends Statement

case class MultiAssign(ids: Seq[Identifier], value: Value) extends Statement

case class FieldAssign(obj: Value, name: String, value: Value) extends Statement

case class IndexAssign(array: Value, index: Value, value: Value) extends Statement

case class Mutator(expr: Expr) extends Statement

case class Throw(value: Value) extends Statement


sealed trait Expr extends ast.Positional {
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
    case id: Identifier if id != Default => f(id).setPos(id.pos)
    case (_: Value) | (_: Catch) => this
  }

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

case class BinaryOp(lhs: Value, op: String, rhs: Value) extends Expr

case class UnaryOp(expr: Value, op: String, postfix: Boolean) extends Expr

case class Call(fun: Value, args: Seq[Value]) extends Expr

case class New(tpe: Type, args: Seq[Value]) extends Expr

case class Index(expr: Value, index: Value) extends Expr

case class InstanceOf(expr: Value, tpe: Type) extends Expr

case class Unapply(tpe: Type, value: Value) extends Expr

case class Catch(tpe: Type) extends Expr

case class Phi(values: Seq[Value]) extends Expr

case class Field(value: Value, name: String) extends Expr


sealed trait Value extends Expr

case class Identifier(name: String) extends Value

case class Literal(value: String) extends Value

case class This() extends Value

case class Super() extends Value

object Default extends Identifier("$default")


sealed trait Type extends ast.Positional

case class PrimitiveType(tpe: ast.PrimitiveType) extends Type

case class ReferenceType(name: String) extends Type

case object MapType extends Type

case object ListType extends Type

case object UnknownType extends Type

