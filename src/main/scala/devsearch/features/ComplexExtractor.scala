package devsearch.features

import devsearch.ast._

case class MapCallFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "map call"
}

case class FlatMapCallFeature(position: CodePiecePosition) extends Feature(position) {
  def key: String = "flatMap call"
}

object ComplexExtractor extends FeatureExtractor {

  def extract(data: CodeFileData) = {
    var features = Set.empty[Feature]
    var idents = Set.empty[String]
    var stack = List.empty[(AST, Set[String])]

    def lookup(name: String): Option[AST] = stack.find { case (ast, ids) => ids(name) }.map(_._1)

    def isListType(ct: ClassType): Boolean = ct match {
      case ClassType(_, "List", _, _) => true
      case ClassType(_, "LinkedList", _, _) => true
      case ClassType(_, "ArrayList", _, _) => true
      case _ => false
    }

    val complexTraverser = new Traverser {
      override def traverse(ast: AST): Unit = ast match {
        case fc @ FunctionCall(Ident("map"), _, List(f)) =>
          features += MapCallFeature(data.location at fc.pos)
          super.traverse(fc)

        case fc @ FunctionCall(Ident("flatMap"), _, List(f)) =>
          features += FlatMapCallFeature(data.location at fc.pos)
          super.traverse(fc)

        case fc @ FunctionCall(FieldAccess(_, "map", _), _, List(f)) =>
          features += MapCallFeature(data.location at fc.pos)
          super.traverse(fc)

        case fc @ FunctionCall(FieldAccess(_, "flatMap", _), _, List(f)) =>
          features += FlatMapCallFeature(data.location at fc.pos)
          super.traverse(fc)

        case fc @ FunctionCall(FieldAccess(Ident(name), "add", _), _, List(elem)) =>
          lookup(name) match {
            case Some(ast) if ast == stack.head._1 => features += MapCallFeature(data.location at ast.pos)
            case Some(ast) => features += FlatMapCallFeature(data.location at ast.pos)
            case _ =>
          }
          super.traverse(fc)

        case fc @ FunctionCall(FieldAccess(Ident(name), "addAll", _), _, List(elem)) =>
          features += FlatMapCallFeature(data.location at ast.pos)
          super.traverse(fc)

        case _ : Foreach | _ : For =>
          val (savedStack, savedIdents) = (stack, idents)
          stack ::= ast -> idents
          idents = Set.empty
          super.traverse(ast)
          stack = savedStack
          idents = savedIdents

        case Assign(Ident(name), ConstructorCall(tpe, _, _), None) if isListType(tpe) =>
          idents += name
          super.traverse(ast)

        case ValDef(_, name, _, _, ConstructorCall(tpe, _, _), _) if isListType(tpe) =>
          idents += name
          super.traverse(ast)

        case _ : Block =>
          val savedIdents = idents
          super.traverse(ast)
          idents = savedIdents

        case _ => super.traverse(ast)
      }
    }

    complexTraverser(data.ast)

    features
  }
}

