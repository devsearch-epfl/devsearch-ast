package devsearch.features

import devsearch.ast._

case class TypedVariable(position: CodeFilePosition, variableType: String, variableName: String) extends Feature(position) {
  def key: String = "variableDeclaration=" + variableName + " type=" + variableType
}

object ValDefFeatures extends FeatureExtractor {

  def convertTypeToString(tpe: Type): String = {
    // TODO(julien, mateusz): need to integrate as many types as possible, and make current strings more readable
    tpe match {
      case classType: ClassType => classType.name
      case primitiveType: PrimitiveType => primitiveType.getClass.getCanonicalName
      case arrayType: ArrayType => "Array[" + convertTypeToString(arrayType.base) + "]"
      case wildcardType: WildcardType => "? wildcard " + wildcardType.subType + " " + wildcardType.superType
      case typeHint: TypeHint => "type hint"
      case _ => "unknown_type"
    }
  }

  def extract(data: CodeFileData) = data.ast.collect[Feature] {
    case valueDefinition: ValDef if valueDefinition.name != Names.DEFAULT => Set(
      TypedVariable(data.location at valueDefinition.pos, convertTypeToString(valueDefinition.tpe), valueDefinition.name)
    )

    case _ => Set.empty
  }
}
