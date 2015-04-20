package devsearch.features

import devsearch.ast.Empty.NoType
import devsearch.ast._

case class TypedVarFeature(position: CodeFilePosition, variableType: String, variableName: String) extends Feature(position) {
  def key: String = "variableDeclaration=" + variableName + " type=" + variableType
}

object ValDefExtractor extends FeatureExtractor {

  def convertTypeToString(tpe: Type): String = {
    tpe match {
      case classType: ClassType => classType.name
      case primitiveType: PrimitiveType => primitiveType.getClass.getSimpleName.stripSuffix("$")
      case arrayType: ArrayType => "Array[" + convertTypeToString(arrayType.base) + "]"
      case functionType: FunctionType => functionType.from + " -> " + functionType.to
      case wildcardType: WildcardType => "? wildcard " + wildcardType.subType + " " + wildcardType.superType
      case AnyType => "Any"
      case BottomType => "Bottom"
      case typeHint: TypeHint => "type hint: " + typeHint.hint
      case complexType: ComplexType => "complex type"
      case NoType => "NoType"
      case _ => "unknown type"
    }
  }

  def extract(data: CodeFileData) = data.ast.collect[Feature] {
    case valueDefinition: ValDef if valueDefinition.name != Names.DEFAULT => Set(
      TypedVarFeature(data.location at valueDefinition.pos, convertTypeToString(valueDefinition.tpe), valueDefinition.name)
    )

    case _ => Set.empty
  }
}