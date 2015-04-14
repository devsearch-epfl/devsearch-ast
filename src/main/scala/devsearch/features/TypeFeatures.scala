package devsearch.features

import devsearch.ast._

case class TypeReference(position: CodeFilePosition, path: String) extends Feature(position) {
  def key: String = "typeReference=" + path
}

object TypeFeatures extends FeatureExtractor {

  object ReferenceExtractor {
    def unapply(expr: Expr): Option[String] = expr match {
      case Ident(Names.DEFAULT) => None
      case Ident(name) => Some(name)
      case FieldAccess(qual, name, _) => unapply(qual).map {
        case "" => name
        case q => q + "." + name
      }
      case Empty.NoExpr => Some("")
      case _ => None
    }
  }

  // TODO: ignore common name parts, like com, org, org.apache, etc. to get matches with
  //       other languages that don't have long paths, like python
  def extract(data: CodeFileData) = data.ast.collect[Feature] {
    case ct @ ClassType(ReferenceExtractor(path), name, _, _) if name != Names.DEFAULT =>
      Set(TypeReference(data.location at ct.pos, if (path == "") name else path + "." + name))
    case i @ Import(name, _, _) if name != Names.DEFAULT =>
      Set(TypeReference(data.location at i.pos, name))
    case _ => Set.empty
  }
}
