package devsearch.features

import devsearch.utils._
import org.scalatest.FlatSpec

class ValDefFeaturesTest extends FlatSpec with CodeProvider {
  "variable declaration extractor" should "extract all variable declarations" in {
    // TODO(julien, mateusz): add a test for each type name
    assert(Set[Feature](
      TypedVariable(location at 376, "Exception", "e"),
      TypedVariable(location at 32,  "devsearch.ast.PrimitiveTypes.Int$", "intWithUnderscore"),
      TypedVariable(location at 109, "Array[Array[devsearch.ast.PrimitiveTypes.Int$]]", "arr4")
    ).subsetOf(ValDefFeatures.extract(code)))
  }


  it should "toString correctly" in {
    assert(ValDefFeatures.extract(code).toList.map(_.toString).sortBy(e => e).foldLeft("")((acc, curr) => acc + curr+"\n") ==
      """variableDeclaration=a type=A,unknown_user,unknown_repo,JavaConcepts.java,374
        |variableDeclaration=a type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,52
        |variableDeclaration=a type=devsearch.ast.PrimitiveTypes.String$,unknown_user,unknown_repo,JavaConcepts.java,307
        |variableDeclaration=args type=Array[devsearch.ast.PrimitiveTypes.String$],unknown_user,unknown_repo,JavaConcepts.java,262
        |variableDeclaration=args type=Array[devsearch.ast.PrimitiveTypes.String$],unknown_user,unknown_repo,JavaConcepts.java,64
        |variableDeclaration=arr type=Array[devsearch.ast.PrimitiveTypes.Int$],unknown_user,unknown_repo,JavaConcepts.java,24
        |variableDeclaration=arr2 type=Array[Array[Array[Array[devsearch.ast.PrimitiveTypes.Int$]]]],unknown_user,unknown_repo,JavaConcepts.java,101
        |variableDeclaration=arr3 type=Array[Array[devsearch.ast.PrimitiveTypes.Int$]],unknown_user,unknown_repo,JavaConcepts.java,107
        |variableDeclaration=arr4 type=Array[Array[devsearch.ast.PrimitiveTypes.Int$]],unknown_user,unknown_repo,JavaConcepts.java,109
        |variableDeclaration=arrLS type=Array[Array[List]],unknown_user,unknown_repo,JavaConcepts.java,46
        |variableDeclaration=b type=devsearch.ast.PrimitiveTypes.Boolean$,unknown_user,unknown_repo,JavaConcepts.java,174
        |variableDeclaration=b type=devsearch.ast.PrimitiveTypes.Boolean$,unknown_user,unknown_repo,JavaConcepts.java,68
        |variableDeclaration=b type=devsearch.ast.PrimitiveTypes.Byte$,unknown_user,unknown_repo,JavaConcepts.java,54
        |variableDeclaration=binaryLiteral type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,44
        |variableDeclaration=bye type=devsearch.ast.PrimitiveTypes.Byte$,unknown_user,unknown_repo,JavaConcepts.java,26
        |variableDeclaration=byebye type=Array[devsearch.ast.PrimitiveTypes.Byte$],unknown_user,unknown_repo,JavaConcepts.java,28
        |variableDeclaration=c type=Comparator,unknown_user,unknown_repo,JavaConcepts.java,370
        |variableDeclaration=cc type=devsearch.ast.PrimitiveTypes.Char$,unknown_user,unknown_repo,JavaConcepts.java,105
        |variableDeclaration=clz1 type=Class,unknown_user,unknown_repo,JavaConcepts.java,16
        |variableDeclaration=clz2 type=Class,unknown_user,unknown_repo,JavaConcepts.java,18
        |variableDeclaration=clz3 type=Class,unknown_user,unknown_repo,JavaConcepts.java,20
        |variableDeclaration=clz4 type=Class,unknown_user,unknown_repo,JavaConcepts.java,22
        |variableDeclaration=cu type=CompilationUnit,unknown_user,unknown_repo,JavaConcepts.java,264
        |variableDeclaration=diamond1 type=List,unknown_user,unknown_repo,JavaConcepts.java,57
        |variableDeclaration=doubleWithUnderscore type=devsearch.ast.PrimitiveTypes.Double$,unknown_user,unknown_repo,JavaConcepts.java,40
        |variableDeclaration=doubleWithUnderscoreAndExponent type=devsearch.ast.PrimitiveTypes.Double$,unknown_user,unknown_repo,JavaConcepts.java,42
        |variableDeclaration=e type=Error,unknown_user,unknown_repo,JavaConcepts.java,357
        |variableDeclaration=e type=Exception,unknown_user,unknown_repo,JavaConcepts.java,357
        |variableDeclaration=e type=Exception,unknown_user,unknown_repo,JavaConcepts.java,376
        |variableDeclaration=e type=IOException,unknown_user,unknown_repo,JavaConcepts.java,341
        |variableDeclaration=e type=IOException,unknown_user,unknown_repo,JavaConcepts.java,347
        |variableDeclaration=e type=NullPointerException,unknown_user,unknown_repo,JavaConcepts.java,318
        |variableDeclaration=e type=RuntimeException,unknown_user,unknown_repo,JavaConcepts.java,320
        |variableDeclaration=e type=RuntimeException,unknown_user,unknown_repo,JavaConcepts.java,336
        |variableDeclaration=e type=RuntimeException,unknown_user,unknown_repo,JavaConcepts.java,355
        |variableDeclaration=fff type=devsearch.ast.PrimitiveTypes.Float$,unknown_user,unknown_repo,JavaConcepts.java,103
        |variableDeclaration=file type=File,unknown_user,unknown_repo,JavaConcepts.java,306
        |variableDeclaration=floatWithUnderscore type=devsearch.ast.PrimitiveTypes.Float$,unknown_user,unknown_repo,JavaConcepts.java,36
        |variableDeclaration=floatWithUnderscoreAndExponent type=devsearch.ast.PrimitiveTypes.Float$,unknown_user,unknown_repo,JavaConcepts.java,38
        |variableDeclaration=i type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,177
        |variableDeclaration=i type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,213
        |variableDeclaration=i type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,294
        |variableDeclaration=i type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,297
        |variableDeclaration=i type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,300
        |variableDeclaration=iii type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,225
        |variableDeclaration=iii type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,229
        |variableDeclaration=in type=InputStream,unknown_user,unknown_repo,JavaConcepts.java,339
        |variableDeclaration=in type=InputStream,unknown_user,unknown_repo,JavaConcepts.java,344
        |variableDeclaration=in type=InputStream,unknown_user,unknown_repo,JavaConcepts.java,350
        |variableDeclaration=in2 type=InputStream,unknown_user,unknown_repo,JavaConcepts.java,345
        |variableDeclaration=intWithUnderscore type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,32
        |variableDeclaration=integer type=Integer,unknown_user,unknown_repo,JavaConcepts.java,365
        |variableDeclaration=j type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,297
        |variableDeclaration=j type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,300
        |variableDeclaration=longWithUnderscore type=devsearch.ast.PrimitiveTypes.Long$,unknown_user,unknown_repo,JavaConcepts.java,34
        |variableDeclaration=min type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,233
        |variableDeclaration=minl type=devsearch.ast.PrimitiveTypes.Long$,unknown_user,unknown_repo,JavaConcepts.java,235
        |variableDeclaration=o1 type=Object,unknown_user,unknown_repo,JavaConcepts.java,372
        |variableDeclaration=o2 type=Object,unknown_user,unknown_repo,JavaConcepts.java,372
        |variableDeclaration=obj type=Object,unknown_user,unknown_repo,JavaConcepts.java,382
        |variableDeclaration=qwe type=QWE,unknown_user,unknown_repo,JavaConcepts.java,267
        |variableDeclaration=sh1 type=devsearch.ast.PrimitiveTypes.Short$,unknown_user,unknown_repo,JavaConcepts.java,30
        |variableDeclaration=sh2 type=devsearch.ast.PrimitiveTypes.Short$,unknown_user,unknown_repo,JavaConcepts.java,30
        |variableDeclaration=sl type=devsearch.ast.PrimitiveTypes.Long$,unknown_user,unknown_repo,JavaConcepts.java,234
        |variableDeclaration=str type=devsearch.ast.PrimitiveTypes.String$,unknown_user,unknown_repo,JavaConcepts.java,205
        |variableDeclaration=string type=ABC,unknown_user,unknown_repo,JavaConcepts.java,365
        |variableDeclaration=t type=JavaConcepts,unknown_user,unknown_repo,JavaConcepts.java,111
        |variableDeclaration=teste type=JavaConcepts,unknown_user,unknown_repo,JavaConcepts.java,266
        |variableDeclaration=val1 type=A,unknown_user,unknown_repo,JavaConcepts.java,395
        |variableDeclaration=val1 type=T,unknown_user,unknown_repo,JavaConcepts.java,171
        |variableDeclaration=val2 type=B,unknown_user,unknown_repo,JavaConcepts.java,395
        |variableDeclaration=val2 type=E,unknown_user,unknown_repo,JavaConcepts.java,172
        |variableDeclaration=x type=List,unknown_user,unknown_repo,JavaConcepts.java,257
        |variableDeclaration=x type=Map,unknown_user,unknown_repo,JavaConcepts.java,369
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,156
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,158
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,169
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,219
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,263
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,62
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.Long$,unknown_user,unknown_repo,JavaConcepts.java,199
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.String$,unknown_user,unknown_repo,JavaConcepts.java,215
        |variableDeclaration=x type=devsearch.ast.PrimitiveTypes.String$,unknown_user,unknown_repo,JavaConcepts.java,308
        |variableDeclaration=y type=devsearch.ast.PrimitiveTypes.Boolean$,unknown_user,unknown_repo,JavaConcepts.java,174
        |variableDeclaration=y type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,195
        |variableDeclaration=y type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,309
        |variableDeclaration=y type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,51
        |variableDeclaration=z type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,211
        |variableDeclaration=z type=devsearch.ast.PrimitiveTypes.Int$,unknown_user,unknown_repo,JavaConcepts.java,51
        |""".stripMargin
    )
  }
}
