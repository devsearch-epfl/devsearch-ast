# DevSearch ASTs

Source code parsing and analysis framework for the [DevSearch](http://devsearch.ch) project. This repository
gathers the common language AST format, all implemented language parsers as well as the feature extractors
that transform ASTs into features from which indexes and models can be built.

## Common AST

We have implemented a common language-agnostic AST structure into which we fit all our parsed code. This AST is mostly syntactic
but we try to preserve as much semantic information as possible to improve the quality and variety of features we are able to
extract. For more specific information, see the quite readable AST source code
[here](https://github.com/devsearch-epfl/devsearch-ast/blob/master/src/main/scala/devsearch/ast/AST.scala).

## Supported Languages

We currently have parsers for
- Java
- Scala
- Query language (a super-set of Scala with support for [Quasiquote](http://docs.scala-lang.org/overviews/quasiquotes/intro.html)-like
  holes in the source)
- Compatibility parser for [DevMine](http://devmine.ch) JSON ASTs that can support any language the DevMine project parses
  (we currently only ship the Go parser)

We also have plans to support Python and JavaScript in a possibly near future.

## Feature Extraction

Feature extraction consists in extracting a set of identifiers from a given source code AST. Each feature is associated to a code
position (user/repo/path:line) that can then be used to build feature indexes and similarity models between different ASTs. Basically,
a feature is simply a key with a well-defined equality function along with the position in the AST at which it was extracted:
```scala
trait Feature {
  def pos: CodeFilePosition
  def key: String
}
```

We provide a helper object `Features` that can be used to extract all features from a given AST through `apply(ast: AST)`.
