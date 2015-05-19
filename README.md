# DevSearch ASTs

Source code parsing and analysis framework for the [DevSearch](http://devsearch.ch) project. This repository
gathers the common language AST format, all implemented language parsers as well as the feature extractors
that transform ASTs into features from which indexes and models can be built.

## Common AST

We have implemented a common language-agnostic AST structure into which we fit all our parsed code. This AST is mostly syntactic
but we try to preserve as much semantic information as possible to improve the quality and variety of features we are able to
extract. For more specific information, see the quite readable AST source code
[here](https://github.com/devsearch-epfl/devsearch-ast/blob/master/src/main/scala/devsearch/ast/AST.scala).

### Normal SSA form

The common AST format described above is quite useful to build syntactic features but can be limited when considering more
semantic information. In order to facilitate reasoning about code semantics, we provide a normal SSA form that can be
extracted from an AST through `devsearch.normalized.Normalizer(ast)`. This will return a definition tree with named nodes
where functions are attributed a `ControlFlowGraph` (see 
[here](https://github.com/devsearch-epfl/devsearch-ast/blob/master/src/main/scala/devsearch/normalized/ControlFlowGraphs.scala))
and graph nodes represent code blocks. For more details about the SSA instructions contained in the control-flow blocks, see
the SSA source code 
[here](https://github.com/devsearch-epfl/devsearch-ast/blob/master/src/main/scala/devsearch/normalized/SSA.scala).

Currently, the normal form has two failings:
- not array-SSA, so array updates and merges are not currently extracted during normalization. However, it would be quite simple to
implement a transformation on the current SSA form to bring it into array-SSA.
- not functionally readable; it is still somewhat complex to extract functional properties from imperative code in this normal
form (see [SemanticExtractor](https://github.com/devsearch-epfl/devsearch-ast/blob/master/src/main/scala/devsearch/features/SemanticExtractor.scala)).
It would be interesting to extract a Fold-normal form from this SSA form in order to improve such analysis.

## Supported Languages

We currently have parsers for
- Java
- Scala
- Query language (a super-set of Scala with support for [Quasiquote](http://docs.scala-lang.org/overviews/quasiquotes/intro.html)-like
  holes in the source)
- Compatibility parser for [DevMine](http://devmine.ch) JSON ASTs that can support any language the DevMine project parses
  (we currently only ship the Go parser)
- JavaScript 

We also have plans to support Python in a possibly near future.

### Adding parsers

New parsers simply need to extend the `devsearch.parsers.Parser` trait and provide both
```scala
def language: String
def parse(source: Source): String
```
methods. If one wishes to reap the benefits of automatic language detection to extend support for language-agnostic queries,
the new parser with associated meta-data should be added to `devsearch.parsers.Languages.orderedLangList` as well. The
language detection if performed by the `devsearch.features.QueryRecognizer` object.

## Feature Extraction

Feature extraction consists in extracting a set of identifiers from a given source code AST. Each feature is associated to a code
position (user/repo/path:line) that can then be used to build feature indexes and similarity models between different ASTs. Basically,
a feature is simply a key with a well-defined equality function along with the position in the AST at which it was extracted:
```scala
trait Feature {
  def pos: CodePiecePosition
  def key: String
}
```

We provide a helper object `devsearch.features.FeatureRecognizer` that can be used to extract all features from a given AST
through `apply(ast: AST)`.

### Adding extractors

New feature extractors must extend the `devsearch.features.FeatureExtractor` trait and provide the
```scala
def extract(data: CodeFile): Set[Feature]
```
method that will (typically) traverse `data.ast` to accumulate code features. Note that our features are positioned in the code
to enable precise subsequent lookups in the source-code. This is linked to the `Feature.pos` described above and to build positions
one should use the `data.location.at(tree.pos)` construction (see existing feature extractors).

In order to add the new extractor to the set of extractors applied by `FeatureRecognizer`, the extractor must be added to the
`extractors` set in the `FeatureRecognizer` object
[here](https://github.com/devsearch-epfl/devsearch-ast/blob/master/src/main/scala/devsearch/features/FeatureRecognizer.scala).


