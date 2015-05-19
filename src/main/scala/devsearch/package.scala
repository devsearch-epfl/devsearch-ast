package devsearch

/** The DevSearch common AST format.
  *
  * The devsearch projects provides a common AST format that unifies languages into a single
  * abstract representation. The base AST type is [[ast.AST]] and additional information about
  * input sources, positions, comments and operations on ASTs can be found at:
  *  - [[ast.Source]]
  *  - [[ast.Positional]]
  *  - [[ast.Commentable]]
  *  - [[ast.Operators]]
  */
package object ast {}

/** Language parsers supported by the DevSearch project.
  *
  * Parsers must conform to the [[parsers.Parser]] interface that requires the methods
  * {{{
  * def language: String
  * def parse(source: Source): AST
  * }}}
  * to be defined. In case of parsing failures, the `parse` method should throw
  * [[parsers.ParsingFailedError]] to provide information about invalid source code.
  *
  * To enable language-agnostic parsing, the [[parsers.Languages.orderedLangList]] value should
  * be extended with any new parser additions.
  */
package object parsers {}

/** Code feature definition and extraction.
  *
  * In order to perform scalable analysis and identify code snippets that are close to each
  * other, we provide feature extractors that enable indexed lookup of code. The feature
  * interface is extremely flexible and each code feature is fully determined by a string
  * key (unique among all features) and a position in a code file (see [[features.Feature]]).
  *
  * To facilitate feature extraction for a given [[features.CodeFile]], we provide the
  * [[features.FeatureRecognizer]] object in order to extracts all features from a given code file.
  *
  * Also, when the source language is unknown, one can use the [[features.QueryRecognizer]]
  * object to extract a code file from a language-agnostic source snippet.
  *
  * For a brief overview, one can look at:
  *  - [[features.CodeFile]], [[features.CodeFileLocation]] and [[features.CodePiecePosition]] for
  *    feature extractor inputs and feature positioning
  *  - [[features.Feature]] and [[features.FeatureExtractor]] for new feature definition (don't
  *    forget to extend the [[features.FeatureRecognizer.extractors]] list with your new feature
  *    extractor)
  */
package object features {}

/** AST transformation to normal form(s)
  *
  * Some analysis is complex to perform on ASTs which are more focused on syntax than
  * semantics. For this reason, we provide an SSA normal form that enables simpler analysis
  * for code functionalities and semantic information.
  *
  * The normal form is composed of a definition tree where each definition simply consists
  * in a name and a list of children definitions (see [[Definition]]). Function definitions
  * are further annotated with a control-flow graph (see [[Graph]] and [[ControlFlowGraph]])
  * where graph nodes contain SSA statements. The kind of statements these will consist in
  * can be found by taking a look at the [[Statement]], [[Expr]] and [[Type]] hierarchies.
  *
  * For more information, look at:
  *  - [[AstExtraction]] for transformation into SSA normal form
  *  - [[ControlFlowGraphs]], [[Graph]] and [[ControlFlowGraph]] for available operations on
  *    flow graphs
  *  - [[ScopingRenamer]] for unique name assignment in scopes
  *  - [[SingleAssignment]] for re-assignment cleaning and phi-function injection
  *
  * Known limitations of the current SSA form are:
  *  - Not array-SSA, could be extended with a rather simple transformation from the current form
  *  - No handling of call-by-name which cannot be computed without a valid symbol table, which we
  *    do not have
  *  - Not so useful for certain functional properties, but is a good base-step for going towards
  *    a Fold-normal form
  */
package object normalized {}
