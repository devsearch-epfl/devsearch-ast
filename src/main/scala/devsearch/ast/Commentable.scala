package devsearch.ast

/**
 * Commentable
 *
 * Trait that enables comment attribution to a given tree (or other structure).
 * Comments are stored as options to model the likely case of no comment for a given node,
 * and we provide `setComment`, `appendComment` and `prependComment` utilities to modify
 * the internally stored comment option.
 */
trait Commentable {
  private var _comment: Option[String] = None
  def comment: Option[String] = _comment

  def setComment(comment: String): this.type = {
    _comment = Some(comment)
    this
  }

  /** Extends the current comment by adding `comment` at the end, or setting it if no comment is defined */
  def appendComment(comment: String): this.type = {
    setComment(_comment match {
      case Some(c) => c +"\n\n" + comment
      case None => comment
    })
  }

  def appendComment(opt: Option[String]): this.type = {
    opt.foreach(appendComment)
    this
  }

  /** Extends the current comment by adding `comment` at the beginning, or setting it if no comment is defined */
  def prependComment(comment: String): this.type = {
    setComment(_comment match {
      case Some(c) => comment +"\n\n" + c
      case None => comment
    })
  }

  def prependComment(opt: Option[String]): this.type = {
    opt.foreach(prependComment)
    this
  }
}

