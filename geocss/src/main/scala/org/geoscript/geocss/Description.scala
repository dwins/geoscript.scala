package org.geoscript.geocss

/**
 * A Description contains the metadata for a Rule, if such metadata is present.
 */
case class Description(
  /** A string to contribute to the Rule's title in legends */
  title: Option[String],
  /** A short description of the Rule's intent */
  abstrakt: Option[String]
) {
  /** 
   * Combine this rule with another by concatenating their titles and
   * abstracts
   */
  def merge(that: Description) = {
    def compose(a: Option[String], b: Option[String]) = 
      (a, b) match {
        case (Some(a), Some(b)) => Some(a + " with " + b)
        case (Some(a), None   ) => Some(a)
        case (None,    Some(b)) => Some(b)
        case (None,    None   ) => None
      }

    Description(
      compose(this.title, that.title),
      compose(this.abstrakt, that.abstrakt)
    )
  }
}

object Description {
  val empty = Description(None, None)

  private def extract(comment: String, keyword: String): Option[String] = {
    val pattern = ("""\s*@""" + keyword + """:?\s*""").r

    comment.lines.map(_.replaceFirst("""\s*\*""", "")).find {
      line => pattern.findPrefixOf(line) != None
    } map { pattern.replaceFirstIn(_, "") }
  }

  def apply(comment: String): Description = {
    val title = extract(comment, "title")
    val abst  = extract(comment, "abstract")
    val res = Description(title, abst)
    res
  }
}
