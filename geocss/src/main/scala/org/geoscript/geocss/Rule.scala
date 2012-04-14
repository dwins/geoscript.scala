package org.geoscript.geocss
import org.geoscript.support.logic.reduce

/**
 * A Rule is the basic unit of a CSS style.  Rules identify a subset of all
 * possible features and a set of styling properties to apply to those
 * features.
 */
case class Rule(
  /** Metadata for this rule, to use in legends, etc. */
  description: Description,
  /** Selectors expressing the set of features to which this rule applies */
  selectors: Seq[Selector],
  /** A List of property lists to apply in different rendering contexts */
  contexts: Seq[Pair[Option[Context], Seq[Property]]]
) {
  /**
   * Combine this rule with another rule, producing a single rule with all
   * properties of both.  This method is not commutative; properties from this
   * rule will take precedence over properties from the operand.
   */
  def merge(that: Rule): Rule =
    Rule(
      this.description merge that.description,
      this.selectors ++ that.selectors,
      this.contexts ++ that.contexts
    )

  /**
   * Is it possible that a feature could meet the constraints in this rule's
   * selectors?
   */
  lazy val isSatisfiable = {
    !(reduce[Selector](And(selectors)) == Exclude)
  }

  /**
   * Create an OGC filter corresponding to the Selectors on this rule which are
   * expressible as OGC filters. Other Selector types will be omitted.
   */
  def getFilter = realize(And(selectors))

  /**
   * The properties to use in the "normal" context, outside of well-known-marks
   * etc.
   */
  def properties =
    contexts.filter(_._1 == None) flatMap (_._2)

  /**
   * A selector which matches the complement of features accepted by this one.
   */
  def negatedSelector =
    Or(selectors map (Not(_)))

  /**
   * Retrieve the properties to be applied in a particular context.  Contexts
   * consist of a well-known-mark type (fill/stroke/mark) and an ordering
   * index.
   */
  def context(symbol: String, order: Int): Seq[Property] = {
    val keys = Seq(
      ParameterizedPseudoClass("nth-" + symbol, order.toString),
      ParameterizedPseudoClass("nth-" + "symbol", order.toString),
      PseudoClass(symbol),
      PseudoClass("symbol")
    ) map (Some(_))

    contexts.filter { keys contains _._1 } flatMap (_._2)
  }
}

/**
 * The Rule with no description, no constraints on features, and no properties.
 */
object EmptyRule extends Rule(Description.empty, Seq.empty, Seq.empty)
