package org.geoscript.geocss

import filter.FilterOps.filters

import collection.JavaConversions._

import java.util.Arrays
import org.opengis.filter.Filter

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

/**
 * A marker trait for values that can be taken by a styling property.
 */
trait Value

/**
 * A Literal is a value that exhibits no dynamic behavior
 */
case class Literal(body: String) extends Value

/**
 * A Function is a built-in function of CSS such as URL or RGB which denotes a
 * special interpretation of a value.
 */
case class Function(name: String, parameters: Seq[Value]) extends Value

/**
 * An Expression is a CQL query embedded in a CSS styling property.
 */
case class Expression(body: String) extends Value

/**
 * A styling property
 */
case class Property(name: String, values: Seq[Seq[Value]]) {
  override def toString = {
    "%s: %s".format(
      name,
      values.map(_.mkString("[", ",", "]")).mkString(",")
    )
  }
}

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
      SelectorOps.simplify(this.selectors ++ that.selectors),
      this.contexts ++ that.contexts
    )

  /**
   * Is it possible that a feature could meet the constraints in this rule's
   * selectors?
   */
  lazy val isSatisfiable =
    !(selectors contains SelectorOps.Exclude)

  /**
   * Create an OGC filter corresponding to the Selectors on this rule which are
   * expressible as OGC filters. Other Selector types will be omitted.
   */
  def getFilter =
    AndSelector(selectors filter { _.filterOpt.isDefined }).filterOpt.get

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
    OrSelector(selectors map SelectorOps.not)

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

/**
 * A Selector expresses some subset of all possible features.  It restricts the
 * conditions under which a Rule will be applied to a feature, including but
 * not limited to requirements regarding the feature's attributes.
 */
abstract class Selector {
  /**
   * An Option containing the OGC Filter equivalent to this Selector, if it
   * exists.
   */
  def filterOpt: Option[Filter]
}

/**
 * A Selector which filters on typical database attributes.
 */
abstract class DataSelector extends Selector {
  protected val filters = 
    org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)

  /**
   * The OGC Filter corresponding to this Selector
   */
  def asFilter: Filter
  override def filterOpt = Some(asFilter)
}

/**
 * A Selector which filters on something other than typical database
 * attributes.
 */
abstract class MetaSelector extends Selector {
  def filterOpt = None
}

/**
 * A Selector which only applies to pseudo-elements
 */
trait Context extends MetaSelector

/**
 * A Selector which only accepts the feature with a specific FID.  This
 * corresponds to the #id123 syntax in CSS, for example.
 */
case class IdSelector(id: String) extends DataSelector {
  /**
   * The (singleton) set of OGC Identifier objects for this selector.
   */
  val idSet: java.util.Set[org.opengis.filter.identity.Identifier] = {
    val set = new java.util.HashSet[org.opengis.filter.identity.Identifier]
    set.add(filters.featureId(id))
    java.util.Collections.unmodifiableSet[org.opengis.filter.identity.Identifier](set)
  }

  override def asFilter = filters.id(idSet)

  override def toString = "#" + id
}

/**
 * A Selector that never rejects anything, corresponding to the '*' syntax in
 * CSS.
 */
case object AcceptSelector extends DataSelector {
  override def asFilter = org.opengis.filter.Filter.INCLUDE
  override def toString = "*"
}

/**
 * A Selector which constrains based on typename.
 */
case class TypenameSelector(typename: String) extends MetaSelector {
  override def toString = typename
}

/**
 * A Selector which uses CQL-like syntax to express constraints on a contextual
 * property such as the scale denominator at render time.  This corresponds to
 * the [&64;scale &gt; 10000] syntax in CSS, for example.
 */
case class PseudoSelector(property: String, operator: String, value: String)
extends MetaSelector {
  override def toString = "@%s%s%s".format(property, operator, value)
}

/**
 * A Selector which only matches pseudo-features such as geometries generated
 * by well-known-marks.  This correponds to the :mark syntax in CSS, for
 * example.
 */
case class PseudoClass(name: String) extends Context {
  override def toString = ":%s".format(name)
}


/**
 * A Selector which only matches pseudo-features such as geometries generated by
 * well-known-marks, with additional parameters besides just the name.  This
 * corresponds to the :nth-mark(n) syntax in CSS, for example.
 */
case class ParameterizedPseudoClass(name: String, param: String) 
extends Context {
  override def toString = ":%s(%s)".format(name, param)
}

/**
 * A Selector which wraps a CQL expression.
 */
case class ExpressionSelector(expression: String) extends DataSelector {
  override lazy val asFilter = 
    org.geotools.filter.text.ecql.ECQL.toFilter(expression)
  override def toString = expression
}

/**
 * A Selector which wraps an OGC Filter instance.
 */
case class WrappedFilter(filter: org.opengis.filter.Filter) 
extends DataSelector {
  override def asFilter = filter
  override def toString = filter.toString
}

/**
 * A selector which wraps another and reverses its decisions (it accepts only
 * features that would not be accepted by the wrapped selector).
 */
case class NotSelector(selector: Selector) extends Selector {
  override def filterOpt =
    selector match {
      case NotSelector(sel) => sel.filterOpt
      case sel => 
        selector.filterOpt map {
          case org.opengis.filter.Filter.EXCLUDE =>
            org.opengis.filter.Filter.INCLUDE
          case org.opengis.filter.Filter.INCLUDE =>
            org.opengis.filter.Filter.EXCLUDE
          case f =>
            filters.not(f)
        }
  }
}

/**
 * An aggregate selector which accepts features accepted by all of its members.
 */
case class AndSelector(children: Seq[Selector]) extends Selector {
  override def filterOpt =
    if (children.forall(_.filterOpt.isDefined)) {
      val operands = children map { _.filterOpt.get }
      Some(
        if (operands contains Filter.EXCLUDE) {
          Filter.EXCLUDE
        } else {
          operands.filter(Filter.INCLUDE !=) match {
            case Seq() => Filter.INCLUDE
            case Seq(f) => f
            case fs => filters.and(fs)
          }
        }
      )
    } else {
      None
    }
}

/**
 * An aggregate selector which accepts features accepted by any of its members.
 */
case class OrSelector(children: Seq[Selector]) extends Selector {
  override def filterOpt =
    if (children.forall(_.filterOpt.isDefined)) {
      val operands = children map { _.filterOpt.get }
      Some(
        if (operands.exists {_ == Filter.INCLUDE}) {
          Filter.INCLUDE
        } else {
          val parts = operands.partition(Filter.EXCLUDE==)
          parts._2 match {
            case Seq() if (parts._1.isEmpty) => Filter.INCLUDE
            case Seq() => Filter.EXCLUDE
            case Seq(f) => f
            case fs => filters.or(fs)
          }
        }
      )
    } else {
      None
    }
}
