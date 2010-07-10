package org.geoserver.community.css

import filter.FilterOps.filters

import java.util.Arrays

import org.opengis.filter.Filter

case class Description(title: Option[String], abstrakt: Option[String])
object Description {
  val Empty = Description(None, None)

  def extract(comment: String, keyword: String): Option[String] = {
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

  def combine(lhs: Description, rhs: Description): Description = {
    def merge(a: Option[String], b: Option[String]) = (a, b) match {
      case (Some(a), Some(b)) => Some(a + " with " + b)
      case (Some(a), None)    => Some(a)
      case (None, Some(b))    => Some(b)
      case (None, None)       => None
    }

    Description(
      merge(lhs.title, rhs.title),
      merge(lhs.abstrakt, rhs.abstrakt)
    )
  }
}

case class Identifier(name: String)
case class Combinator(operator: String)

class Value
case class Literal(body: String) extends Value
case class Function(name: String, parameters: List[Value]) extends Value
case class Expression(body: String) extends Value

case class Property(name: String, values: List[List[Value]]) {
  override def toString = {
    "%s: %s".format(
      name,
      values.map(_.mkString("[", ",", "]")).mkString(",")
    )
  }
}

case class Rule(
  comment: Description,
  selectors: List[List[Selector]],
  properties: List[Property]
)

abstract class Selector {
  def filterOpt: Option[Filter]
}

abstract class DataSelector extends Selector {
  protected val filters = 
    org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)
  def asFilter: Filter
  override def filterOpt = Some(asFilter)
}

abstract class MetaSelector extends Selector {
  def filterOpt = None
}

case class IdSelector(id: String) extends DataSelector {
  val idSet: java.util.Set[org.opengis.filter.identity.Identifier] = {
    val set = new java.util.HashSet[org.opengis.filter.identity.Identifier]
    set.add(filters.featureId(id))
    java.util.Collections.unmodifiableSet[org.opengis.filter.identity.Identifier](set)
  }

  override def asFilter = filters.id(idSet)

  override def toString = "#" + id
}

case object AcceptSelector extends DataSelector {
  override def asFilter = org.opengis.filter.Filter.INCLUDE
  override def toString = "*"
}

case class TypenameSelector(typename: String) extends MetaSelector {
  override def toString = typename
}

case class PseudoSelector(property: String, operator: String, value: String) extends MetaSelector {
  override def toString = "@%s%s%s".format(property, operator, value)
}

case class PseudoClass(name: String) extends MetaSelector {
  override def toString = ":%s".format(name)
}

case class ExpressionSelector(expression: String) extends DataSelector {
  private val filter = org.geotools.filter.text.ecql.ECQL.toFilter(expression)
  override def asFilter = filter
  override def toString = expression
}

case class WrappedFilter(filter: org.opengis.filter.Filter) extends DataSelector {
  override def asFilter = filter
  override def toString = filter.toString
}

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

case class AndSelector(children: List[Selector]) extends Selector {
  override def filterOpt = {
    if (children.forall(_.filterOpt.isDefined)) {
      val operands = children map { _.filterOpt.get }
      Some(
        if (operands contains Filter.EXCLUDE) {
          Filter.EXCLUDE
        } else {
          operands.filter(Filter.INCLUDE !=) match {
            case Nil => Filter.INCLUDE
            case List(f) => f
            case l => filters.and(Arrays.asList(l.toArray:_*))
          }
        }
      )
    } else {
      None
    }
  }
}

case class OrSelector(children: List[Selector]) extends Selector {
  override def filterOpt = {
    if (children.forall(_.filterOpt.isDefined)) {
      val operands = children map { _.filterOpt.get }
      Some(
        if (operands.exists {_ == Filter.INCLUDE}) {
          Filter.INCLUDE
        } else {
          val parts = operands.partition(Filter.EXCLUDE==)
          parts._2 match {
            case Nil => {
              if (parts._1.isEmpty) Filter.INCLUDE
              else Filter.EXCLUDE
            }
            case List(f) => f
            case l => filters.or(Arrays.asList(l.toArray:_*))
          }
        }
      )
    } else {
      None
    }
  }
}
