package org.geoserver.community.css

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

abstract class Selector

abstract class DataSelector extends Selector {
  protected val filters = 
    org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)
  def asFilter: org.opengis.filter.Filter
}

abstract class MetaSelector extends Selector

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

case class NotSelector(selector: DataSelector) extends DataSelector {
  override def asFilter = {
    selector match {
      case NotSelector(sel) => sel.asFilter
      case sel => 
        selector.asFilter match {
          case org.opengis.filter.Filter.EXCLUDE =>
            org.opengis.filter.Filter.INCLUDE
          case org.opengis.filter.Filter.INCLUDE =>
            org.opengis.filter.Filter.EXCLUDE
          case f =>
            filters.not(f)
        }
    }
  }

  override def toString = {
    selector match {
      case NotSelector(sel) => sel.toString
      case sel => 
        sel.asFilter match {
          case org.opengis.filter.Filter.INCLUDE => "!!"
          case org.opengis.filter.Filter.EXCLUDE => "*"
          case _ => "!" + sel.toString
        }
    }
  }
}

case class AndSelector(children: List[DataSelector]) extends DataSelector {
  override def asFilter = {
    val operands = children map {_.asFilter}
    if (operands.exists {_ == org.opengis.filter.Filter.EXCLUDE}) {
      org.opengis.filter.Filter.EXCLUDE
    } else {
      operands.filter(org.opengis.filter.Filter.INCLUDE !=) match {
        case Nil => org.opengis.filter.Filter.INCLUDE
        case List(f) => f
        case l => filters.and(java.util.Arrays.asList(l.toArray:_*))
      }
    }
  }

  override def toString = {
    val operands = children map { x => (x, x.asFilter) }

    if (operands.exists {_._2 == org.opengis.filter.Filter.EXCLUDE}) {
      "!!"
    } else {
      operands filter {
        _._2 != org.opengis.filter.Filter.INCLUDE
      } map {
        _._1
      } match {
        case Nil => "*"
        case List(f) => f.toString
        case l => "(" + l.mkString(" + ") + ")"
      }
    }
  }
}

case class OrSelector(children: List[DataSelector]) extends DataSelector {
  override def asFilter = {
    val operands = children map {_.asFilter}
    if (operands.exists {_ == org.opengis.filter.Filter.INCLUDE}) {
      org.opengis.filter.Filter.INCLUDE
    } else {
      val parts = operands.partition(org.opengis.filter.Filter.EXCLUDE==)
      parts._2 match {
        case Nil => {
          if (parts._1.isEmpty) org.opengis.filter.Filter.INCLUDE
          else org.opengis.filter.Filter.EXCLUDE
        }
        case List(f) => f
        case l => filters.or(java.util.Arrays.asList(l.toArray:_*))
      }
    }
  }

  override def toString = {
    val operands = children map { x => (x, x.asFilter) }

    if (operands.exists {_._2 == org.opengis.filter.Filter.INCLUDE}) {
      "*"
    } else {
      operands filter {
        _._2 != org.opengis.filter.Filter.INCLUDE
      } map {
        _._1
      } match {
        case Nil => "*"
        case List(f) => f.toString
        case l => "(" + l.mkString(" | ") + ")"
      }
    }
  }
}
