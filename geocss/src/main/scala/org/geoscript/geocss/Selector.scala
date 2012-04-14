package org.geoscript.geocss

import collection.JavaConversions._

import org.opengis.{ filter => ogc }, ogc.Filter

/**
 * A Selector expresses some subset of all possible features.  It restricts the
 * conditions under which a Rule will be applied to a feature, including but
 * not limited to requirements regarding the feature's attributes.
 */
sealed abstract class Selector {
  /**
   * An Option containing the OGC Filter equivalent to this Selector, if it
   * exists.
   */
  def filterOpt: Option[Filter]
}

object Selector {
  import org.geoscript.support.logic.{ given, Sentential }
  private implicit val filt = filter.FiltersAreSentential

  implicit object SelectorsAreSentential extends Sentential[Selector] {
    val False = Exclude
    val True = Accept

    private object DataFilter {
      def unapply(s: Selector): Option[ogc.Filter] = s.filterOpt
    }

    def implies(p: Selector, q: Selector): Boolean =
      (p, q) match {
        case (Accept, Accept) => true
        case (DataFilter(ogc.Filter.INCLUDE), DataFilter(ogc.Filter.INCLUDE)) => true
        case (Accept, _) => false
        case (DataFilter(ogc.Filter.INCLUDE), _) => false
        case (Exclude, _) => false
        case (DataFilter(ogc.Filter.EXCLUDE), _) => false
        case (Not(p), Not(q)) => implies(q, p)
        case (p, Not(q)) => !allows(p, q)
        case (PseudoSelector("scale", ">", a), PseudoSelector("scale", ">", b)) => 
          b.toDouble <= a.toDouble
        case (PseudoSelector("scale", "<", a), PseudoSelector("scale", "<", b)) => 
          b.toDouble >= a.toDouble
        case (DataFilter(f), DataFilter(g)) =>
          try {
            given(f).reduce(g) == ogc.Filter.INCLUDE
          } catch {
            case _ => 
              val tpl = "Tried to reduce with inconsistent givens: \n%s\n%s"
              sys.error(tpl format(f, g))
          }
        case _ => false
      } 

    def allows(p: Selector, q: Selector): Boolean =
      (p, q) match {
        case (Accept, Accept) => true
        case (DataFilter(ogc.Filter.INCLUDE), DataFilter(ogc.Filter.INCLUDE)) => true
        case (Accept, _) => true
        case (DataFilter(ogc.Filter.INCLUDE), _) => true
        case (Exclude, _) => false
        case (DataFilter(ogc.Filter.EXCLUDE), _) => false
        case (_, Exclude) => false
        case (_, DataFilter(ogc.Filter.EXCLUDE)) => false
        //case (NotSelector(p), NotSelector(q)) => allows(p, q)
        case (Not(p), Not(q)) => allows(q, p)
        case (Not(p), q) => !implies(q, p)
        case (PseudoSelector("scale", ">", a), PseudoSelector("scale", "<", b)) => 
          b.toDouble > a.toDouble
        case (PseudoSelector("scale", "<", a), PseudoSelector("scale", ">", b)) => 
          b.toDouble < a.toDouble
        case (DataFilter(f), DataFilter(g)) =>
          try {
            given(f).reduce(g) != ogc.Filter.EXCLUDE
          } catch {
            case _ => 
              val tpl = "Tried to reduce with inconsistent givens: \n%s\n%s"
              sys.error(tpl format(f, g))
          }
        case _ => true
      }

    private def negate(p: Selector): Option[Selector] =
      Some(p) collect { case Not(sel) => sel }

    def disprovenBy(givens: Set[Selector], s: Selector): Boolean = {
      negate(s)
        .map(provenBy(givens, _))
        .getOrElse(givens exists (g => !allows(g, s)))
    }

    def provenBy(givens: Set[Selector], s: Selector): Boolean = {
      (givens contains s) || (givens exists (implies(_, s)))
    }

    def isLiteral(p: Selector) = 
      p match {
        case (_: And| _: Or) => false
        case _ => true
      }

    def or(p: Selector, q: Selector) = {
      def child(s: Selector) = s match {
        case Or(children) => children
        case s => List(s)
      }

      Or(child(p) ++ child(q))
    }

    def extractOr(p: Selector): Option[(Selector, Selector)] =
      Option(p) collect {
        case Or(ps) => ps match {
          case Seq() => (False, False)
          case Seq(q) => (q, False)
          case Seq(p, q) => (p, q)
          case Seq(h, t @ _*) => (h, Or(t))
        }
        case DataSelector(filt.Ops.Or(p, q)) =>
          (asSelector(p), asSelector(q))
      }

    def and(p: Selector, q: Selector) = {
      def child(s: Selector) = s match {
        case And(children) => children
        case s => List(s)
      }

      And(child(p) ++ child(q))
    }

    def extractAnd(p: Selector): Option[(Selector, Selector)] =
      Option(p) collect {
        case And(ps) => ps match {
          case Seq() => (True, True)
          case Seq(q) => (q, True)
          case Seq(p, q) => (p, q)
          case Seq(h, t @ _*) => (h, And(t))
        }
        case DataSelector(filt.Ops.And(p, q)) =>
          (asSelector(p), asSelector(q))
      }

    def not(p: Selector) = Not(p)

    def extractNot(p: Selector): Option[Selector] =
      Option(p) collect { 
        case Not(p) => p
        case DataSelector(filt.Ops.Not(p)) =>
          asSelector(p)
      }
  }


  private case class FilterAsSelector(filter: ogc.Filter)
  extends DataSelector {
    override def asFilter = filter
    override def toString = filter.toString
  }

  def asSelector(f: ogc.Filter): Selector = {
    import filter.FiltersAreSentential.Ops
    f match {
      case Ops.And(p, q) =>
        SelectorsAreSentential.and(asSelector(p), asSelector(q))
      case Ops.Or(p, q) =>
        SelectorsAreSentential.or(asSelector(p), asSelector(q))
      case Ops.Not(p) =>
        SelectorsAreSentential.not(asSelector(p))
      case f => FilterAsSelector(f)
    }
  }
}

/**
 * A Selector which filters on typical database attributes.
 */
abstract class DataSelector extends Selector {
  /**
   * The OGC Filter corresponding to this Selector
   */
  def asFilter: Filter
  override def filterOpt = Some(asFilter)
}

object DataSelector {
  def unapply(sel: DataSelector): Some[Filter] = Some(sel.asFilter)
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
case class Id(id: String) extends DataSelector {
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
case object Accept extends DataSelector {
  override def asFilter = org.opengis.filter.Filter.INCLUDE
  override def toString = "*"
}

case object Exclude extends DataSelector {
  override def asFilter = org.opengis.filter.Filter.EXCLUDE
  override def toString = "[!!]"
}

/**
 * A Selector which constrains based on typename.
 */
case class Typename(typename: String) extends MetaSelector {
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
 * A selector which wraps another and reverses its decisions (it accepts only
 * features that would not be accepted by the wrapped selector).
 */
case class Not(selector: Selector) extends Selector {
  override def filterOpt =
    selector match {
      case Not(sel) => sel.filterOpt
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
case class And(children: Seq[Selector]) extends Selector {
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
case class Or(children: Seq[Selector]) extends Selector {
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
