package org.geoserver.community.css

import org.geoserver.community.css.filter.FilterOps

import org.opengis.filter.{
  BinaryComparisonOperator,
  Filter,
  Not,
  PropertyIsEqualTo
}

import org.opengis.filter.expression.{
  PropertyName
}

/**
 * The SelectorOps trait provides some facilities for manipulating CSS Selector 
 * objects.
 *
 * @see org.geoserver.community.css.filter.FilterOps
 * @author David Winslow <cdwinslow@gmail.com>
 */
object SelectorOps extends Simplifier[Selector] {
  // aliases for use during port
  val negate = not _
  def simplify(sels: List[Selector]): List[Selector] = 
    simplify(AndSelector(sels)) match {
      case AndSelector(children) => children
      case singleton => List(singleton)
    }

  val constrainOption = intersection _
  def constrain(a: Selector, b: Selector): Selector =
    intersection(a, b) getOrElse a
  def redundant(p: Selector, q: Selector): Boolean =
    isSubSet(q, p)

  val Exclude = 
    new DataSelector {
      def asFilter = Filter.EXCLUDE

      def unapply(x: Selector): Option[Selector] = 
        Some(x).filter(_.filterOpt == Some(Filter.EXCLUDE))

      override def toString = "[Exclude]"
    }

  // okay, on with the show
  val Everything = AcceptSelector
  val Empty = Exclude

  override def simpleCovering(p: Selector, q: Selector): Boolean = 
    (p, q) match {
      case (p, q) if p.filterOpt.isDefined && q.filterOpt.isDefined 
        => FilterOps.areCovering(p.filterOpt.get, q.filterOpt.get)
      case _ => false
    }

  override def simpleDisjoint(p: Selector, q: Selector): Boolean = 
    (p, q) match {
      case (PseudoSelector(lh1, op1, rh1), PseudoSelector(lh2, op2, rh2))
        if lh1 == lh2
        =>
           (op1, op2) match {
             case (">", "<") => rh1.toDouble >= rh2.toDouble
             case ("<", ">") => rh1.toDouble <= rh2.toDouble
             case _ => false
           }
      case (p, q) if p.filterOpt.isDefined && q.filterOpt.isDefined 
        => FilterOps.isDisjoint(p.filterOpt.get, q.filterOpt.get)
      case _ => false
    }

  override def simpleSubSet(set: Selector, candidate: Selector): Boolean = 
    (set, candidate) match {
      case (PseudoSelector(lh1, op1, rh1), PseudoSelector(lh2, op2, rh2))
        if (lh1 == lh2)
        => (op1, op2) match {
             case ("<", "<") => rh1.toDouble >= rh2.toDouble
             case (">", ">") => rh1.toDouble <= rh2.toDouble
             case _ => false
           }
      case (p, q) if p.filterOpt.isDefined && q.filterOpt.isDefined
        => FilterOps.isSubSet(p.filterOpt.get, q.filterOpt.get)
      case _ => false
    }

  override def complementExtract(p: Selector): Option[Selector] =
    p match {
      case NotSelector(p) => Some(p)
      case _ => None
    }

  override def simpleComplement(p: Selector): Option[Selector] = 
    p match {
      case PseudoSelector(lh, ">", rh) => Some(PseudoSelector(lh, "<", rh))
      case PseudoSelector(lh, "<", rh) => Some(PseudoSelector(lh, ">", rh))
      case p =>
        for (filt <- p.filterOpt) yield WrappedFilter(FilterOps.not(filt))
    }

  def invert(p: Selector): Selector = NotSelector(p)

  def intersectionExtract(p: Selector): Option[Seq[Selector]] = 
    p match {
      case AndSelector(children) => Some(children)
      case _ => None
    }

  def allOf(ps: Seq[Selector]): Selector = AndSelector(ps.toList)

  def unionExtract(p: Selector): Option[Seq[Selector]] =
    p match {
      case OrSelector(children) => Some(children)
      case _ => None
    }
  
  def anyOf(ps: Seq[Selector]): Selector = OrSelector(ps.toList)
}
