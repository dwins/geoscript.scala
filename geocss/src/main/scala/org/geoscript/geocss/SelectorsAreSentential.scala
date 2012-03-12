package org.geoscript.geocss

import dwins.logic.{ Knowledge, Sentential }
import dwins.cql.FiltersAreSentential

import org.opengis.{ filter => ogc }

object SelectorsAreSentential extends Sentential[Selector] {
  val False = Exclude
  val True = Accept

  private object DataFilter {
    def unapply(s: Selector): Option[ogc.Filter] = s.filterOpt
  }

  private val kb = dwins.logic.Knowledge.Oblivion(FiltersAreSentential)

  def implies(p: Selector, q: Selector): Boolean = {
    val res = 
    (p, q) match {
      case (Accept, Accept) => true
      case (DataFilter(ogc.Filter.INCLUDE), DataFilter(ogc.Filter.INCLUDE)) => true
      case (Accept, _) => false
      case (DataFilter(ogc.Filter.INCLUDE), _) => false
      case (Exclude, _) => false
      case (DataFilter(ogc.Filter.EXCLUDE), _) => false
      case (Not(p), q) => negate(q).exists(!implies(_, p))
      case (PseudoSelector("scale", ">", a), PseudoSelector("scale", ">", b)) => 
        b.toDouble <= a.toDouble
      case (PseudoSelector("scale", "<", a), PseudoSelector("scale", "<", b)) => 
        b.toDouble >= a.toDouble
      case (DataFilter(f), DataFilter(g)) =>
        try {
          kb.given(f).reduce(g) == ogc.Filter.INCLUDE
        } catch {
          case _ => 
            val tpl = "Tried to reduce with inconsistent givens: \n%s\n%s"
            sys.error(tpl format(f, g))
        }
      case _ => false
    } 
    // if (res)
    //   println("%s => %s ? %s" format(p,q, res))
    res
  }

  def allows(p: Selector, q: Selector): Boolean = {
    val res =
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
      case (Not(p), q) => !implies(q, p)
      case (PseudoSelector("scale", ">", a), PseudoSelector("scale", "<", b)) => 
        b.toDouble > a.toDouble
      case (PseudoSelector("scale", "<", a), PseudoSelector("scale", ">", b)) => 
        b.toDouble < a.toDouble
      case (DataFilter(f), DataFilter(g)) =>
        try {
          kb.given(f).reduce(g) != ogc.Filter.EXCLUDE
        } catch {
          case _ => 
            val tpl = "Tried to reduce with inconsistent givens: \n%s\n%s"
            sys.error(tpl format(f, g))
        }
      case _ => true
    } 
    // if (!res)
    //   println("%s && %s ? %s" format(p,q, res))
    res
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
    }

  def not(p: Selector) = Not(p)

  def extractNot(p: Selector): Option[Selector] =
    Option(p) collect { case Not(p) => p }
}
