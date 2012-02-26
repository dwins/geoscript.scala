package org.geoscript.geocss
import dwins.logic.Sentential

object SelectorsAreSentential extends Sentential[Selector] {
  val False = NotSelector(AcceptSelector)
  val True = AcceptSelector

  def disprovenBy(givens: Set[Selector], s: Selector): Boolean =
    s match {
      case NotSelector(p) => givens contains p
      case p => givens contains NotSelector(p)
    }

  def provenBy(givens: Set[Selector], s: Selector): Boolean =
    givens contains s

  def isLiteral(p: Selector) = 
    p match {
      case (_: AndSelector | _: OrSelector) => false
      case _ => true
    }

  def or(p: Selector, q: Selector) = {
    def child(s: Selector) = s match {
      case OrSelector(children) => children
      case s => List(s)
    }

    OrSelector(child(p) ++ child(q))
  }

  def extractOr(p: Selector): Option[(Selector, Selector)] =
    Option(p) collect {
      case OrSelector(ps) => ps match {
        case Seq() => (False, False)
        case Seq(q) => (q, False)
        case Seq(p, q) => (p, q)
        case Seq(h, t @ _*) => (h, OrSelector(t))
      }
    }

  def and(p: Selector, q: Selector) = {
    def child(s: Selector) = s match {
      case AndSelector(children) => children
      case s => List(s)
    }

    AndSelector(child(p) ++ child(q))
  }

  def extractAnd(p: Selector): Option[(Selector, Selector)] =
    Option(p) collect {
      case AndSelector(ps) => ps match {
        case Seq() => (True, True)
        case Seq(q) => (q, True)
        case Seq(p, q) => (p, q)
        case Seq(h, t @ _*) => (h, AndSelector(t))
      }
    }

  def not(p: Selector) = NotSelector(p)

  def extractNot(p: Selector): Option[Selector] =
    Option(p) collect { case NotSelector(p) => p }
}
