package org.geoserver.community.css

import org.opengis.filter.{
  BinaryComparisonOperator,
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
trait SelectorOps extends org.geoserver.community.css.filter.FilterOps {
  object Exclude extends NotSelector(AcceptSelector) {
    def unapply(x: Selector): Option[Selector] = {
      if (x.isInstanceOf[DataSelector] &&
          x.asInstanceOf[DataSelector].asFilter == org.opengis.filter.Filter.EXCLUDE) {
            Some(x)
        } else None
    }
  }

  /**
   * Given two selectors, attempt to combine them into a single selector that
   * accepts all features that are accepted by BOTH of the selectors.  For
   * example,
   * <code>constrain([a&gt;2], [a&gt;4])</code> should return 
   * <code>Some([a&gt;4])</code>.  
   *
   * This method is probably incomplete and simply returns the first argument
   * when the input is not accounted for.
   */
  def constrain(x: Selector, y: Selector): Selector =
    constrainOption(x, y) getOrElse x

  def constrainOption(x: Selector, y: Selector): Option[Selector] =
    if (constraintRules isDefinedAt (x, y)) Some(constraintRules((x, y)))
    else None

  private val constraintRules
  : PartialFunction[(Selector, Selector), Selector] = {
    case (Exclude(_), _) => Exclude
    case (AcceptSelector, f) => f
    case (a, NotSelector(b)) if a == b => Exclude
    case (f@IdSelector(a), IdSelector(b)) => {
      if (a == b) f else Exclude
    }
    case (f@TypenameSelector(a), TypenameSelector(b)) => {
      if (a == b) f else Exclude
    }
    case (PseudoSelector(p, "<", a), PseudoSelector(q, "<", b)) 
      if p == q =>
      PseudoSelector(p, "<", (a.toDouble min b.toDouble) toString)
    case (PseudoSelector(p, ">", a), PseudoSelector(q, ">", b)) 
      if p == q =>
      PseudoSelector(p, ">", (a.toDouble max b.toDouble) toString)
    case (PseudoSelector(p, "<", a), PseudoSelector(q, ">", b)) 
      if p == q && a.toDouble <= b.toDouble => 
      Exclude
    case (PseudoSelector(p, ">", a), PseudoSelector(q, "<", b)) 
      if p == q && a.toDouble >= b.toDouble => 
      Exclude
    case (a: DataSelector, b: DataSelector)
      if constrainOption(a.asFilter, b.asFilter) != None
      => constrainOption(a.asFilter, b.asFilter).get match {
          case org.opengis.filter.Filter.EXCLUDE => Exclude
          case org.opengis.filter.Filter.INCLUDE => AcceptSelector
          case f => WrappedFilter(f)
        }
  }

  /**
   * Find the simplest filter possible that matches those features NOT accepted
   * by the filter passed in, if possible.
   */
  val negate: PartialFunction[Selector, Selector] = {
    case AcceptSelector => Exclude
    case Exclude => AcceptSelector
    case d: DataSelector =>
      WrappedFilter(negate(d.asFilter))
  }

  /**
   * Test two selectors to determine whether the set of features matched by the 
   * second is a subset of those matched by the first.  That is, returns true if
   * <code>y.accept(f)</code> implies <code>x.accept(f)</code> for all f.
   * 
   * This method is probably incomplete and defaults to false when the input has
   * not been accounted for. 
   */
  def redundant(x: Selector, y: Selector): Boolean = {
    (x, y) match {
      case (a, b) if a == b => true
      case (AcceptSelector, _) => true
      case (Exclude(_), _) => false
      case (a, NotSelector(b)) if a == b => false
      case (a: DataSelector, b: DataSelector) => 
        redundant(a.asFilter, b.asFilter)
      case _ => false
    }
  }

  /**
   * Reduce a list of Selector instances to a shorter (or at least, no longer) 
   * list of selectors that expresses the same constraint on data. (This assumes
   * that the Filters are being AND'ed together; ie, ALL filters in the list 
   * must be satisfied for a feature to be accepted.)
   */
  def simplify(xs: List[Selector]): List[Selector] = {
    def conflicting(x: Selector, y: Selector) = constrain(x, y) == Exclude

    def step(xs: List[Selector], simple: List[Selector]): List[Selector] = {
      if (xs isEmpty) {
        simple
      } else if (xs exists (x => simple.exists(conflicting(_, x)))) {
        List(Exclude)
      } else if (xs.tail isEmpty) {
        xs.head :: simple
      } else {
        val (constrained, unapplied) =
          (xs foldLeft (AcceptSelector: Selector, Nil: List[Selector])) {
            (accum, next) => 
            val (constrained, unapplied) = accum
            if (redundant(constrained, next)) {
              (next, unapplied)
            } else {
              constrainOption(next, constrained) match {
                case Some(result) => 
                  (result, unapplied)
                case None => 
                  (constrained, next :: unapplied)
              }
            }
          }

        if (unapplied.length < xs.length) {
          step(
            unapplied.filter(!redundant(constrained, _)),
            constrained :: simple
          )
        } else {
          step(
            unapplied.tail.filter(!redundant(constrained, _)),
            unapplied.head :: constrained :: simple
          )
        }
      }
    }

    step(xs, Nil) match {
      case Nil => List(Exclude)
      case xs if xs contains Exclude => List(Exclude)
      case xs => xs
    }
  }
}
