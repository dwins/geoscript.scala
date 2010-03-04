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
  def constrain(x: Selector, y: Selector): Selector = {
    (x, y) match {
      case (Exclude(_), _) => Exclude
      case (AcceptSelector, f) => f
      case (a, NotSelector(b)) if a == b => Exclude
      case (f@IdSelector(a), IdSelector(b)) => {
        if (a == b) f else Exclude
      }
      case (f@TypenameSelector(a), TypenameSelector(b)) => {
        if (a == b) f else Exclude
      }
      case (PseudoSelector("scale", op1, a), PseudoSelector("scale", op2, b)) => {
        (op1, op2) match {
          case (">", ">") => PseudoSelector("scale", ">", (a.toDouble max b.toDouble).toString)
          case ("<", "<") => PseudoSelector("scale", "<", (a.toDouble min b.toDouble).toString)
          case (">", "<") if a.toDouble >= b.toDouble => NotSelector(AcceptSelector)
          case ("<", ">") if a.toDouble <= b.toDouble => NotSelector(AcceptSelector)
          case _ => x
        }
      }
      case (a: DataSelector, b: DataSelector) => {
        val aFilter = a.asFilter
        constrain(aFilter, b.asFilter) match {
          case org.opengis.filter.Filter.EXCLUDE => Exclude
          case org.opengis.filter.Filter.INCLUDE => AcceptSelector
          case f if f == aFilter => a
          case f => WrappedFilter(f)
        }
      }
      case _ => x
    }
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
      case (Exclude(_), _) => true
      case (a, NotSelector(b)) if a == b => false
      case (AcceptSelector, _) => true
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
    if (xs isEmpty) Nil
    else if (xs.tail isEmpty) xs.head :: Nil
    else {
      val first = xs.foldLeft (AcceptSelector: Selector) (constrain)
      first :: simplify(xs.tail.filter(!redundant(first, _)))
    }
  }
}
