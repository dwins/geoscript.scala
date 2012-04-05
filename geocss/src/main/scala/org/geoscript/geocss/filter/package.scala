package org.geoscript.geocss

import org.geoscript.support.interval._
import org.geoscript.support.logic.Sentential

import org.opengis.{ filter => ogc }

import collection.JavaConversions._

package object filter {
  implicit object FiltersAreSentential extends Sentential[ogc.Filter] {
    private val factory =
      org.geotools.factory.CommonFactoryFinder.getFilterFactory
    import factory._
    import Ops._

    val False = ogc.Filter.EXCLUDE
    val True = ogc.Filter.INCLUDE

    def or(p: ogc.Filter, q: ogc.Filter): ogc.Filter =
      (p, q) match {
        case (p: ogc.Or, q: ogc.Or) =>
          factory.or(p.getChildren ++ q.getChildren)
        case (p: ogc.Or, q) => factory.or(p.getChildren :+ q)
        case (p, q: ogc.Or) => factory.or(p +: (q.getChildren))
        case (p, q) => factory.or(p, q)
      }

    def extractOr(p: ogc.Filter): Option[(ogc.Filter, ogc.Filter)] =
      p match {
        case (p: ogc.Or) =>
          (p.getChildren: Seq[ogc.Filter]) match {
            case Seq() => Some((False, False))
            case Seq(q) => Some((q, False))
            case Seq(p, q) => Some((p, q))
            case Seq(h, t @ _*) => Some((h, factory.or(t)))
          }
        case _ => None
      }

    def and(p: ogc.Filter, q: ogc.Filter): ogc.Filter =
      (p, q) match {
        case (p: ogc.And, q: ogc.And) =>
          factory.and(p.getChildren.toList ++ q.getChildren)
        case (p: ogc.And, q) => factory.and(p.getChildren :+ q)
        case (p, q: ogc.And) => factory.and(p +: (q.getChildren))
        case (p, q) => factory.and(p, q)
      }

    def extractAnd(p: ogc.Filter): Option[(ogc.Filter, ogc.Filter)] =
      p match {
        case (p: ogc.And) =>
          (p.getChildren: Seq[ogc.Filter]) match {
            case Seq() => Some((True, True))
            case Seq(q) => Some((q, True))
            case Seq(p, q) => Some((p, q))
            case Seq(h, t @ _*) => Some((h, factory.and(t)))
          }
        case _ => None
      }

    def not(p: ogc.Filter): ogc.Filter = factory.not(p)

    def extractNot(p: ogc.Filter): Option[ogc.Filter] =
      p match {
        case (p: ogc.Not) => Some(p.getFilter)
        case _ => None
      }

    def isLiteral(p: ogc.Filter): Boolean =
      p match {
        case (_: ogc.And) | (_: ogc.Or) => false
        case (p: ogc.Not) => isLiteral(p.getFilter)
        case _ => true
      }

    def provenBy(givens: Set[ogc.Filter], p: ogc.Filter): Boolean =
      givens.contains(p) ||
      givens.exists(q => implies(q, p))

    def disprovenBy(givens: Set[ogc.Filter], p: ogc.Filter): Boolean =
      p match {
        case p if constraint(p) != Unconstrained => 
          givens.exists(q => !compatible(p, q))
        case Not(p) =>
          provenBy(givens, p)
        case p => false
      }

    def compatible(p: ogc.Filter, q: ogc.Filter): Boolean =
      q match {
        case q if constraint(q) != Unconstrained =>
          constraint(p) compatibleWith constraint(q)
        case Not(q) => !(constraint(p) implies constraint(q))
        case q => true
      }

    def implies(p: ogc.Filter, q: ogc.Filter): Boolean =
      q match {
        case Not(q) => !(constraint(p) compatibleWith constraint(q))
        case q => constraint(p) implies constraint(q)
      }

    sealed trait Constraint {
      def compatibleWith(that: Constraint): Boolean = 
        (this, that) match {
          case (Unconstrained, _) | (_, Unconstrained) => true
          case (IsNull(x), In(y, _)) => x != y
          case (In(x, _), IsNull(y)) => x != y
          case (In(x, xrange), In(y, yrange)) if x == y =>
            Interval.intersection(xrange, yrange) != Interval.Empty
          case (In(x, xrange), IsNot(y, yvalue)) if x == y =>
            !(xrange == Interval.degenerate(yvalue))
          case (IsNot(x, xvalue), In(y, yrange)) if x == y =>
            !(yrange contains xvalue)
          case (IsNot(x, _), IsNull(y)) => x != y
          case (IsNull(x), IsNot(y, _)) => x != y
          case _ => true
        }

      def implies(that: Constraint): Boolean =
        (this, that) match {
          case (Unconstrained, _) => false
          case _ if this == that => true
          case (IsNull(x), IsNot(y, yvalue)) => y == x
          case (In(x, xrange), In(y, yrange)) =>
            x == y &&
            Interval.intersection(xrange, yrange) == xrange
          case (In(x, xrange), IsNot(y, yvalue)) =>
            x == y &&
            Interval.intersection(xrange, Interval.degenerate(yvalue)) != Interval.Empty
          case _ => false
        }
    }

    class Value(val text: String) {
      override def toString = text
      override def equals(that: Any) =
        that match {
          case (that: Value) => 
            try {
              this.text.toDouble == that.text.toDouble
            } catch {
              case (_: NumberFormatException) => this.text == that.text
            }
          case _ => false 
        }
    }

    object Value {
      def apply(text: String): Value = new Value(text)
      def apply(literal: ogc.expression.Literal): Value =
        apply(literal.getValue.toString)

      implicit val valuesAreOrdered: Ordering[Value] =
        Ordering.fromLessThan { (a: Value, b: Value) =>
          try {
            a.text.toDouble < b.text.toDouble
          } catch {
            case (_: NumberFormatException) => a.text < b.text
          }
        }
    }
    case object Unconstrained extends Constraint
    case class IsNot(variable: String, value: Value) extends Constraint
    case class In(variable: String, interval: Interval[Value]) extends Constraint
    case class IsNull(variable: String) extends Constraint

    def constraint(p: ogc.Filter): Constraint = {
      import ogc.{
        Not,
        PropertyIsBetween,
        PropertyIsEqualTo,
        PropertyIsGreaterThan,
        PropertyIsGreaterThanOrEqualTo,
        PropertyIsLessThan,
        PropertyIsLessThanOrEqualTo,
        PropertyIsNotEqualTo,
        PropertyIsNull
      }
      import ogc.expression.{ Literal, PropertyName }

      p match {
        case p: PropertyIsBetween =>
          (p.getExpression, p.getLowerBoundary, p.getUpperBoundary) match {
            case (x: PropertyName, lower: Literal, upper: Literal) =>
              In(x.getPropertyName, Closed(Value(lower)) to Closed(Value(upper)))
            case _ =>
              Unconstrained
          }
        case p: PropertyIsEqualTo =>
          val lhs = p.getExpression1
          val rhs = p.getExpression2
          (lhs, rhs) match {
            case (variable: PropertyName, value: Literal) =>
              In(variable.getPropertyName, Interval.degenerate(Value(value)))
            case (value: Literal, variable: PropertyName) =>
              In(variable.getPropertyName, Interval.degenerate(Value(value)))
            case _ =>
              Unconstrained
          }
        case p: PropertyIsGreaterThan =>
          val lhs = p.getExpression1
          val rhs = p.getExpression2
          (lhs, rhs) match {
            case (variable: PropertyName, value: Literal) =>
              In(variable.getPropertyName, Open(Value(value)).left)
            case (value: Literal, variable: PropertyName) =>
              In(variable.getPropertyName, Open(Value(value)).right)
            case _ => 
              Unconstrained
          }
        case p: PropertyIsGreaterThanOrEqualTo =>
          val lhs = p.getExpression1
          val rhs = p.getExpression2
          (lhs, rhs) match {
            case (variable: PropertyName, value: Literal) =>
              In(variable.getPropertyName, Closed(Value(value)).left)
            case (value: Literal, variable: PropertyName) =>
              In(variable.getPropertyName, Closed(Value(value)).right)
            case _ => 
              Unconstrained
          }
        case p: PropertyIsLessThan =>
          val lhs = p.getExpression1
          val rhs = p.getExpression2
          (lhs, rhs) match {
            case (variable: PropertyName, value: Literal) =>
              In(variable.getPropertyName, Open(Value(value)).right)
            case (value: Literal, variable: PropertyName) =>
              In(variable.getPropertyName, Open(Value(value)).left)
            case _ => 
              Unconstrained
          }
        case p: PropertyIsLessThanOrEqualTo =>
          val lhs = p.getExpression1
          val rhs = p.getExpression2
          (lhs, rhs) match {
            case (variable: PropertyName, value: Literal) =>
              In(variable.getPropertyName, Closed(Value(value)).right)
            case (value: Literal, variable: PropertyName) =>
              In(variable.getPropertyName, Closed(Value(value)).left)
            case _ => 
              Unconstrained
          }
        case (n: Not) if n.getFilter.isInstanceOf[PropertyIsEqualTo] =>
          val f = n.getFilter.asInstanceOf[PropertyIsEqualTo]
          val lhs = f.getExpression1
          val rhs = f.getExpression2
          (lhs, rhs) match {
            case (variable: PropertyName, value: Literal) =>
              IsNot(variable.getPropertyName, Value(value))
            case (value: Literal, variable: PropertyName) =>
              IsNot(variable.getPropertyName, Value(value))
            case _ =>
              Unconstrained
          }
        case (n: Not) if n.getFilter.isInstanceOf[PropertyIsNull] =>
          val f = n.getFilter.asInstanceOf[PropertyIsNull]
          val lhs = f.getExpression
          (lhs) match {
            case (variable: PropertyName) =>
              In(variable.getPropertyName, Interval.Full)
            case _ =>
              Unconstrained
          }
        case (n: Not) =>
          constraint(n.getFilter) match {
            case Unconstrained => Unconstrained
            case IsNull(x) => In(x, Interval.Full)
            case IsNot(x, v) => In(x, Interval.Degenerate(v))
            case In(x, Interval.NonEmpty(None, None)) => IsNull(x)
            case In(x, Interval.Degenerate(v)) => IsNot(x, v)
            case In(x, Interval.NonEmpty(Some(_), Some(_))) => Unconstrained
            case In(x, Interval.NonEmpty(min, max)) =>
              val flip: Cap[Value] => Cap[Value] = {
                case Open(v) => Closed(v)
                case Closed(v) => Open(v)
              }
              val toggle = (_: Option[Cap[Value]]) map flip
              In(x, Interval.NonEmpty(toggle(max), toggle(min)))
          }
        case p: PropertyIsNotEqualTo =>
          val lhs = p.getExpression1
          val rhs = p.getExpression2
          (lhs, rhs) match {
            case (variable: PropertyName, value: Literal) =>
              IsNot(variable.getPropertyName, Value(value))
            case (value: Literal, variable: PropertyName) =>
              IsNot(variable.getPropertyName, Value(value))
            case _ =>
              Unconstrained
          }
        case p: PropertyIsNull =>
          val f = p.getExpression
          f match {
            case (variable: PropertyName) =>
              IsNull(variable.getPropertyName)
            case _ => 
              Unconstrained
          }
        case _ => Unconstrained
      }
    }

    def constraintToFilter(c: Constraint): ogc.Filter = {
      import factory._
      val empty = Interval.Empty[Value]
      c match {
        case Unconstrained => sys.error("Unconstrained cannot be converted back to a filter")
        case IsNot(variable, value) => notEqual(property(variable), literal(value.text))
        case In(variable, `empty`) => ogc.Filter.EXCLUDE
        case In(variable, Interval.NonEmpty(min, max)) =>
          val lower = 
            min map {
              case Open(value) => greater(property(variable), literal(value.text))
              case Closed(value) => greaterOrEqual(property(variable), literal(value.text))
            }
          val upper =
            max map {
              case Open(value) => less(property(variable), literal(value.text))
              case Closed(value) => lessOrEqual(property(variable), literal(value.text))
            }
          (lower, upper) match {
            case (Some(p), Some(q)) => And(p, q)
            case (Some(p), None) => p
            case (None, Some(q)) => q
            case (None, None) => ogc.Filter.INCLUDE
          }
        case IsNull(variable) => isNull(property(variable))
      }
    }

    def consolidate(f: ogc.Filter): ogc.Filter = {
      val both: (ogc.Filter, ogc.Filter) => Option[ogc.Filter] = (p, q) => {
        val constraintOpt = 
          (constraint(p), constraint(q)) match {
            case (In(x, xrange), In(y, yrange)) if x == y =>
              Some(In(x, Interval.intersection(xrange, yrange)))
            case _ => None
          }
        constraintOpt map constraintToFilter
      }
      val either: (ogc.Filter, ogc.Filter) => Option[ogc.Filter] = (p, q) => {
        val constraintOpt: Option[Constraint] = 
          (constraint(p), constraint(q)) match {
            case _ => None
          }
        constraintOpt map constraintToFilter
      }

      def consolidateOn
        (f: (ogc.Filter, ogc.Filter) => Option[ogc.Filter])
        (ps: Seq[ogc.Filter])
        : Seq[ogc.Filter] =
          ps match {
            case Seq(h, ts @ _*) =>
              val (consolidatable, nonconsolidatable) =
                ts partition(f(h, _) isDefined)
              val consolidated = 
                (consolidatable foldLeft h) {
                  f(_, _).getOrElse(sys.error("Consolidatable not transitive after all!"))
                }
              consolidated +: consolidateOn(f)(nonconsolidatable)
            case _ => ps
          }


      f match {
        case (p: ogc.And) =>
          val child = consolidateOn(both)(p.getChildren)
          factory.and(child)
        case (p: ogc.Or) =>
          val child = consolidateOn(either)(p.getChildren)
          factory.or(child)
        case (p: ogc.Not) =>
          factory.not(consolidate(p.getFilter))
        case p => p
      }
    }
  }
}
