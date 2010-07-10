package org.geoserver.community.css.filter

import org.geoserver.community.css.Simplifier

import java.util.Arrays
import scala.collection.jcl.Conversions._

import org.opengis.filter.{
  And,
  BinaryComparisonOperator,
  Filter,
  Not,
  Or,
  PropertyIsEqualTo,
  PropertyIsGreaterThanOrEqualTo,
  PropertyIsGreaterThan,
  PropertyIsLessThanOrEqualTo,
  PropertyIsLessThan,
  PropertyIsLike,
  PropertyIsNotEqualTo,
  PropertyIsNull
}

import org.opengis.filter.expression.{Expression, Literal, PropertyName}


/**
 * The FilterOps trait provides some facilities for manipulating GeoAPI 
 * Filter objects, such as simplification.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
object FilterOps extends Simplifier[Filter] {
  val filters = org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)

  val Empty = Filter.EXCLUDE
  val Everything = Filter.INCLUDE

  // aliases to use during port
  def redundant(a: Filter, b: Filter) = isSubSet(b, a)
  val negate = not _
  def constrainOption(a: Filter, b: Filter) = intersection(a, b)
  def constrain(a: Filter, b: Filter) = intersection(a, b) getOrElse a 
  def relax(a: Filter, b: Filter): Filter = union(a, b) getOrElse a

  def invert(pred: Filter): Filter = 
    filters.not(pred)

  def complementExtract(pred: Filter) = 
    pred match {
      case f: Not => Some(f.getFilter)
      case _ => None
    }

  def allOf(preds: Seq[Filter]): Filter =
    filters.and(Arrays.asList(preds.toArray: _*))

  def intersectionExtract(f: Filter): Option[Seq[Filter]] = 
    f match {
      case a: And => Some(a.getChildren().toSeq.asInstanceOf[Seq[Filter]])
      case _ => None
    }

  def anyOf(preds: Seq[Filter]): Filter = 
    filters.or(Arrays.asList(preds.toArray: _*))

  def unionExtract(f: Filter): Option[Seq[Filter]] = 
    f match {
      case o: Or => Some(o.getChildren().toSeq.asInstanceOf[Seq[Filter]])
      case _ => None
    }

  def inverse(op: Symbol): (Expression, Expression) => Filter = 
    op match {
      case '=  => filters.notEqual _
      case '<> => filters.equal _
      case '<  => filters.greaterOrEqual _
      case '>  => filters.lessOrEqual _
      case '<= => filters.greater _
      case '>= => filters.less _
    }

  override def simpleComplement(pred: Filter): Option[Filter] =
    complementAggregates(pred) orElse {
      pred match {
        case BinOp(a, op, b) => 
          Some(filters.or(inverse(op)(a, b), filters.isNull(a)))
        case _ => None
      }
    }

  override def simpleDisjoint(a: Filter, b: Filter): Boolean = 
    (a, b) match {
      case (BinOp(a, _, _), b: PropertyIsNull)
        if equivalent(a, b.getExpression()) 
        => true
      case (a: PropertyIsNull, BinOp(b, _, _))
        if equivalent(a.getExpression(), b) 
        => true
      case (BinOp(lh1, op1, Lit(rh1)), BinOp(lh2, op2, Lit(rh2))) 
        if equivalent(lh1, lh2)
        =>
          val cmp = rh1 compareTo rh2
          def rules(cmp: Int): PartialFunction[(Symbol, Symbol), Boolean] = {
            case ('=, '=)    => cmp != 0
            case ('=, '<>)   => cmp == 0
            case ('=, '<)    => cmp >= 0
            case ('=, '<=)   => cmp >  0
            case ('=, '>)    => cmp <= 0
            case ('=, '>=)   => cmp <  0
            case ('<, '>)    => cmp <= 0
            case ('<, '>=)   => cmp <= 0
            case ('<=, '>)   => cmp <= 0
            case ('<=, '>=)  => cmp <  0
          }

          if (rules(cmp).isDefinedAt((op1, op2))) {
            rules(cmp)((op1, op2)) 
          } else if (rules(-cmp).isDefinedAt((op2, op1))) {
            rules(-cmp)((op2, op1)) 
          } else {
            false
          }
      case _ => false
    }

  override def simpleSubSet(set: Filter, candidate: Filter): Boolean =
    (set, candidate) match {
      case (a, b) if a == b => true
      case (a: PropertyIsNull, b: PropertyIsNull) => 
        equivalent(a.getExpression(), b.getExpression())
      case (a: PropertyIsNull, BinOp(b, _, _)) 
        if equivalent(a.getExpression(), b)
        => false
      case (BinOp(lh1, op1, Lit(rh1)), BinOp(lh2, op2, Lit(rh2))) 
        if equivalent(lh1, lh2) 
        => 
        val cmp = rh1.compareTo(rh2)

        (op1, op2) match {
          case ('=,  '= ) => cmp == 0
          case ('<>, '= ) => cmp != 0
          case ('<>, '<>) => cmp == 0
          case ('<>, '< ) => cmp >= 0
          case ('<>, '<=) => cmp >  0
          case ('<>, '> ) => cmp <= 0
          case ('<>, '>=) => cmp <  0
          case ('<,  '= ) => cmp >  0
          case ('<,  '< ) => cmp >= 0
          case ('<,  '<=) => cmp >  0
          case ('<=, '= ) => cmp >= 0
          case ('<=, '< ) => cmp >= 0
          case ('<=, '<=) => cmp >= 0
          case ('>,  '= ) => cmp <  0
          case ('>,  '> ) => cmp <= 0
          case ('>,  '>=) => cmp <  0
          case ('>=, '= ) => cmp <= 0
          case ('>=, '> ) => cmp <  0
          case ('>=, '>=) => cmp <= 0
          case _ => false
        }
      case _ => false
    }

  override def simpleCovering(a: Filter, b: Filter) = 
    (a, b) match {
      case (Not(a), b) if equivalent(a, b) => true
      case (a, Not(b)) if equivalent(a, b) => true
      case (BinOp(lh1, op1, Lit(rh1)), BinOp(lh2, op2, Lit(rh2))) 
        if equivalent(lh1, lh2)
        =>
        val cmp = rh1.compareTo(rh2)

        val rules: PartialFunction[(Symbol, Symbol), Boolean] = {
          case ('=,  '<>) => cmp == 0
          case ('<>, '<>) => cmp != 0
          case ('<>, '< ) => cmp <  0
          case ('<>, '<=) => cmp <= 0
          case ('<>, '> ) => cmp >  0
          case ('<>, '>=) => cmp >= 0
          case ('<,  '> ) => cmp >  0
          case ('<,  '>=) => cmp >= 0
          case ('<=, '>)  => cmp >= 0
          case ('<=, '>=) => cmp >= 0
        }

        if (rules.isDefinedAt((op1, op2))) {
          rules((op1, op2)) 
        } else if (rules.isDefinedAt((op2, op1))) {
          rules((op2, op1)) 
        } else {
          false
        }
      case _ => false
    }

  override def simpleUnion(a: Filter, b: Filter): Option[Filter] =
    (a, b) match {
      case (BinOp(lh1, op1, rh@Lit(rh1)), BinOp(lh2, op2, Lit(rh2)))
        if equivalent(lh1, lh2)
        =>
          val cmp = rh1 compareTo rh2
          (op1, op2) match {
            case ('>, '=) if cmp == 0 => Some(filters.greaterOrEqual(lh1, rh))
            case ('>, '<) if cmp == 0 => Some(filters.notEqual(lh1, rh))
            case ('<, '>) if cmp == 0 => Some(filters.notEqual(lh1, rh))
            case ('<, '>) if cmp == 0 => Some(filters.notEqual(lh1, rh))
            case _ => None
          }
      case _ => None
    }

  override def simpleSimplify(f: Filter) = 
    f match {
      case Not(BinOp(lh, op, rh)) => inverse(op)(lh, rh)
      case f => f
    }

  def equivalent(a: Expression, b: Expression): Boolean =
    (a, b) match {
      case (a: Literal, b: Literal) =>
        a.getValue() == b.getValue()
      case (a: PropertyName, b: PropertyName) =>
        a.getPropertyName() == b.getPropertyName()
      case _ => false
    }


  object BinOp {
    def unapply(x: Filter): Option[(Expression, Symbol, Expression)] =
      x match {
        case x: BinaryComparisonOperator =>
          val op = x match {
            case _: PropertyIsEqualTo => '=
            case _: PropertyIsGreaterThan => '>
            case _: PropertyIsGreaterThanOrEqualTo => '>=
            case _: PropertyIsLessThan => '<
            case _: PropertyIsLessThanOrEqualTo => '<=
            case _: PropertyIsNotEqualTo => '<>
            case _ => null
          }

          Some((x.getExpression1(), op, x.getExpression2())) filter { _._2 != null }
        case Not(x: PropertyIsEqualTo) =>
          Some((x.getExpression1(), '<>, x.getExpression2()))
        case _ => None
      }
  }

  object Not {
    def unapply(f: Filter): Option[Filter] = f match {
      case f: Not => Some(f.getFilter)
      case _ => None
    }
  }

  object Lit {
    def unapply(x: Expression): Option[Comparable[_]] = 
      x match {
      case lit: Literal if lit.getValue().isInstanceOf[Comparable[_]] => 
        Some(lit.getValue().asInstanceOf[Comparable[_]])
      case _ => None
    }
  }
}
