package org.geoserver.community.css.filter

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
trait FilterOps {
  val filters = org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)

  private implicit def comparisonOps(me: Comparable[AnyRef]) = new Ordered[AnyRef] {
    def compare(you: AnyRef) = me.compareTo(you)
  }

  private def conflicting(a: Filter, b: Filter) =
    constrain(a, b) == Filter.EXCLUDE

  private def operatorConstraintRules[A](rA: Comparable[A], rB: Comparable[A])
  : PartialFunction[(Symbol, Symbol), Filter => Filter] = {
    val cmp = rA.compareTo(rB.asInstanceOf[A])
    return {
      case ('=,            '=)       if cmp != 0 => { _ => Filter.EXCLUDE }
      case ('=,            '<>)      if cmp == 0 => { _ => Filter.EXCLUDE }
      case ('=,            '<>)      if cmp != 0 => identity
      case ('=,            '=)       if cmp == 0 => identity
      case ('= | '< | '<=, '>)       if cmp <= 0 => { _ => Filter.EXCLUDE }
      case ('= | '> | '>=, '<)       if cmp >= 0 => { _ => Filter.EXCLUDE }
      case ('<=,           '<=)      if cmp <= 0 => identity
      case ('<,            '< | '<=) if cmp <= 0 => identity
      case ('<=,           '< | '<=) if cmp <  0 => identity
      case ('>=,           '>=)      if cmp >= 0 => identity
      case ('>,            '> | '>=) if cmp >= 0 => identity
      case ('>=,           '> | '>=) if cmp >  0 => identity
      case ('<> | '< | '>, '<>)      if cmp == 0 => identity
    }
  }

  private val constraintRules: PartialFunction[(Filter, Filter), Filter] = {
    case (Filter.INCLUDE, f) => f
    case (Filter.EXCLUDE, _) => Filter.EXCLUDE
    case (BinOp('nullcheck, la, _), BinOp(opB, lb, _)) 
      if opB != 'nullcheck && equivalent(la, lb)
      => Filter.EXCLUDE
    case (a@BinOp(opA, lA, Lit(rA)), BinOp(opB, lB, Lit(rB)))
      if (equivalent(lA, lB)) &&
         (operatorConstraintRules(rA, rB) isDefinedAt (opA, opB))
      => operatorConstraintRules(rA, rB)((opA, opB)) (a)
    case (a: Not, b) if equivalent(a.getFilter, b) =>
      Filter.EXCLUDE
    case (And(children), b) =>
      if (children.exists(conflicting(_, b))) {
        Filter.EXCLUDE
      } else {
        And(children ++ Seq(b) filter { Filter.INCLUDE != })
      }
    case (Or(children), b) =>
      if (children.exists(equivalent(b, _))) {
        b
      } else {
        val constrained = children map { 
          child => constrainOption(b, child) getOrElse And(Seq(b, child))
        }

        if (constrained.forall(_.isInstanceOf[And])) {
          null // workaround for compiler bug; see constrainOption method
        } else {
          Or(constrained filter { Filter.EXCLUDE != })
        }
      }
    case (a, b) if redundant(a, b) => b
  }

  private val redundancyRules: PartialFunction[(Filter, Filter), Boolean] = {
    case (a, b) if equivalent(a, b) => true
    case (Filter.EXCLUDE, _) => false
    case (Filter.INCLUDE, _) => true
    case (_, Filter.EXCLUDE) => true
    case (And(children), b) if children.forall(redundant(_, b)) => true
    case (Or(children), b) if children.exists(redundant(_, b)) => true
    case (Not(BinOp('nullcheck, la, _)), BinOp(opB, lb, _)) 
      if opB != 'nullcheck && equivalent(la, lb)
      => true
    case (BinOp(opA, lA, Lit(rA)), BinOp(opB, lB, Lit(rB)))
      if (equivalent(lA, lB)) 
      => (opA, opB) match {
        case ('=,        '=)  => rA == rB
        case ('=,        '>=) => rA >= rB
        case ('<,        '<)  => rA >= rB
        case ('<>,       '<>) => rA == rB
        case ('<>,       '<)  => rA >= rB
        case ('<>,       '>)  => rA <= rB
        case ('<>,       '<=) => rA >  rB
        case ('<>,       '>=) => rA <  rB
        case ('<= | '<,  '<)  => rA >  rB
        case ('<  | '<=, '<=) => rA >= rB
        case ('>= | '>,  '>)  => rA <= rB
        case ('>= | '>,  '>=) => rA <  rB
        case _ => false
      }
    case _ => false
  }

  def constrainOption(a: Filter, b: Filter): Option[Filter] = {
    val sa = simplify(a)
    val sb = simplify(b)
    (
      if (constraintRules isDefinedAt (sa, sb)) {
         Some(constraintRules((sa, sb)))
      } else if (constraintRules isDefinedAt (sb, sa)) {
      Some(constraintRules((sb, sa)))
      } else {
        None
      }
    ) filter ( null != ) 
    // Workaround for pattern matching bug in compiler; 
    // Calling a function in the guard for the (Or(children), b) causes crash
    // TODO: Revisit after upgrade to 2.8
  }

  def constrain(a: Filter, b: Filter): Filter = 
    constrainOption(a, b) getOrElse a

  def relaxOption(a: Filter, b: Filter): Option[Filter] = {
    val sa = simplify(a)
    val sb = simplify(b)
    if (relaxRules isDefinedAt (sa, sb)) {
      Some(relaxRules((sa, sb)))
    } else if (relaxRules isDefinedAt (sb, sa)) {
      Some(relaxRules((sb, sa)))
    } else {
      None
    }
  }

  val relaxRules: PartialFunction[(Filter, Filter), Filter] = {
    case (Filter.INCLUDE, f) => Filter.INCLUDE
    case (Filter.EXCLUDE, f) => f
    case (Not(child), f) if equivalent(child, f) => Filter.INCLUDE
    case (a@BinOp(opA, lA, Lit(rA)), BinOp(opB, lB, Lit(rB)))
      if (equivalent(lA, lB)) &&
         (operatorRelaxRules(rA, rB) isDefinedAt (opA, opB))
      => operatorRelaxRules(rA, rB)((opA, opB)) (a)
    case (And(children), b) =>
      if (children forall (redundant(b, _))) {
        b
      } else {
        And(
          children map { 
            child => relaxOption(child, b) getOrElse Or(Seq(child, b))
          } filter {
            Filter.INCLUDE != 
          }
        )
      }
    case (Or(children), b) => Or((Seq(b) ++ children) filter (Filter.EXCLUDE !=))
    case (a, b) if redundant(a, b) => b
  }

  private def operatorRelaxRules[A](rA: Comparable[A], rB: Comparable[A])
  : PartialFunction[(Symbol, Symbol), Filter => Filter] = {
    val cmp = rA.compareTo(rB.asInstanceOf[A])
    return {
      case ('=,       '<>)           if cmp == 0 => { _ => Filter.INCLUDE }
      case ('=,       '=)            if cmp == 0 => identity
      case ('=,       '< | '<=)      if cmp == 0 => { case BinOp(_, lhs, rhs) => filters.lessOrEqual(lhs, rhs) }
      case ('=,       '> | '>=)      if cmp == 0 => { case BinOp(_, lhs, rhs) => filters.greaterOrEqual(lhs, rhs) }
      case ('< | '<=, '>)            if cmp >= 0 => { _ => Filter.INCLUDE }
      case ('> | '>=, '<)            if cmp <= 0 => { _ => Filter.INCLUDE }
      case ('<=,      '<=)           if cmp <= 0 => identity
      case ('< | '<=, '<)            if cmp >= 0 => identity
      case ('<=,      '< | '<=)      if cmp >  0 => identity
      case ('>=,      '>=)           if cmp <= 0 => identity
      case ('>,       '> | '>=)      if cmp <= 0 => identity
      case ('>=,      '> | '>=)      if cmp <  0 => identity
      case ('<>,      '<> | '< | '>) if cmp == 0 => identity
    }
  }

  def relax(a: Filter, b: Filter): Filter =
    relaxOption(a, b).getOrElse(a)

  /**
   * Test two filters to determine whether the set of features matched by the 
   * second is a subset of those matched by the first.  That is, returns true if
   * <code>y.accept(f)</code> implies <code>x.accept(f)</code> for all f.
   * 
   * This method is probably incomplete and defaults to false when the input has
   * not been accounted for. 
   */
  def redundant(a: Filter, b: Filter): Boolean = {
    redundancyRules((simplify(a), simplify(b)))
  }

  def negate(x: Filter): Filter = x match {
    case BinOp(op, lhs, rhs) =>
      import filters._
      op match {
        case '=  => or(notEqual(lhs, rhs), isNull(lhs))
        case '<> => or(filters.equals(lhs, rhs), isNull(lhs))
        case '<  => or(greaterOrEqual(lhs, rhs), isNull(lhs))
        case '<= => or(greater(lhs, rhs), isNull(lhs))
        case '>  => or(lessOrEqual(lhs, rhs), isNull(lhs))
        case '>= => or(less(lhs, rhs), isNull(lhs))
        case 'nullcheck => not(x)
      }
    case Not(child) => child
    case And(children) => 
      simplify(Or(children.map(negate) filter (Filter.EXCLUDE != )))
    case Or(children) =>  
      val negated = children map negate
      simplify(And(negated filter (Filter.INCLUDE != )))
    case _ => filters.not(x)
  }

  def equivalent(x: Filter, y: Filter): Boolean = (x, y) match {
    case (Not(a), Not(b)) => equivalent(a, b)
    case (BinOp('<>, al, ar), Not(BinOp('=, bl, br))) =>
      equivalent(al, bl) && equivalent(ar, br)
    case (Not(BinOp('=, al, ar)), BinOp('<>, bl, br)) =>
      equivalent(al, bl) && equivalent(ar, br)
    case (a: PropertyIsNull, b: PropertyIsNull) => 
      equivalent(a.getExpression(), b.getExpression())
    case (BinOp(a, b, c), BinOp(x, y, z)) =>
      a == x && equivalent(b, y) && equivalent(c, z)
    case (a: PropertyIsLike, b: PropertyIsLike) =>
      equivalent(a.getExpression, b.getExpression) &&
      a.getLiteral == b.getLiteral &&
      a.getEscape == b.getEscape &&
      a.getSingleChar == b.getSingleChar &&
      a.getWildCard == b.getWildCard &&
      a.isMatchingCase == b.isMatchingCase
    case (Filter.EXCLUDE, Filter.EXCLUDE) => true
    case (Filter.INCLUDE, Filter.INCLUDE) => true
    case (a@And(_), b@And(_)) =>
      val as = simplify(a)
      val bs = simplify(b)
      (as, bs) match {
        case (And(aChildren), And(bChildren)) => 
          aChildren.length == bChildren.length && 
          aChildren.forall { a => bChildren.exists(b => equivalent(a, b)) }
        case (a, b) => equivalent(a, b)
      }
    case (a@Or(as), b@Or(bs)) =>
      val as = simplify(a)
      val bs = simplify(b)
      (as, bs) match {
        case (Or(aChildren), Or(bChildren)) => 
          aChildren.length == bChildren.length && 
          aChildren.forall { a => bChildren.exists(b => equivalent(a, b)) }
        case (a, b) => equivalent(a, b)
      }
    case _ => false
  }

  def equivalent(a: Expression, b: Expression): Boolean = (a, b) match {
    case (Lit(a), Lit(b)) if a == b => true
    case (Prop(a), Prop(b)) if a == b => true
    case _ => false
  }

  def simplify(f: Filter): Filter = f match {
    case Not(Not(f)) => f
    case Not(BinOp(op, lhs, rhs)) => 
      import filters._
      op match {
        case '= => notEqual(lhs, rhs)
        case '<> => filters.equals(lhs, rhs)
        case '< => greaterOrEqual(lhs, rhs)
        case '> => lessOrEqual(lhs, rhs)
        case '<= => greater(lhs, rhs)
        case '>= => less(lhs, rhs)
        case _ => f
      }
    case And(children) =>
      val flattened = children.map(simplify).flatMap {
        case And(nested) => nested map simplify
        case f => Seq.singleton(f)
      } filter (Filter.INCLUDE !=)

      def reduce(accum: Seq[Filter], queue: Seq[Filter]): Seq[Filter] = {
        if (queue isEmpty) {
          accum
        } else if (accum.exists(f => queue.exists(conflicting(f, _)))) {
          List(Filter.EXCLUDE)
        } else if (queue.length == 1) {
          accum ++ queue
        } else {
          var filter: Filter = queue.first
          var shelf = new collection.mutable.ListBuffer[Filter]()
          for (f <- queue.drop(1)) {
            val constrained = constrainOption(f, filter)
            if (constrained isDefined) {
              filter = constrained.get
            } else {
              shelf += f
            }
          }
          reduce(accum ++ Seq(filter), shelf)
        }
      }
      
      And(reduce(Seq.empty, flattened).filter(Filter.INCLUDE !=))
    case Or(children) =>
      val flattened = children.map(simplify).flatMap {
        case Or(nested) => nested.map(simplify)
        case f => Seq.singleton(f)
      }.filter(Filter.EXCLUDE !=).toList.removeDuplicates

      def reduce(accum: Seq[Filter], queue: Seq[Filter]): Seq[Filter] = {
        if (queue isEmpty) {
          accum
        } else if (accum.exists(f => queue.exists(redundant(f, _)))) {
          accum 
        } else if (queue.size == 1) {
          accum ++ queue
        } else {
          var filter: Filter = queue.first
          var shelf = new collection.mutable.ListBuffer[Filter]()
          for (f <- queue.drop(1)) {
            val relaxed = relaxOption(f, filter)
            if (relaxed isDefined) {
              filter = relaxed.get
            } else {
              shelf += f
            }
          }
          reduce(accum ++ Seq(filter), shelf)
        }
      }

      Or(reduce(Seq.empty, flattened).filter(Filter.EXCLUDE != ))
    case f => f
  }

  object BinOp {
    def unapply(f: Filter): Option[(Symbol, Expression, Expression)] =
      f match {
        case binop: BinaryComparisonOperator => 
          val symbol = binop match {
            case _: PropertyIsEqualTo => '=
            case _: PropertyIsGreaterThan => '>
            case _: PropertyIsGreaterThanOrEqualTo => '>=
            case _: PropertyIsLessThan => '<
            case _: PropertyIsLessThanOrEqualTo => '<=
            case _: PropertyIsNotEqualTo => '<>
            case _: PropertyIsNull => 'nullcheck
          }
          Some((symbol, binop.getExpression1(), binop.getExpression2()))
        case _ => None
      }
  }

  object And {
    def apply(children: Seq[Filter]) =
      children match {
        case Seq() => Filter.EXCLUDE
        case Seq(onlyChild) => onlyChild
        case children => filters.and(Arrays.asList(children.toArray: _*))
      }

    def unapply(f: Filter): Option[Seq[Filter]] = f match {
      case (f: And) => Some(f.getChildren())
      case _ => None
    }
  }

  object Or {
    def apply(children: Seq[Filter]) =
      children match {
        case Seq() => Filter.EXCLUDE
        case Seq(onlyChild) => onlyChild
        case children => filters.or(Arrays.asList(children.toArray: _*))
      }

    def unapply(f: Filter): Option[Seq[Filter]] = f match {
      case (f: Or) => Some(f.getChildren())
      case _ => None
    }
  }

  object Not {
    def unapply(f: Filter): Option[Filter] = f match {
      case f: Not => Some(f.getFilter)
      case _ => None
    }
  }

  object Prop {
    def unapply(e: Expression): Option[String] = e match {
      case e: PropertyName => Some(e.getPropertyName)
      case _ => None
    }
  }

  object Lit {
    def unapply(e: Expression): Option[Comparable[_]] = e match {
      case e: Literal if e.getValue.isInstanceOf[Comparable[_]] => {
        Some(e.getValue.asInstanceOf[Comparable[_]])
      }
      case _ => None
    }
  }
}
