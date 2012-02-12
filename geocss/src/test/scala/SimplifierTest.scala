package org.geoscript.geocss

import org.specs2._

/**
 * Tests for general simplification rules
 */
class SimplifierTest extends Specification { def is = pending
//  sealed abstract class Predicate
//  case object P extends Predicate
//  case object Q extends Predicate
//  case object R extends Predicate
//  case class And(preds: Seq[Predicate]) extends Predicate {
//    override def equals(that: Any): Boolean =
//      that match {
//        case And(p) =>
//          preds.forall { p.contains } && p.forall { preds.contains } 
//        case _ => false
//      }
//  }
//  case class Or(preds: Seq[Predicate]) extends Predicate {
//    override def equals(that: Any): Boolean =
//      that match {
//        case Or(p) =>
//          preds.forall { p.contains } && p.forall { preds.contains } 
//        case _ => false
//      }
//  }
//  case class Not(pred: Predicate) extends Predicate
//
//  object PredicateSimplifier extends Simplifier[Predicate] {
//    object Everything extends Predicate
//    object Empty extends Predicate
//
//    override def invert(p: Predicate): Predicate = Not(p)
//    override def allOf(preds: Seq[Predicate]): Predicate = And(preds)
//    override def anyOf(preds: Seq[Predicate]): Predicate = Or(preds)
//
//    override def intersectionExtract(p: Predicate): Option[Seq[Predicate]] =
//      p match {
//        case And(preds) => Some(preds)
//        case _ => None
//      }
//
//    override def unionExtract(p: Predicate): Option[Seq[Predicate]] =
//      p match {
//        case Or(preds) => Some(preds)
//        case _ => None
//      }
//
//    override def complementExtract(p: Predicate): Option[Predicate] =
//      p match {
//        case Not(p) => Some(p)
//        case _ => None
//      }
//  }
//
//  import PredicateSimplifier._
//  
//  "predicates that compare equal are subsets" in {
//    isSubSet(P, P) must_== (true)
//    isSubSet(Q, Q) must_== (true)
//  }
//
//  "predicates that aren't equal are not subsets, in general" in {
//    isSubSet(P, Q) must_== (false)
//    isSubSet(Q, P) must_== (false)
//  }
//
//  "any predicate is a subset of Everything" in {
//    isSubSet(Everything, P) must_== (true)
//    isSubSet(Everything, Q) must_== (true)
//    isSubSet(Everything, Everything) must_== (true)
//    // with one exception:
//    isSubSet(Everything, Empty) must_== (false)
//  }
//
//  "no predicate is a subset of Empty (the null set)" in {
//    isSubSet(Empty, P) must_== (false)
//    isSubSet(Empty, Q) must_== (false)
//    isSubSet(Empty, Empty) must_== (false)
//    isSubSet(Empty, Everything) must_== (false)
//  }
//
//  "predicates are never self-disjoint" in {
//    isDisjoint(P, P) must_== (false)
//    isDisjoint(Q, Q) must_== (false)
//    
//    // except for Empty
//    isDisjoint(Empty, Empty) must_== (true)
//  }
//
//  "disjoint tests return false in ambiguous cases" in {
//    isDisjoint(P, Q) must_== (false)
//    isDisjoint(Q, P) must_== (false)
//  }
//
//  "nothing is disjoint with Everything" in {
//    isDisjoint(Everything, P) must_== (false)
//    isDisjoint(Everything, Q) must_== (false)
//    isDisjoint(P, Everything) must_== (false)
//    isDisjoint(Q, Everything) must_== (false)
//  }
//
//  "a predicate is  always disjoint with Empty" in {
//    isDisjoint(Empty, P) must_== (true)
//    isDisjoint(Empty, Q) must_== (true)
//    isDisjoint(P, Empty) must_== (true)
//    isDisjoint(Q, Empty) must_== (true)
//
//    // unstoppable force, meet immovable object (the object wins this time)
//    isDisjoint(Everything, Empty) must_== (true)
//    isDisjoint(Empty, Everything) must_== (true)
//  }
//
//  "any predicate is covering in combination with Everything" in {
//    // areCovering tests whether (a or b) is equivalent to Everything, so these
//    // had better pass:
//    areCovering(Everything, P) must_== (true)
//    areCovering(Everything, Q) must_== (true)
//
//    // the covering test is commutative
//    areCovering(P, Everything) must_== (true)
//    areCovering(Q, Everything) must_== (true)
//
//    areCovering(Everything, Empty) must_== (true)
//    areCovering(Empty, Everything) must_== (true)
//  }
//
//  "areCovering returns false in ambiguous cases" in {
//    areCovering(P, Q) must_== (false)
//    areCovering(Q, P) must_== (false)
//    areCovering(P, Empty) must_== (false)
//    areCovering(Empty, P) must_== (false)
//    areCovering(Q, Empty) must_== (false)
//    areCovering(Empty, Q) must_== (false)
//  }
//
//  "intersection returns None in ambiguous cases" in {
//    intersection(P, Q) must_== (None)
//  }
//
//  "self-intersection returns self" in {
//    intersection(P, P) must_== Some(P)
//    intersection(Q, Q) must_== Some(Q)
//  }
//
//  "intersection with Everything also returns self" in {
//    intersection(Everything, P) must_== (Some(P))
//    intersection(P, Everything) must_== (Some(P))
//    intersection(Everything, Q) must_== (Some(Q))
//    intersection(Q, Everything) must_== (Some(Q))
//  }
//
//  "intersection with Empty gives Empty" in {
//    intersection(Empty, P) must_== (Some(Empty))
//    intersection(P, Empty) must_== (Some(Empty))
//    intersection(Empty, Q) must_== (Some(Empty))
//    intersection(Q, Empty) must_== (Some(Empty))
//
//    intersection(Empty, Everything) must_== (Some(Empty))
//    intersection(Everything, Empty) must_== (Some(Empty))
//  }
//
//  "union returns None in ambiguous cases" in {
//    union(P, Q) must_== (None)
//  }
//
//  "self-union returns self" in {
//    union(P, P) must_== (Some(P))
//    union(Q, Q) must_== (Some(Q))
//  }
//
//  "any predicate unioned with Everything gives Everything" in {
//    union(Everything, P) must_== (Some(Everything))
//    union(P, Everything) must_== (Some(Everything))
//    union(Everything, Q) must_== (Some(Everything))
//    union(Q, Everything) must_== (Some(Everything))
//  }
//
//  "any predicate unioned with Empty returns itself" in {
//    union(Empty, P) must_== (Some(P))
//    union(P, Empty) must_== (Some(P))
//    union(Empty, Q) must_== (Some(Q))
//    union(Q, Empty) must_== (Some(Q))
//
//    union(Empty, Everything) must_== (Some(Everything))
//    union(Everything, Empty) must_== (Some(Everything))
//  }
//
//  "simplifying simple predicates produces the original filter" in {
//    simplify(P) must_== (P)
//    simplify(Q) must_== (Q)
//    simplify(Everything) must_== (Everything)
//    simplify(Empty) must_== (Empty)
//  }
//
//  "subsets of negated predicates" in {
//    // !p should not be a subset of p
//    isSubSet(P, Not(P)) must_== (false)
//    isSubSet(Q, Not(Q)) must_== (false)
//    isSubSet(Not(P), P) must_== (false)
//    isSubSet(Not(Q), Q) must_== (false)
//
//    // but that doesn't imply anything about q
//    isSubSet(P, Not(Q)) must_== (false)
//    isSubSet(Q, Not(P)) must_== (false)
//
//    // Everything and Empty are still "special"
//    isSubSet(Everything, Not(P)) must_== (true)
//    isSubSet(Everything, Not(Q)) must_== (true)
//
//    // And...
//    isSubSet(Everything, Not(Everything)) must_== (false)
//    isSubSet(Empty, Not(Empty)) must_== (false)
//  }
//
//  "disjointness of negated predicates" in {
//    // p is always disjoint with Not(p)
//    isDisjoint(P, Not(P)) must_== (true)
//    isDisjoint(Not(P), P) must_== (true)
//    isDisjoint(Q, Not(Q)) must_== (true)
//    isDisjoint(Not(Q), Q) must_== (true)
//
//    // but we still don't have a clue about how P and Q interact
//    isDisjoint(P, Not(Q)) must_== (false)
//    isDisjoint(Q, Not(P)) must_== (false)
//    isDisjoint(Not(P), Q) must_== (false)
//    isDisjoint(Not(Q), P) must_== (false)
//  }
//
//  "covering tests on negated predicates" in {
//    // a and !a always comes out to Everything
//    areCovering(P, Not(P)) must_== (true)
//    areCovering(Q, Not(Q)) must_== (true)
//    areCovering(Everything, Not(Everything)) must_== (true)
//    areCovering(Empty, Not(Empty)) must_== (true)
//
//    // covering-ness is commutative
//    areCovering(Not(P), P) must_== (true)
//    areCovering(Not(Q), Q) must_== (true)
//    areCovering(Not(Everything), Everything) must_== (true)
//    areCovering(Not(Empty), Empty) must_== (true)
//
//    // covering tests involving Not can still return false though
//    areCovering(Not(P), Q) must_== (false)
//    areCovering(Not(Q), P) must_== (false)
//  }
//
//  "intersections of negated predicates" in {
//    intersection(P, Not(P)) must_== (Some(Empty))
//    intersection(Empty, Not(P)) must_== (Some(Empty))
//    intersection(Q, Not(Q)) must_== (Some(Empty))
//    intersection(Empty, Not(Q)) must_== (Some(Empty))
//
//    intersection(P, Not(Empty)) must_== (Some(P))
//    intersection(P, Not(Everything)) must_== (Some(Empty))
//  }
// 
//  "unions of negated predicates" in {
//    union(P, Not(P)) must_== (Some(Everything))
//    union(Everything, Not(P)) must_== (Some(Everything))
//    union(Q, Not(Q)) must_== (Some(Everything))
//    union(Everything, Not(Q)) must_== (Some(Everything))
//
//    union(P, Not(Empty)) must_== (Some(Not(Empty)))
//    union(P, Not(Everything)) must_== (Some(P))
//  }
//
//  "simplification of negated predicates" in {
//    // if a predicate is already simple, not much simplification to do
//    simplify(Not(P)) must_== (Not(P))
//    simplify(Not(Q)) must_== (Not(Q))
//    simplify(Not(Everything)) must_== (Empty)
//    simplify(Not(Empty)) must_== (Everything)
//  }
//
//  "subset tests on And'ed predicates" in {
//    // basics - if you add criteria, you create a subset 
//    isSubSet(P, And(Seq(P, Q))) must_== (true)
//    isSubSet(P, And(Seq(Q, P))) must_== (true)
//    isSubSet(P, And(Seq(Everything, P))) must_== (true)
//    isSubSet(Q, And(Seq(P, Q))) must_== (true)
//    isSubSet(Q, And(Seq(Q, P))) must_== (true)
//    isSubSet(Q, And(Seq(Everything, Q))) must_== (true)
//
//    // it's not enough to be a subset of a member of the And
//    isSubSet(P, And(Seq(Everything, Q))) must_== (false)
//    isSubSet(P, And(Seq(Q, Everything))) must_== (false)
//    isSubSet(Q, And(Seq(Everything, P))) must_== (false)
//    isSubSet(Q, And(Seq(P, Everything))) must_== (false)
//
//    // subset-ness is not commutative, even when Ands are involved
//    isSubSet(And(Seq(P, Q)), P) must_== (false)
//    isSubSet(And(Seq(P, Q)), Q) must_== (false)
//
//    // however, an And *can* have subsets
//    isSubSet(And(Seq(Everything, P)), P) must_== (true)
//    isSubSet(And(Seq(Everything, Q)), Q) must_== (true)
//
//    // a single disjoint element is enough to throw you out as well
//    isSubSet(P, And(Seq(Not(P), Q))) must_== (false)
//    isSubSet(Q, And(Seq(P, Not(Q)))) must_== (false)
//    isSubSet(P, And(Seq(Not(P), P))) must_== (false)
//    isSubSet(Q, And(Seq(Q, Not(Q)))) must_== (false)
//  }
//
//  "disjointness tests on And'ed predicates" in {
//    // if a single member is disjoint, then the entire group is disjoint
//    isDisjoint(P, And(Seq(Everything, Not(P)))) must_== (true)
//    isDisjoint(Q, And(Seq(Everything, Not(Q)))) must_== (true)
//    
//    // Disjointness is associative
//    isDisjoint(And(Seq(Everything, Not(P))), P) must_== (true)
//    isDisjoint(And(Seq(Everything, Not(Q))), Q) must_== (true)
//    
//    // Sometimes disjointness on Ands will actually return false
//    isDisjoint(P, And(Seq(Everything, P))) must_== (false)
//    isDisjoint(Q, And(Seq(Everything, Q))) must_== (false)
//
//    // It's also false when we just aren't sure
//    isDisjoint(P, And(Seq(Everything, Q))) must_== (false)
//    isDisjoint(Q, And(Seq(Everything, P))) must_== (false)
//  }
//
//  "covering tests on And'ed predicates" in {
//    // if all members are covering, then the entire group is covering
//    areCovering(P, And(Seq(Everything, Not(P)))) must_== (true)
//    areCovering(Q, And(Seq(Everything, Not(Q)))) must_== (true)
//
//    areCovering(P, And(Seq(Everything, Q))) must_== (false)
//    areCovering(Q, And(Seq(Everything, P))) must_== (false)
//
//    areCovering(Everything, And(Seq(Everything, Empty))) must_== (true)
//    areCovering(Empty, And(Seq(Everything, Empty))) must_== (false)
//  }
//
//  "intersections of And'ed predicates" in {
//    intersection(And(Seq(P, Q)), R) must_== (Some(And(Seq(P, Q, R))))
//    intersection(And(Seq(P, Q)), Q) must_== (Some(And(Seq(P, Q))))
//    intersection(And(Seq(P, Q)), Not(Q)) must_== (Some(Empty))
//  }
//
//  "unions of And'ed predicates" in {
//    union(And(Seq(P, Q)), Q) must beSome(Q)
//  }
//
//  "simplification of And'ed predicates" in {
//    simplify(And(Seq(P, Q))) must_== (And(Seq(P, Q)))
//    simplify(And(Seq(P, P))) must_== (P)
//    simplify(And(Seq(Everything, P))) must_== (P)
//    simplify(And(Seq(Not(P), P))) must_== (Empty)
//    simplify(And(Seq(Empty, P))) must_== (Empty)
//  }
//
//  "subsets of Or'ed predicates" in {
//    // basics - if you add criteria, a subset of any criterion is a subset
//    isSubSet(Or(Seq(P, Q)), P) must_== (true)
//    isSubSet(Or(Seq(P, Q)), Q) must_== (true)
//    isSubSet(Or(Seq(Everything, Q)), P) must_== (true)
//    isSubSet(Or(Seq(P, Everything)), Q) must_== (true)
//
//    // subset-ness is still not commutative (stop asking me!)
//    isSubSet(P, Or(Seq(P, Q))) must_== (false)
//    isSubSet(Q, Or(Seq(P, Q))) must_== (false)
//    isSubSet(P, Or(Seq(Everything, Q))) must_== (false)
//    isSubSet(Q, Or(Seq(P, Everything))) must_== (false)
//
//    // however, an Or *can* be a subset
//    isSubSet(P, Or(Seq(P, P))) must_== (true)
//    isSubSet(Q, Or(Seq(Q, Q))) must_== (true)
//
//    // what if both arguments are Or's?
//    isSubSet(Or(Seq(P, Q)), Or(Seq(P, Q))) must_== (true)
//  }
//
//  "disjointness tests on Or'ed predicates" in {
//    // if all members are disjoint, then the entire group is disjoint
//    isDisjoint(P, Or(Seq(Empty, Not(P)))) must_== (true)
//    isDisjoint(Q, Or(Seq(Empty, Not(Q)))) must_== (true)
//    
//    // Disjointness is associative
//    isDisjoint(Or(Seq(Empty, Not(P))), P) must_== (true)
//    isDisjoint(Or(Seq(Empty, Not(Q))), Q) must_== (true)
//    
//    // Sometimes disjointness on Ors will actually return false
//    isDisjoint(P, Or(Seq(Empty, P))) must_== (false)
//    isDisjoint(Q, Or(Seq(Empty, Q))) must_== (false)
//
//    // It's also false when we just aren't sure
//    isDisjoint(P, Or(Seq(Empty, Q))) must_== (false)
//    isDisjoint(Q, Or(Seq(Empty, P))) must_== (false)
//  }
//
//  "covering tests on Or'ed predicates" in {
//    // if any members are covering, then the entire group is covering
//    areCovering(P, Or(Seq(Empty, Not(P)))) must_== (true)
//    areCovering(Q, Or(Seq(Empty, Not(Q)))) must_== (true)
//
//    areCovering(P, Or(Seq(Empty, Q))) must_== (false)
//    areCovering(Q, Or(Seq(Empty, P))) must_== (false)
//
//    areCovering(Everything, Or(Seq(Everything, Empty))) must_== (true)
//    areCovering(Empty, Or(Seq(Everything, Empty))) must_== (true)
//  }
//
//  "intersections of Or'ed predicates" in {
//    intersection(Or(Seq(P, Q)), R) must_== (None)
//    intersection(Or(Seq(P, Q)), Q) must_== (Some(Q))
//    intersection(Or(Seq(P, Q)), Not(Q)) must_== (Some(And(Seq(P, Not(Q)))))
//    (intersection(Or(Seq(P, Q)), Or(Seq(P, Not(Q)))) map simplify) must_== Some(P)
//  }
//
//  "unions of Or'ed predicates" in {
//    union(Or(Seq(P, Q)), R) must_== (Some(Or(Seq(P, Q, R))))
//    union(Or(Seq(P, Q)), Q) must_== (Some(Or(Seq(P, Q))))
//    union(Or(Seq(P, Q)), Not(Q)) must_== (Some(Everything))
//  }
//
//  "simplification of Or'ed predicates" in {
//    simplify(Or(Seq(P, Q))) must_== (Or(Seq(P, Q)))
//    simplify(Or(Seq(P, P))) must_== (P)
//    simplify(Or(Seq(Everything, P))) must_== (Everything)
//    simplify(Or(Seq(Not(P), P))) must_== (Everything)
//    simplify(Or(Seq(Empty, P))) must_== (P)
//  }
//
//  "miscellaneous combinations" in {
//    union(P, Or(Seq(Not(P), Q))) must beSome(Everything)
//    union(Not(P), Or(Seq(P, Q))) must beSome(Everything)
//    simplify(Or(Seq(P, Or(Seq(Not(P), Q))))) must_== Everything
//    simplify(Or(Seq(Not(P), Or(Seq(P, Q))))) must_== Everything
//
//    intersection(P, Or(Seq(P, R))) must beSome(P)
//    simplify(And(Seq(P, Or(Seq(P, R))))) must_== P
//  }
}
