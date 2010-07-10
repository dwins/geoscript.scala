package org.geoserver.community.css

import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.scalatest.matchers.{HavePropertyMatcher, HavePropertyMatchResult}
import org.junit.Test

/**
 * Tests for general simplification rules
 */
class SimplifierTest extends JUnitSuite with MustMatchersForJUnit {

  sealed abstract class Predicate
  case object P extends Predicate
  case object Q extends Predicate
  case object R extends Predicate
  case class And(preds: Seq[Predicate]) extends Predicate {
    override def equals(that: Any): Boolean =
      that match {
        case And(p) =>
          preds.forall { p.contains } && p.forall { preds.contains } 
        case _ => false
      }
  }
  case class Or(preds: Seq[Predicate]) extends Predicate {
    override def equals(that: Any): Boolean =
      that match {
        case Or(p) =>
          preds.forall { p.contains } && p.forall { preds.contains } 
        case _ => false
      }
  }
  case class Not(pred: Predicate) extends Predicate

  object PredicateSimplifier extends Simplifier[Predicate] {
    object Everything extends Predicate
    object Empty extends Predicate

    override def invert(p: Predicate): Predicate = Not(p)
    override def allOf(preds: Seq[Predicate]): Predicate = And(preds)
    override def anyOf(preds: Seq[Predicate]): Predicate = Or(preds)

    override def intersectionExtract(p: Predicate): Option[Seq[Predicate]] =
      p match {
        case And(preds) => Some(preds)
        case _ => None
      }

    override def unionExtract(p: Predicate): Option[Seq[Predicate]] =
      p match {
        case Or(preds) => Some(preds)
        case _ => None
      }

    override def complementExtract(p: Predicate): Option[Predicate] =
      p match {
        case Not(p) => Some(p)
        case _ => None
      }
  }

  import PredicateSimplifier._
  
  @Test 
  def simpleSubsets {
    // predicates that compare equal are subsets
    isSubSet(P, P) must be (true)
    isSubSet(Q, Q) must be (true)

    // ones that don't, don't, other rules excluded
    isSubSet(P, Q) must be (false)
    isSubSet(Q, P) must be (false)

    // any predicate is a subset of Everything
    isSubSet(Everything, P) must be (true)
    isSubSet(Everything, Q) must be (true)
    isSubSet(Everything, Everything) must be (true)
    
    // with one exception:
    isSubSet(Everything, Empty) must be (false)

    // no predicate is a subset of Empty (the null set)
    isSubSet(Empty, P) must be (false)
    isSubSet(Empty, Q) must be (false)
    isSubSet(Empty, Empty) must be (false)
    isSubSet(Empty, Everything) must be (false)
  }

  @Test def simpleDisjoint {
    // self-disjointness is definitely not the case
    isDisjoint(P, P) must be (false)
    isDisjoint(Q, Q) must be (false)
    
    // except for Empty
    isDisjoint(Empty, Empty) must be (true)

    // we also return false when we're just not sure
    isDisjoint(P, Q) must be (false)
    isDisjoint(Q, P) must be (false)

    // you can't be disjoint with Everything
    isDisjoint(Everything, P) must be (false)
    isDisjoint(Everything, Q) must be (false)
    isDisjoint(P, Everything) must be (false)
    isDisjoint(Q, Everything) must be (false)

    // you're always disjoint with Empty
    isDisjoint(Empty, P) must be (true)
    isDisjoint(Empty, Q) must be (true)
    isDisjoint(P, Empty) must be (true)
    isDisjoint(Q, Empty) must be (true)

    // unstoppable force, meet immovable object (the object wins this time)
    isDisjoint(Everything, Empty) must be (true)
    isDisjoint(Empty, Everything) must be (true)
  }

  @Test
  def simpleCovering {
    // areCovering tests whether (a or b) is equivalent to Everything, so these
    // had better pass:
    areCovering(Everything, P) must be (true)
    areCovering(Everything, Q) must be (true)

    // the covering test is commutative
    areCovering(P, Everything) must be (true)
    areCovering(Q, Everything) must be (true)

    areCovering(Everything, Empty) must be (true)
    areCovering(Empty, Everything) must be (true)

    // that's pretty much all we can say for sure, everything else should be
    // false
    areCovering(P, Q) must be (false)
    areCovering(Q, P) must be (false)
    areCovering(P, Empty) must be (false)
    areCovering(Empty, P) must be (false)
    areCovering(Q, Empty) must be (false)
    areCovering(Empty, Q) must be (false)
  }

  @Test
  def simpleIntersect {
    // if there's no intersection that doesn't add structure, give a None
    intersection(P, Q) must be (None)

    // intersecting p with p gives p
    intersection(P, P) must be (Some(P))
    intersection(Q, Q) must be (Some(Q))

    // intersecting everything with p gives p
    intersection(Everything, P) must be (Some(P))
    intersection(P, Everything) must be (Some(P))
    intersection(Everything, Q) must be (Some(Q))
    intersection(Q, Everything) must be (Some(Q))

    // intersecting Empty with anything gives Empty
    intersection(Empty, P) must be (Some(Empty))
    intersection(P, Empty) must be (Some(Empty))
    intersection(Empty, Q) must be (Some(Empty))
    intersection(Q, Empty) must be (Some(Empty))

    intersection(Empty, Everything) must be (Some(Empty))
    intersection(Everything, Empty) must be (Some(Empty))
  }

  @Test
  def simpleUnion {
    // if there's no union that doesn't add structure, give a None
    union(P, Q) must be (None)

    // the union of p and p gives p
    union(P, P) must be (Some(P))
    union(Q, Q) must be (Some(Q))

    // the union of Everything with p gives Everything
    union(Everything, P) must be (Some(Everything))
    union(P, Everything) must be (Some(Everything))
    union(Everything, Q) must be (Some(Everything))
    union(Q, Everything) must be (Some(Everything))

    // the union of Empty with p gives p
    union(Empty, P) must be (Some(P))
    union(P, Empty) must be (Some(P))
    union(Empty, Q) must be (Some(Q))
    union(Q, Empty) must be (Some(Q))

    union(Empty, Everything) must be (Some(Everything))
    union(Everything, Empty) must be (Some(Everything))
  }

  @Test
  def simpleSimplify {
    // if a predicate is already simple, not much simplification to do
    simplify(P) must be (P)
    simplify(Q) must be (Q)
    simplify(Everything) must be (Everything)
    simplify(Empty) must be (Empty)
  }

  @Test 
  def negationSubsets {
    // !p should not be a subset of p
    isSubSet(P, Not(P)) must be (false)
    isSubSet(Q, Not(Q)) must be (false)
    isSubSet(Not(P), P) must be (false)
    isSubSet(Not(Q), Q) must be (false)

    // but that doesn't imply anything about q
    isSubSet(P, Not(Q)) must be (false)
    isSubSet(Q, Not(P)) must be (false)

    // Everything and Empty are still "special"
    isSubSet(Everything, Not(P)) must be (true)
    isSubSet(Everything, Not(Q)) must be (true)

    // And...
    isSubSet(Everything, Not(Everything)) must be (false)
    isSubSet(Empty, Not(Empty)) must be (false)
  }

  @Test
  def negationDisjoint {
    // p is always disjoint with Not(p)
    isDisjoint(P, Not(P)) must be (true)
    isDisjoint(Not(P), P) must be (true)
    isDisjoint(Q, Not(Q)) must be (true)
    isDisjoint(Not(Q), Q) must be (true)

    // but we still don't have a clue about how P and Q interact
    isDisjoint(P, Not(Q)) must be (false)
    isDisjoint(Q, Not(P)) must be (false)
    isDisjoint(Not(P), Q) must be (false)
    isDisjoint(Not(Q), P) must be (false)
  }

  @Test 
  def negationCovering {
    // a and !a always comes out to Everything
    areCovering(P, Not(P)) must be (true)
    areCovering(Q, Not(Q)) must be (true)
    areCovering(Everything, Not(Everything)) must be (true)
    areCovering(Empty, Not(Empty)) must be (true)

    // covering-ness is commutative
    areCovering(Not(P), P) must be (true)
    areCovering(Not(Q), Q) must be (true)
    areCovering(Not(Everything), Everything) must be (true)
    areCovering(Not(Empty), Empty) must be (true)

    // covering tests involving Not can still return false though
    areCovering(Not(P), Q) must be (false)
    areCovering(Not(Q), P) must be (false)
  }

  @Test
  def negationIntersect {
    intersection(P, Not(P)) must be (Some(Empty))
    intersection(Empty, Not(P)) must be (Some(Empty))
    intersection(Q, Not(Q)) must be (Some(Empty))
    intersection(Empty, Not(Q)) must be (Some(Empty))

    intersection(P, Not(Empty)) must be (Some(P))
    intersection(P, Not(Everything)) must be (Some(Empty))
  }

  @Test
  def negationUnion {
    union(P, Not(P)) must be (Some(Everything))
    union(Everything, Not(P)) must be (Some(Everything))
    union(Q, Not(Q)) must be (Some(Everything))
    union(Everything, Not(Q)) must be (Some(Everything))

    union(P, Not(Empty)) must be (Some(Not(Empty)))
    union(P, Not(Everything)) must be (Some(P))
  }

  @Test
  def negationSimplify {
    // if a predicate is already simple, not much simplification to do
    simplify(Not(P)) must be (Not(P))
    simplify(Not(Q)) must be (Not(Q))
    simplify(Not(Everything)) must be (Empty)
    simplify(Not(Empty)) must be (Everything)
  }

  @Test
  def intersectionSubsets {
    // basics - if you add criteria, you create a subset 
    isSubSet(P, And(Seq(P, Q))) must be (true)
    isSubSet(P, And(Seq(Q, P))) must be (true)
    isSubSet(P, And(Seq(Everything, P))) must be (true)
    isSubSet(Q, And(Seq(P, Q))) must be (true)
    isSubSet(Q, And(Seq(Q, P))) must be (true)
    isSubSet(Q, And(Seq(Everything, Q))) must be (true)

    // it's not enough to be a subset of a member of the And
    isSubSet(P, And(Seq(Everything, Q))) must be (false)
    isSubSet(P, And(Seq(Q, Everything))) must be (false)
    isSubSet(Q, And(Seq(Everything, P))) must be (false)
    isSubSet(Q, And(Seq(P, Everything))) must be (false)

    // subset-ness is not commutative, even when Ands are involved
    isSubSet(And(Seq(P, Q)), P) must be (false)
    isSubSet(And(Seq(P, Q)), Q) must be (false)

    // however, an And *can* have subsets
    isSubSet(And(Seq(Everything, P)), P) must be (true)
    isSubSet(And(Seq(Everything, Q)), Q) must be (true)

    // a single disjoint element is enough to throw you out as well
    isSubSet(P, And(Seq(Not(P), Q))) must be (false)
    isSubSet(Q, And(Seq(P, Not(Q)))) must be (false)
    isSubSet(P, And(Seq(Not(P), P))) must be (false)
    isSubSet(Q, And(Seq(Q, Not(Q)))) must be (false)
  }

  @Test
  def intersectionDisjoint {
    // if a single member is disjoint, then the entire group is disjoint
    isDisjoint(P, And(Seq(Everything, Not(P)))) must be (true)
    isDisjoint(Q, And(Seq(Everything, Not(Q)))) must be (true)
    
    // Disjointness is associative
    isDisjoint(And(Seq(Everything, Not(P))), P) must be (true)
    isDisjoint(And(Seq(Everything, Not(Q))), Q) must be (true)
    
    // Sometimes disjointness on Ands will actually return false
    isDisjoint(P, And(Seq(Everything, P))) must be (false)
    isDisjoint(Q, And(Seq(Everything, Q))) must be (false)

    // It's also false when we just aren't sure
    isDisjoint(P, And(Seq(Everything, Q))) must be (false)
    isDisjoint(Q, And(Seq(Everything, P))) must be (false)
  }

  @Test 
  def intersectionCovering {
    // if all members are covering, then the entire group is covering
    areCovering(P, And(Seq(Everything, Not(P)))) must be (true)
    areCovering(Q, And(Seq(Everything, Not(Q)))) must be (true)

    areCovering(P, And(Seq(Everything, Q))) must be (false)
    areCovering(Q, And(Seq(Everything, P))) must be (false)

    areCovering(Everything, And(Seq(Everything, Empty))) must be (true)
    areCovering(Empty, And(Seq(Everything, Empty))) must be (false)
  }

  @Test
  def intersectionIntersect {
    intersection(And(Seq(P, Q)), R) must be (Some(And(Seq(P, Q, R))))
    intersection(And(Seq(P, Q)), Q) must be (Some(And(Seq(P, Q))))
    intersection(And(Seq(P, Q)), Not(Q)) must be (Some(Empty))
  }

  @Test
  def intersectionUnion {
    union(And(Seq(P, Q)), R) must 
      be (Some(And(Seq(Or(Seq(P, R)), Or(Seq(Q, R))))))
    union(And(Seq(P, Q)), Q) must be (Some(Q))
    union(And(Seq(P, Q)), Not(Q)) must be (Some(Or(Seq(P, Not(Q)))))
  }

  @Test 
  def intersectionSimplify {
    simplify(And(Seq(P, Q))) must be (And(Seq(P, Q)))
    simplify(And(Seq(P, P))) must be (P)
    simplify(And(Seq(Everything, P))) must be (P)
    simplify(And(Seq(Not(P), P))) must be (Empty)
    simplify(And(Seq(Empty, P))) must be (Empty)
  }

  @Test 
  def unionSubsets {
    // basics - if you add criteria, a subset of any criterion is a subset
    isSubSet(Or(Seq(P, Q)), P) must be (true)
    isSubSet(Or(Seq(P, Q)), Q) must be (true)
    isSubSet(Or(Seq(Everything, Q)), P) must be (true)
    isSubSet(Or(Seq(P, Everything)), Q) must be (true)

    // subset-ness is still not commutative (stop asking me!)
    isSubSet(P, Or(Seq(P, Q))) must be (false)
    isSubSet(Q, Or(Seq(P, Q))) must be (false)
    isSubSet(P, Or(Seq(Everything, Q))) must be (false)
    isSubSet(Q, Or(Seq(P, Everything))) must be (false)

    // however, an Or *can* be a subset
    isSubSet(P, Or(Seq(P, P))) must be (true)
    isSubSet(Q, Or(Seq(Q, Q))) must be (true)

    // what if both arguments are Or's?
    isSubSet(Or(Seq(P, Q)), Or(Seq(P, Q))) must be (true)
  }

  @Test
  def unionDisjoint {
    // if all members are disjoint, then the entire group is disjoint
    isDisjoint(P, Or(Seq(Empty, Not(P)))) must be (true)
    isDisjoint(Q, Or(Seq(Empty, Not(Q)))) must be (true)
    
    // Disjointness is associative
    isDisjoint(Or(Seq(Empty, Not(P))), P) must be (true)
    isDisjoint(Or(Seq(Empty, Not(Q))), Q) must be (true)
    
    // Sometimes disjointness on Ors will actually return false
    isDisjoint(P, Or(Seq(Empty, P))) must be (false)
    isDisjoint(Q, Or(Seq(Empty, Q))) must be (false)

    // It's also false when we just aren't sure
    isDisjoint(P, Or(Seq(Empty, Q))) must be (false)
    isDisjoint(Q, Or(Seq(Empty, P))) must be (false)
  }

  @Test 
  def unionCovering {
    // if any members are covering, then the entire group is covering
    areCovering(P, Or(Seq(Empty, Not(P)))) must be (true)
    areCovering(Q, Or(Seq(Empty, Not(Q)))) must be (true)

    areCovering(P, Or(Seq(Empty, Q))) must be (false)
    areCovering(Q, Or(Seq(Empty, P))) must be (false)

    areCovering(Everything, Or(Seq(Everything, Empty))) must be (true)
    areCovering(Empty, Or(Seq(Everything, Empty))) must be (true)
  }

  @Test
  def unionIntersect {
    intersection(Or(Seq(P, Q)), R) must be (None)
    intersection(Or(Seq(P, Q)), Q) must be (Some(Q))
    intersection(Or(Seq(P, Q)), Not(Q)) must be (Some(And(Seq(P, Not(Q)))))
  }

  @Test
  def unionUnion {
    union(Or(Seq(P, Q)), R) must be (Some(Or(Seq(P, Q, R))))
    union(Or(Seq(P, Q)), Q) must be (Some(Or(Seq(P, Q))))
    union(Or(Seq(P, Q)), Not(Q)) must be (Some(Everything))
  }

  @Test 
  def unionSimplify {
    simplify(Or(Seq(P, Q))) must be (Or(Seq(P, Q)))
    simplify(Or(Seq(P, P))) must be (P)
    simplify(Or(Seq(Everything, P))) must be (Everything)
    simplify(Or(Seq(Not(P), P))) must be (Everything)
    simplify(Or(Seq(Empty, P))) must be (P)
  }

  // @Test 
  // def miscellaneousSimplification {
  //   simplify(And(Seq(Or(Seq(P, Not(Q))), And(Seq(R, Q))))) must
  //     be (And(Seq(P, Q, R)))
  // }
}
