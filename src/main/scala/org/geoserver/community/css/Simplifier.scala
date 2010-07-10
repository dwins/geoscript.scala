package org.geoserver.community.css

trait Simplifier[P] {
  val Empty: P
  val Everything: P

  def anyOf(preds: Seq[P]): P
  def simpleUnion(a: P, b: P): Option[P] = None
  def unionExtract(p: P): Option[Seq[P]]

  def allOf(preds: Seq[P]): P
  def simpleIntersection(a: P, b: P): Option[P] = None
  def intersectionExtract(p: P): Option[Seq[P]]

  def invert(pred: P): P
  def simpleComplement(pred: P): Option[P] = None
  def complementExtract(pred: P): Option[P]

  def simpleSubSet(set: P, candidate: P): Boolean = false
  def simpleDisjoint(a: P, b: P): Boolean = false
  def simpleCovering(a: P, b: P): Boolean = false
  def simpleSimplify(a: P): P = a

  def intersection(a: P, b: P): Option[P] = {
    if (isSubSet(a, b)) {
      Some(b)
    } else if (isSubSet(b, a)) {
      Some(a)
    } else if (isDisjoint(a, b)) {
      Some(Empty)
    } else {
      simpleIntersection(a, b) orElse
      simpleIntersection(b, a) orElse
      aggregateIntersection(a, b) orElse
      aggregateIntersection(b, a)
    }
  }

  def union(a: P, b: P): Option[P] = 
    if (isSubSet(a, b)) {
      Some(a)
    } else if (isSubSet(b, a)) {
      Some(b)
    } else if (areCovering(a, b)) {
      Some(Everything)
    } else {
      aggregateUnion(a, b) orElse
      aggregateUnion(b, a) orElse
      simpleUnion(a, b) orElse
      simpleUnion(b, a)
    }

  def isSubSet(set: P, candidate: P): Boolean = {
    (!isDisjoint(set, candidate)) && 
    (aggregateSubset(set, candidate) || simpleSubSet(set, candidate))
  }

  def isDisjoint(a: P, b: P): Boolean =
    // disjointness is commutative so let's do developers a favor and try it
    // both ways
    (a == Empty) || (b == Empty) || 
    ( a != b 
      && (
        aggregateDisjoint(a, b) || 
        aggregateDisjoint(b, a) || 
        simpleDisjoint(a, b) ||
        simpleDisjoint(b, a)
      ) 
    )

  def areCovering(a: P, b: P): Boolean = 
    // covering-ness is commutative so let's do developers a favor and try it
    // both ways
    aggregateCovering(a, b) ||
    aggregateCovering(b, a) ||
    simpleCovering(a, b) ||
    simpleCovering(b, a)

  def aggregateSubset(a: P, b: P): Boolean = {
    (a, b) match {
      case (_, Empty) => false
      case (Not(a), b) if a == b => false
      case (a, Not(b)) if a == b => false
      case (Not(a), b) 
        if isDisjoint(a, b)
        => true
      case (And(set), candidate) => 
        set.forall(isSubSet(_, candidate))
      case (candidate, And(set)) =>
        set.exists(isSubSet(candidate, _)) && 
        !set.exists(isDisjoint(candidate, _))
      case (Or(set), Or(candidates)) =>
        set.forall(s => candidates.exists(c => isSubSet(s, c)))
      case (Or(set), candidate)  =>
        set.exists(isSubSet(_, candidate))
      case (candidate, Or(set))  =>
        set.forall(isSubSet(candidate, _))
      case (a, b) if a == b => true
      case (Everything, _) => true
      case _ => 
        false
    }
  }

  def aggregateDisjoint(a: P, b: P): Boolean =
    (a, b) match {
      case (_, Empty) => true
      case (Everything, _) => false
      // case (Empty, Empty) => false
      case (Not(a), b) => isSubSet(a, b)
      case (And(a), b) => a.exists(isDisjoint(_, b))
      case (Or(a), b)  => a.forall(isDisjoint(_, b))
      case _ => false
    }

  def aggregateIntersection(a: P, b: P): Option[P] = {
    (a, b) match {
      case (Everything, p) => Some(p)
      case (Not(Empty), p) => Some(p)
      case (_, Empty) => Some(Empty)
      case (And(a), b) if a.exists(intersection(_, b).isDefined) =>
        Some(And(a map { p => intersection(p, b) getOrElse p }))
      case (And(a), b) => Some(And(a ++ Seq(b)))
      case (Or(a), b) if a.exists(intersection(_, b).isDefined) =>
        Some(Or(a map { p => intersection(p, b) getOrElse And(Seq(p, b)) }))
      case (a, b) if isDisjoint(a, b) => Some(Empty)
      case _ => None
    }
  }

  def aggregateCovering(a: P, b: P): Boolean =
    (a, b) match {
      case (Everything, _) => true
      case (Not(Empty), _) => true
      case (a, Not(b)) => isSubSet(a, b)
      case (a, And(bs)) => bs.forall { areCovering(a, _) }
      case (a, Or(bs)) => bs.exists { areCovering(a, _) }
      case _ => false
    }

  def aggregateUnion(a: P, b: P): Option[P] = {
    (a, b) match {
      case (Everything, _) => Some(Everything)
      case (Empty, p) => Some(p)
      case (Not(Everything), p) => Some(p)
      case (Or(a), b) if a.exists(union(_, b).isDefined) => 
        Some(Or(a map { p => union(p, b) getOrElse p }))
      case (Or(a), b) => Some(Or(a ++ Seq(b)))
      case (And(a), b) =>
        Some(And(a map { p => union(p, b) getOrElse Or(Seq(p, b)) }))
      case _ => None
    }
  }

  def complementAggregates(pred: P): Option[P] =
    pred match {
      case Everything => Some(Empty)
      case Empty => Some(Everything)
      case And(children) => Some(Or(children map not))
      case Or(children) => Some(And(children map not))
      case _ => None
    }

  def not(pred: P): P = 
    complementAggregates(pred) orElse 
    simpleComplement(pred) getOrElse 
    invert(pred)

  def equivalent(a: P, b: P): Boolean = 
    (a == b) ||
    (isSubSet(a, b) && isSubSet(b, a))

  def simplify(pred: P): P = 
    pred match {
      case Not(pred) => not(simplify(pred))
      case And(children) =>
        val flattened = children map simplify flatMap {
          case And(nested) => nested
          case child => Some(child)
        }

        def reduce(results: Seq[P], queue: Seq[P]): Seq[P] = {
          if (queue.exists { p => results.exists(isDisjoint(p, _)) }) {
            Seq(Empty)
          } else if (queue.size <= 1) {
            results ++ queue
          } else {
            var equiv = queue.first
            var shelf = new collection.mutable.ListBuffer[P]()
            for (pred <- queue.drop(1)) {
              val intersected = intersection(equiv, pred)
              if (intersected isDefined) {
                equiv = intersected.get
              } else {
                shelf += pred
              }
            }
            reduce(results ++ Seq(equiv), shelf)
          }
        }

        And(reduce(Seq(), flattened))
      case Or(children) =>
        val flattened = children map simplify flatMap {
          case Or(nested) => nested
          case child => Some(child)
        }
        
        def reduce(results: Seq[P], queue: Seq[P]): Seq[P] = {
          if (queue.exists { p => results.exists(areCovering(p, _)) }) {
            Seq(Empty)
          } else if (queue.size <= 1) {
            results ++ queue
          } else {
            var equiv = queue.first
            var shelf = new collection.mutable.ListBuffer[P]()
            for (pred <- queue.drop(1)) {
              val unioned = union(equiv, pred)
              if (unioned isDefined) {
                equiv = unioned.get
              } else {
                shelf += pred
              }
            }
            reduce(results ++ Seq(equiv), shelf)
          }
        }

        Or(reduce(Seq(), flattened))
      case pred => simpleSimplify(pred)
    }

  private object And {
    def unapply(pred: P): Option[Seq[P]] = intersectionExtract(pred)

    def apply(preds: Seq[P]): P = 
      if (preds contains Empty) {
        Empty
      } else {
        preds filter (Everything !=) match {
          case Seq() => 
            if (preds contains Everything) { Everything } 
            else { Empty }
          case Seq(singleton) => singleton
          case preds => allOf(preds)
        }
      }
  }

  private object Or {
    def unapply(pred: P): Option[Seq[P]] = unionExtract(pred)

    def apply(preds: Seq[P]): P = {
      if (preds contains Everything) {
        Everything
      } else {
        preds filter (Empty !=) match {
          case Seq() => Empty
          case Seq(singleton) => singleton
          case preds => anyOf(preds)
        }
      }
    }
  }

  private object Not {
    def unapply(pred: P): Option[P] = complementExtract(pred)
  }
}
