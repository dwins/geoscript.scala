package org.geoscript.geocss

trait Simplifier[P] {
  val Empty: P
  val Everything: P

  /**
   * Remove logical And's and Or's from a predicate tree, producing a Seq
   * containing all the simple predicates in the tree.
   */
  def flatten(pred: P): Seq[P] =
    pred match {
      case And(ps) => ps map(flatten) flatten
      case Or(ps) => ps map(flatten) flatten
      case p => Seq(p)
    }

  /**
   * Remove all predicates from a predicate tree that do not meet the
   * provided criterion, preserving logical And's and Or's.
   *
   * @returns None if the entire predicate tree fails the criterion, otherwise
   * a Some with the filtered tree.
   */
  def trim(crit: P => Boolean)(pred: P): Option[P] = {
    def traverse(pred: P): Option[P] =
      pred match {
        case And(children) =>
          Some(And(Everything +: children.flatMap(traverse)))
        case Or(children) =>
          Some(Or(Empty +: children.flatMap(traverse)))
        case pred if crit(pred) => Some(pred)
        case _ => None
      }

    traverse(pred)
  }

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
      case (And(a), And(b)) => Some(And(a ++ b))
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
      case (Or(a), Or(b)) => Some(Or(a ++ b))
      case (Or(a), b) => Some(Or(a ++ Seq(b)))
      case (And(a), b) if a.forall(union(_, b).isDefined) =>
        Some(And(a map { p => union(p, b) get }))
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

  def homologous(a: P, b: P): Boolean =
    (a, b) match {
      case (a, b) if a == b => true
      case (Or(as),  Or(bs))  =>
        as.length == bs.length &&
        as.forall(a => bs.exists(homologous(_, a))) &&
        bs.forall(b => as.exists(homologous(_, b)))
      case (And(as), And(bs)) =>
        as.length == bs.length &&
        as.forall(a => bs.exists(homologous(_, a))) &&
        bs.forall(b => as.exists(homologous(_, b)))
      case _ => false
    }

  def simplify(pred: P): P =
    ((normalize _) andThen (reduce _) andThen (stripContainers _))(pred)

  private def stripContainers(pred: P): P =
    pred match {
      case Or(ps) =>
        (ps map stripContainers) match {
          case Seq(p) => p
          case ps     => anyOf(ps)
        }
      case And(ps) =>
        (ps map stripContainers) match {
          case Seq(p) => p
          case ps     => allOf(ps)
        }
      case pred => pred
    }

  def reduce(pred: P): P = {
    def accum(a: P, bs: Seq[P])(op: (P, P) => Option[P]): (P, Seq[P]) = {
      var res = a
      val extras = collection.mutable.ListBuffer[P]()
      for (b <- bs)
        op(res, b) match {
          case Some(x) => res = x
          case None => extras += b
        }
      (res, extras.toSeq)
    }

    def aggregateReduce(xs: Seq[P], reduction: (P, P) => Option[P]): Seq[P] = {
      def iterate(xs: Seq[P]): Seq[P] = {
        var todo = xs
        var reduced = collection.mutable.ListBuffer[P]()
        while (todo nonEmpty) {
          val (simple, rest) = accum(todo.head, todo.tail)(reduction)
          reduced += simple
          todo = rest
        }
        reduced
      }

      val reductions = Stream.iterate(xs)(iterate)
      val alternatives =
        (reductions take 5) zip
        (reductions sliding(2) map(x => x(0).length == x(1).length)).toSeq

      ((alternatives find(_._2)) getOrElse (alternatives.last))._1
    }

    pred match {
      case Not(pred) =>
        not(pred)
      case And(children) =>
        aggregateReduce(children.map(reduce), intersection) match {
          case Seq() => allOf(Seq(Empty))
          case xs    => allOf(xs)
        }
      case Or(children) =>
        aggregateReduce(children.map(reduce), union) match {
          case Seq() => anyOf(Seq(Empty))
          case xs    => anyOf(xs)
        }
      case pred =>
        pred
    }
  }

  def normalize(pred: P): P =
    pred match {
      case and @ And(children) =>
        val combine: (P, P) => P = { (a, b) =>
          val Or(andA) = a
          val Or(andB) = b
          anyOf(
            for {
              And(as) <- andA
              And(bs) <- andB
            } yield allOf(as ++ bs)
          )
        }

        val empty = anyOf(Seq(allOf(Seq())))
        (children map normalize foldLeft(empty))(combine)
      case Or(children) =>
        anyOf(
          children map normalize flatMap { case Or(xs) => xs }
        )
      case pred => anyOf(Seq(allOf(Seq(pred))))
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
