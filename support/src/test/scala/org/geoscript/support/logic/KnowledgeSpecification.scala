package org.geoscript.support.logic

import org.scalacheck._, Arbitrary._, Prop._

object KnowledgeSpecification extends Properties("Knowledge") {
  import Knowledge.sat
  import symbolic._
  import Generators._

  val oblivion = Knowledge.Oblivion[Sentence]
  import oblivion._


  def atomsIn(s: Sentence): Set[Atom] = {
    def helper(s: Sentence, accum: Set[Atom]): Set[Atom] = 
      s match {
        case True | False => accum
        case p @ Atom(_) => accum + p
        case And(p, q) => helper(p, helper(q, accum))
        case Or(p, q) => helper(p, helper(q, accum))
        case Not(p) => helper(p, accum)
      }

    helper(s, Set.empty)
  }

  type Assignment = Map[Atom, Boolean]

  def truthTable(s: Sentence)(atoms: Set[Atom] = atomsIn(s))
  : Map[Assignment, Boolean]
  = {
    require(atomsIn(s) subsetOf atoms)

    val allFalse = (atoms map (_ -> false) toMap)
    val assignments = 
      for (subset <- atoms.subsets) yield
        allFalse ++ (subset map (_ -> true))

    def evaluate(as: Assignment): Boolean = {
      def eval(s: Sentence): Boolean =
        s match {
          case False => false
          case True => true
          case p @ Atom(_) => as(p)
          case Not(s) => !(eval(s))
          case And(p, q) => eval(p) && eval(q)
          case Or(p, q) => eval(p) || eval(q)
        }

      eval(s)
    }

    assignments.map(a => (a, evaluate(a))).toMap
  }

  def size(s: Sentence): Int = {
    def helper(s: Sentence, accum: Int): Int = 
      s match {
        case True | False | Atom(_) => accum + 1
        case And(p, q) => helper(p, helper(q, accum + 1))
        case Or(p, q) => helper(p, helper(q, accum + 1))
        case Not(p) => helper(p, accum)
      }

    helper(s, 0)
  }

  property("Reduction never increases sentence size") = 
    forAll { (s: Sentence) => size(reduce(s)) <= size(s) }

  property("Reduction never introduces terms") =
    forAll { (s: Sentence) => atomsIn(reduce(s)) subsetOf atomsIn(s) }

  property("Reduction never alters the truth table") =
    forAll { (s: Sentence) => 
      val atoms = atomsIn(s)
      truthTable(s)(atoms) == truthTable(reduce(s))(atoms)
    }

  property("Conjunction with negation") =
    forAll { (s: Sentence) => reduce(And(s, Not(s))) == False }

  property("Disjunction with negation") =
    forAll { (s: Sentence) => reduce(Or(s, Not(s))) == True }

  property("Conjunction with self") =
    forAll { (s: Sentence) =>
      truthTable(reduce(And(s, s)))(atomsIn(s)) == truthTable(s)(atomsIn(s))
    }

  property("Disjunction with self") =
    forAll { (s: Sentence) => 
      truthTable(reduce(Or(s, s)))(atomsIn(s)) == truthTable(s)(atomsIn(s))
    }

  property("Satisfiability") =
    forAll { (s: Sentence) =>
      sat(s).forall { assignment =>
        val kb = assignment.foldLeft(oblivion) { _ given _ }
        kb.reduce(s) == True
      }
    }

  // TODO: Rewrite this to construct implied sentences instead of hoping to
  //       receive them randomly
  // property("Implication is transitive") =
    forAll { (p: Sentence, q: Sentence, r: Sentence) => 
      ((reduce(p) != False && reduce(q) != False) &&
      given(p).reduce(q) == True &&
      given(q).reduce(r) == True) ==> (given(p).reduce(r) == True)
    }
} 
