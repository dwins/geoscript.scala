package org.geoscript.support.logic

package object symbolic {
  implicit def symbolAsAtom(s: Symbol): Sentence = Atom(s)
}

package symbolic {
  sealed trait Sentence
  case object False extends Sentence
  case object True extends Sentence

  case class Atom(s: Symbol) extends Sentence {
    override def toString = s.toString
  }

  case class Not(p: Sentence) extends Sentence {
    override def toString = "¬" + p.toString
  }

  case class And(p: Sentence, q: Sentence) extends Sentence {
    override def toString = "(%s ∧ %s)" format(p, q)
  }

  case class Or(p: Sentence, q: Sentence) extends Sentence {
    override def toString = "(%s ∨ %s)" format(p, q)
  }

  object Sentence {
    implicit object symbolsAreSentential extends Sentential[Sentence] {
      val True = symbolic.True
      val False = symbolic.False

      def and(p: Sentence, q: Sentence): Sentence = And(p, q)
      def extractAnd(p: Sentence): Option[(Sentence, Sentence)] =
        p match {
          case And(p, q) => Some((p, q))
          case _ => None
        }

      def or(p: Sentence, q: Sentence): Sentence = Or(p, q)
      def extractOr(p: Sentence): Option[(Sentence, Sentence)] =
        p match {
          case Or(p, q) => Some((p, q))
          case _ => None
        }

      def not(p: Sentence): Sentence = Not(p)
      def extractNot(p: Sentence): Option[Sentence] =
        p match {
          case Not(p) => Some(p)
          case _ => None
        }

      def isLiteral(p: Sentence): Boolean =
        p match {
          case Atom(_) | Not(Atom(_)) => true
          case _ => false
        }

      def provenBy(facts: Set[Sentence], s: Sentence): Boolean =
        facts contains s

      def disprovenBy(facts: Set[Sentence], s: Sentence): Boolean = {
        s match {
          case p @ Atom(_) => facts contains Not(p)
          case Not(p @ Atom(_)) => facts contains p
          case _ => sys.error("Tried to test a non-literal against a knowledge base")
        }
      }
    }
  }
}
