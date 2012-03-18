package org.geoscript.support.logic

trait Sentential[Sentence] {
  val False: Sentence
  val True: Sentence

  def not(p: Sentence): Sentence
  def extractNot(p: Sentence): Option[Sentence]

  def and(p: Sentence, q: Sentence): Sentence
  def extractAnd(p: Sentence): Option[(Sentence, Sentence)]

  def or(p: Sentence, q: Sentence): Sentence
  def extractOr(p: Sentence): Option[(Sentence, Sentence)]

  def isLiteral(p: Sentence): Boolean
  def provenBy(givens: Set[Sentence], s: Sentence): Boolean
  def disprovenBy(givens: Set[Sentence], s: Sentence): Boolean

  object Ops {
    object Not {
      def apply(p: Sentence): Sentence = not(p)
      def unapply(p: Sentence): Option[Sentence] = extractNot(p)
    }

    object And {
      def apply(p: Sentence, q: Sentence): Sentence = and(p, q)
      def unapply(p: Sentence): Option[(Sentence, Sentence)] = extractAnd(p)
    }

    object Or {
      def apply(p: Sentence, q: Sentence): Sentence = or(p, q)
      def unapply(p: Sentence): Option[(Sentence, Sentence)] = extractOr(p)
    }

    object Literal {
      def unapply(p: Sentence): Boolean = isLiteral(p)
    }
  }
}
