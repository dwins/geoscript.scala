package org.geoscript.util

import org.specs._

class ClosingIteratorTests extends Specification {
  var closeCount = 0
  def closeOnce[A](op: Iterator[Int] => Unit) {
    closeCount = 0
    op(iterator)
    closeCount must_== 1
    op(iterator ++ List(1, 2, 3).iterator)
    closeCount must_== 2
    op(iterator drop 1)
    closeCount must_== 3
    op(iterator take 2)
    closeCount must_== 4
    op(iterator.filter(2 ==))
    closeCount must_== 5
    op(iterator.map(-1 +))
    closeCount must_== 6
    iterator.map(_ => throw new RuntimeException("Boom!")) // shouldn't go boom
    closeCount must_== 6
    op(iterator.map(_ => throw new RuntimeException("Boom!"))) must 
      throwA[RuntimeException]
    closeCount must_== 7
  }

  def closing[A](it: Iterator[A]): ClosingIterator[A] =
    new ClosingIterator(it) { def close() = closeCount += 1 }

  def iterator = closing(List(1, 2, 3).iterator)

  "Closing iterators" should {
    "support folds" in {
      closeOnce { it => (0 /: it) { (_, _) => 0 } }
      closeOnce { it => (it foldLeft 0) { (_, _) => 0 } }
      closeOnce { it => (it :\ 0) { (_, _) => 0 } }
      closeOnce { it => (it foldRight 0) { (_, _) => 0 } }
      closeOnce { _ forall { _ => true } }
      closeOnce { _ forall { _ => false } }
      closeOnce { _ foreach { _ => () } }
      closeOnce { _ mkString(".") }
      closeOnce { _ reduceLeft { _ + _ } }
      closeOnce { _ reduceRight { _ + _ } }
    }

    "support searches" in {
      closeOnce { _ contains 2 must beTrue }
      closeOnce { _ contains 4 must beFalse }
      closeOnce { _ exists (2 == ) must beTrue } 
      closeOnce { _ exists (4 == ) must beFalse } 
      closeOnce { _ find (2 ==) must_== Some(2) }
      closeOnce { _ find (4 ==) must_== None }
      closeOnce { _ indexWhere (2 ==) }
      closeOnce { _ indexWhere (4 ==) }
      closeOnce { _ indexOf 2 }
      closeOnce { _ indexOf 4 }
    }
  }
}
