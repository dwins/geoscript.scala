package org.geoscript.util

import org.specs._

class ClosingIteratorTests extends Specification {
  var closeCount = 0
  def closeOnce[A](op: Iterator[Int] => Unit) {
    closeCount = 0
    op(iterator)
    closeCount must_== 1
    op(iterator ++ List(1, 2, 3).elements)
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

  def iterator = closing(List(1, 2, 3).elements)

  "Closing iterators" should {
    "support folds" in {
      closeOnce { it => (0 /: it) { (_, _) => 0 } }
      closeOnce { it => (it foldLeft 0) { (_, _) => 0 } }
      closeOnce { it => (it :\ 0) { (_, _) => 0 } }
      closeOnce { it => (it foldRight 0) { (_, _) => 0 } }
      closeOnce { it => it forall { _ => true } }
      closeOnce { it => it forall { _ => false } }
      closeOnce { it => it foreach { _ => () } }
      closeOnce { it => it mkString(".") }
      closeOnce { it => it reduceLeft { _ + _ } }
      closeOnce { it => it reduceRight { _ + _ } }
    }

    "support searches" in {
      closeOnce { it => it contains 2 must beTrue }
      closeOnce { it => it contains 4 must beFalse }
      closeOnce { it => it exists (2 == ) must beTrue } 
      closeOnce { it => it exists (4 == ) must beFalse } 
      closeOnce { it => it find (2 ==) must_== Some(2) }
      closeOnce { it => it find (4 ==) must_== None }
      closeOnce { it => it findIndexOf (2 ==) }
      closeOnce { it => it findIndexOf (4 ==) }
      closeOnce { it => it indexOf 2 }
      closeOnce { it => it indexOf 4 }
    }
  }
}
