package org.geoscript.util

abstract class ClosingIterator[A](iter: Iterator[A]) extends Iterator[A] {
  private var open = true

  def close()

  private def cleanup() {
    if (open) {
      open = false
      close()
    }
  }

  private def delegate[B](that: Iterator[B]): Iterator[B] = {
    new ClosingIterator(that) {
      def close() { ClosingIterator.this.cleanup() } 
    }
  }

  override def hasNext = 
    if (open) {
      if (iter.hasNext) true
      else {
        cleanup()
        false
      }
    } else false

  override def next = {
    if (!open) 
      throw new IllegalStateException("Called next() on a closed iterator")

    try {
      iter.next
    } catch {
      case ex => 
        cleanup()
        throw ex
    }
  }

  override def ++[B >: A](that: => Iterator[B]) = {
    val concatenated = super.++(that)
    new ClosingIterator(concatenated) {
      def close() { ClosingIterator.this.cleanup() } 
      def what = concatenated.what
      def what_=(b: Iterator[B]) = concatenated.what = b 
    }
  }

  override def take(n: Int) = delegate(super.take(n)) 

  override def filter(p: A => Boolean) = delegate(super.filter(p))

  override def map[B](f: A => B) = delegate(super.map(f))

  override def contains(elem: Any): Boolean = {
    try { super.contains(elem) }
    finally { cleanup() } 
  }

  override def exists(p: A => Boolean) = {
    try { super.exists(p) }
    finally { cleanup() } 
  }

  override def find(p: A => Boolean) = {
    try { super.find(p) }
    finally { cleanup() }
  }

  override def findIndexOf(p: A => Boolean) = {
    try { super.findIndexOf(p) }
    finally { cleanup() }
  }

  override def forall(p: A => Boolean) = {
    try { super.forall(p) }
    finally { cleanup() }
  }

  override def indexOf[B>:A](elem: B) = {
    try { super.indexOf(elem) } 
    finally { cleanup() } 
  }
}
