package org.geoscript
package projection

trait Referenced[T] {
  def map[U](f: T => U): Referenced[U]
  def flatMap[U](f: T => Referenced[U]): Referenced[U]
  def force(p: Projection): T
}

object Referenced {
  def apply[T <: geometry.Geometry](value: T, projection: Projection)
    : Referenced[T] = new Physical(value, projection)

  def apply[T](value: T): Referenced[T] = new Ideal(value)

  private class Ideal[T](value: T) extends Referenced[T] {
    def map[U](f: T => U): Referenced[U] = new Ideal(f(value))
    def flatMap[U](f: T => Referenced[U]): Referenced[U] = f(value)
    def force(p: Projection): T = value
  }

  private class Physical[T <: geometry.Geometry](value: T, proj: Projection)
  extends Referenced[T] {
    def map[U](f: T => U): Referenced[U] =
      new Mapped(this, f)

    def flatMap[U](f: T => Referenced[U]): Referenced[U] =
      new FlatMapped(this, f)

    def force(p: Projection): T =
      (proj to p)(value)
  }

  private class Mapped[T, U](base: Referenced[T], f: T => U) extends Referenced[U] {
    def map[V](g: U => V): Referenced[V] =
      new Mapped(base, f andThen g)

    def flatMap[V](g: U => Referenced[V]): Referenced[V] =
      new FlatMapped(base, f andThen g)

    def force(p: Projection): U = f(base.force(p))
  }

  private class FlatMapped[T, U](base: Referenced[T], f: T => Referenced[U])
  extends Referenced[U] {
    def map[V](g: U => V): Referenced[V] = new Mapped(this, g)

    def flatMap[V](g: U => Referenced[V]): Referenced[V] =
       new FlatMapped(this, g)

    def force(p: Projection): U =
      f(base.force(p)).force(p)
  }
}
