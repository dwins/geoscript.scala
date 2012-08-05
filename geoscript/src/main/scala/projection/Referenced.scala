package org.geoscript
package projection

import geometry.Envelope
import org.geotools.geometry.jts.ReferencedEnvelope

trait Referenced[T] {
  def map[U](f: T => U): Referenced[U]
  def flatMap[U](f: T => Referenced[U]): Referenced[U]
  def native: Option[Projection]
  def project(p: Projection): T
  def forceNative: T = project(native.get)
}

object Referenced {
  def apply[T : Projectable](value: T, projection: Projection)
    : Referenced[T] = new Physical(value, projection)

  def apply[T](value: T): Referenced[T] = new Ideal(value)

  private class Ideal[T](value: T) extends Referenced[T] {
    def map[U](f: T => U): Referenced[U] = new Ideal(f(value))
    def flatMap[U](f: T => Referenced[U]): Referenced[U] = f(value)
    def project(p: Projection): T = value
    def native = None
  }

  private class Physical[T : Projectable](value: T, proj: Projection)
  extends Referenced[T] {
    def map[U](f: T => U): Referenced[U] =
      new Mapped(this, f)

    def flatMap[U](f: T => Referenced[U]): Referenced[U] =
      new FlatMapped(this, f)

    def project(p: Projection): T =
      implicitly[Projectable[T]].project(proj, p)(value)

    def native = Some(proj)
  }

  private class Mapped[T, U](base: Referenced[T], f: T => U) extends Referenced[U] {
    def map[V](g: U => V): Referenced[V] =
      new Mapped(base, f andThen g)

    def flatMap[V](g: U => Referenced[V]): Referenced[V] =
      new FlatMapped(base, f andThen g)

    def project(p: Projection): U = f(base.project(p))

    def native = base.native
  }

  private class FlatMapped[T, U](base: Referenced[T], f: T => Referenced[U])
  extends Referenced[U] {
    def map[V](g: U => V): Referenced[V] = new Mapped(this, g)

    def flatMap[V](g: U => Referenced[V]): Referenced[V] =
       new FlatMapped(this, g)

    def project(p: Projection): U =
      f(base.project(p)).project(p)

    def native = base.native
  }

  import org.geotools.geometry.jts.ReferencedEnvelope
  implicit def referenceEnvelope(renv: ReferencedEnvelope): Referenced[Envelope] =
    Referenced(renv, renv.getCoordinateReferenceSystem)

  def envelope(renv: Referenced[Envelope]): ReferencedEnvelope =
    new ReferencedEnvelope(renv.forceNative, renv.native.orNull)
}
