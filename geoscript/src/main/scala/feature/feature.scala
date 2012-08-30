package org.geoscript

import geometry._
import org.opengis.feature.`type`.AttributeDescriptor
import scala.collection.JavaConverters._

package object feature extends org.geoscript.feature.LowPriorityImplicits {
  type Feature = org.opengis.feature.simple.SimpleFeature
  type FeatureCollection = org.geotools.data.simple.SimpleFeatureCollection
  type Field = org.opengis.feature.`type`.AttributeDescriptor
  type GeoField = org.opengis.feature.`type`.GeometryDescriptor
  type Schema = org.opengis.feature.simple.SimpleFeatureType

  def bind[T : Bindable](name: String): Field =
    implicitly[Bindable[T]].bind(name)

  def bind[T <: geometry.Geometry : Manifest]
    (name: String, proj: projection.Projection): GeoField = {
    val builder = new org.geotools.feature.AttributeTypeBuilder
    builder.setName(name)
    builder.setBinding(manifest[T].erasure)
    builder.setCRS(proj)
    builder.buildDescriptor(name, builder.buildGeometryType())
  }

  private def field(name: String, binding: Class[_]): Field = {
    val builder = new org.geotools.feature.AttributeTypeBuilder
    builder.setName(name)
    builder.setBinding(binding)
    builder.buildDescriptor(name, builder.buildType)
  }

  def setSchemaName(name: String, schema: Schema): Schema = {
    val builder = new org.geotools.feature.simple.SimpleFeatureTypeBuilder
    builder.init(schema)
    builder.setName(name)
    builder.buildFeatureType
  }

  def fromAttributes(attributes: (String, Any)*): Feature = {
    val fields = attributes.map { case (n, v) => field(n, v.getClass) }
    val schema = Schema("internal", fields)
    val builder = new org.geotools.feature.simple.SimpleFeatureBuilder(schema)
    for ((key, value) <- attributes) builder.set(key, value)
    builder.buildFeature(null)
  }

  def feature(schema: Schema, attributes: Seq[Any]): Feature = {
    import org.geotools.feature.simple.SimpleFeatureBuilder
    val builder = new SimpleFeatureBuilder(schema)
    for ((value, idx) <- attributes.zipWithIndex)
      builder.set(idx, value)
    builder.buildFeature(null)
  }

  def next[A : Bindable]: PositionalFieldSet[A] =
    new DirectPositionalFieldSet

  def pos[A : Bindable](index: Int): FieldSet[A] = new IndexedFieldSet(index)

  def named[A : Bindable](name: String): AbsoluteFieldSet[A] = 
    new NamedFieldSet(name)

  implicit def bindingSugar(name: String) = new {
    def binds[T : Bindable] = named[T](name)
  }

  implicit def combinesWithTilde[T](t: T) = new {
    def ~[U,V](u: U)(implicit chain: TildeCombine[T,U,V]): V = chain(t, u)
  }

  implicit def combinesWithChain[A, B, T[_] : TildeChainable]
  : TildeCombine[T[A], T[B], T[A ~ B]] = 
    new TildeCombine[T[A], T[B], T[A ~ B]] {
      def apply(a: T[A], b: T[B]): T[A ~ B] =
        implicitly[TildeChainable[T]].chain(a,b)
    }

  implicit object positionalFieldSetsAreChainable 
  extends TildeChainable[PositionalFieldSet]
  {
    def chain[A,B](
      a: PositionalFieldSet[A],
      b: PositionalFieldSet[B]
    ): PositionalFieldSet[A ~ B] = new CombinedPositionalFieldSet(a, b)
  }

  implicit object absoluteFieldSetsAreChainable 
  extends TildeChainable[AbsoluteFieldSet]
  {
    def chain[A,B](
      a: AbsoluteFieldSet[A],
      b: AbsoluteFieldSet[B]
    ): AbsoluteFieldSet[A ~ B] = new CombinedAbsoluteFieldSet(a, b)
  }

  implicit def asFeatureCollection(fs: Traversable[Feature]): FeatureCollection = {
    import scala.collection.JavaConversions._
    new org.geotools.data.collection.ListFeatureCollection(fs.head.schema, fs.toSeq)
  }

  implicit def bindGeometry[G <: Geometry : Manifest]: Bindable[G] =
    new Bindable[G] {
      def bind(name: String): Field = {
        val builder = new org.geotools.feature.AttributeTypeBuilder
        builder.setName(name)
        builder.setBinding(manifest[G].erasure)
        // builder.setCRS(proj)
        builder.buildDescriptor(name, builder.buildGeometryType())
      }

      def withDefaultName = bind(implicitly[Manifest[G]].erasure.getSimpleName)
    }
}

package feature {
  sealed class ~[A,B](val a: A, val b: B) {
    override def toString = a + " ~ " + b
  }

  object ~ {
    def unapply[A,B](x: (A ~ B)): Some[(A,B)] = Some((x.a, x.b))
  }

  class TildeChainer[A, T[_]] private[feature](a: T[A]) {
    def ~[B](b: T[B])(implicit chainable: TildeChainable[T]): T[A ~ B] =
      implicitly[TildeChainable[T]].chain(a, b)
  }

  trait TildeCombine[T, U, V] extends ((T, U) => V)

  trait TildeChainable[T[_]] {
    def chain[A,B](a: T[A], b: T[B]): T[A ~ B]
  }

  trait FieldSet[T] {
    def unapply(ft: Feature): Option[T]
    // def toSeq: Seq[Field]
  }

  sealed trait PositionalFieldSet[T] extends FieldSet[T] { self =>
    def unapply(xs: Feature): Some[T] = Some(extract(xs, 0)._1)
    def extract(ft: Feature, idx: Int): (T, Int)
  }

  private class DirectPositionalFieldSet[T : Bindable] extends PositionalFieldSet[T] {
    def extract(ft: Feature, idx: Int): (T, Int) =
      (ft.getAttribute(idx).asInstanceOf[T], idx + 1)

    def toSeq: Seq[Field] =
      Seq(implicitly[Bindable[T]].withDefaultName)
  }

  private class CombinedPositionalFieldSet[T, U](
    exT: PositionalFieldSet[T],
    exU: PositionalFieldSet[U])
  extends PositionalFieldSet[T ~ U]
  {
    override def extract(ft: Feature, idx: Int): (T ~ U, Int) = {
      val (t, nextIndex) = exT.extract(ft, idx)
      val (u, nextIndex2) = exU.extract(ft, nextIndex)
      (new ~ (t, u), nextIndex2)
    }
  }

  class IndexedFieldSet[T : Bindable](index: Int) extends FieldSet[T] {
    def unapply(ft: Feature) = Some(extract(ft))
    def extract(ft: Feature) = ft.getAttribute(index).asInstanceOf[T]
    def build(builder: org.geotools.feature.simple.SimpleFeatureBuilder, t: T): Unit =
      builder.set(index, t)
    def toSeq: Seq[Field] =
      Seq(implicitly[Bindable[T]].withDefaultName)
  }

  sealed trait AbsoluteFieldSet[T] extends FieldSet[T] {
    def apply(t: T): Feature = {
      val schema = Schema("builder", this)
      val builder = new org.geotools.feature.simple.SimpleFeatureBuilder(schema)
      build(builder, t)
      builder.buildFeature(null)
    }
    def build(builder: org.geotools.feature.simple.SimpleFeatureBuilder, t: T): Unit
    def extract(xs: Feature): T 
    def unapply(xs: Feature): Some[T] = Some(extract(xs))

    def toSeq: Seq[Field]
  }

  class NamedFieldSet[T : Bindable](name: String) extends AbsoluteFieldSet[T] {
    def extract(ft: Feature) = ft.getAttribute(name).asInstanceOf[T]

    def build(builder: org.geotools.feature.simple.SimpleFeatureBuilder, t: T): Unit =
      builder.set(name, t)

    def toSeq: Seq[Field] =
      Seq(implicitly[Bindable[T]].bind(name))
  }

  class CombinedAbsoluteFieldSet[T, U](
    exT: AbsoluteFieldSet[T],
    exU: AbsoluteFieldSet[U]
  ) extends AbsoluteFieldSet[T ~ U]{
    def extract(ft: Feature): (T ~ U) = {
      val t = exT.extract(ft)
      val u = exU.extract(ft)
      (new ~ (t, u))
    }

    def build(builder: org.geotools.feature.simple.SimpleFeatureBuilder, v: T ~ U): Unit = {
      val (t ~ u) = v
      exT.build(builder, t)
      exU.build(builder, u)
    }

    def toSeq: Seq[Field] = exT.toSeq ++ exU.toSeq
  }
  
  object Schema {
    def apply(name: String, fields: AbsoluteFieldSet[_]): Schema =
      apply(name, fields.toSeq)

    def apply(name: String, fields: Seq[Field]): Schema = {
      val builder = new org.geotools.feature.simple.SimpleFeatureTypeBuilder
      builder.setName(name)
      for (field <- fields) {
        builder.add(field)
      }
      builder.buildFeatureType()
    }
  }

  class RichSchema(schema: Schema) {
    def name: String = schema.getName.getLocalPart
    def fields: Seq[Field] =
      schema.getAttributeDescriptors.asScala

    def apply(values: Seq[Any]): Feature = {
      val builder = new org.geotools.feature.simple.SimpleFeatureBuilder(schema)
      for ((v, i) <- values.zipWithIndex)
        builder.set(i, v)
      builder.buildFeature(null)
    }

    def geometry = schema.getGeometryDescriptor
    def projection = schema.getGeometryDescriptor.getCoordinateReferenceSystem
    def get(name: String): Field = schema.getDescriptor(name)
    def get(index: Int): Field = schema.getDescriptor(index)
    def withName(name: String): Schema = Schema(name, fields)
    def feature(attributes: Seq[Any]): Feature =
      org.geoscript.feature.feature(schema, attributes)
  }

  object Feature {
    def unapplySeq(f: Feature): Option[Seq[Any]] = Some(f.getAttributes.asScala)
  }

  class RichFeature(feature: Feature) {
    def id: String = feature.getID
    def get[A](index: Int): A = feature.getAttribute(index).asInstanceOf[A]
    def get[A](key: String): A = feature.getAttribute(key).asInstanceOf[A]
    def geometry: org.geoscript.geometry.Geometry =
      feature.getDefaultGeometry.asInstanceOf[org.geoscript.geometry.Geometry]
    def schema: Schema = feature.getFeatureType
    def attributes: Seq[Any] =
      feature.getAttributes.asScala.toSeq
    def asMap: Map[String, Any] =
      (feature.getProperties.asScala.map { p =>
        val name = p.getName.getLocalPart
        (name, feature.getAttribute(name))
      })(collection.breakOut)
  }

  class RichField(field: Field) {
    def name: String = field.getName.getLocalPart
    def binding: Class[_] = field.getType.getBinding
  }

  class RichGeoField(field: GeoField) {
    def withProjection(proj: projection.Projection): GeoField = {
      val builder = new org.geotools.feature.AttributeTypeBuilder
      builder.init(field)
      builder.setCRS(proj)
      builder.buildDescriptor(field.getName, builder.buildGeometryType())
    }
  }

  trait Bindable[A] {
    def bind(name: String): Field
    def withDefaultName: Field
  }

  trait LowPriorityImplicits {
    implicit def bindAnything[A : Manifest]: Bindable[A] =
      new Bindable[A] {
        def bind(name: String): Field = {
          val builder = new org.geotools.feature.AttributeTypeBuilder
          builder.setName(name)
          builder.setBinding(manifest[A].erasure)
          builder.buildDescriptor(name, builder.buildType())
        }

        def withDefaultName = bind(implicitly[Manifest[A]].erasure.getSimpleName)
      }

    implicit def combinesDirectly[A,B]: TildeCombine[A,B,A ~ B] =
      new TildeCombine[A,B,A ~ B] {
        def apply(a: A, b: B): A ~ B = new ~ (a, b)
      }
  }
}
