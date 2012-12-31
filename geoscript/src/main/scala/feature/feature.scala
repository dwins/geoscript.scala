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

  def Field(name: String, binding: Class[_]): Field = {
    val builder = new org.geotools.feature.AttributeTypeBuilder
    builder.setName(name)
    builder.setBinding(binding)
    builder.buildDescriptor(name, builder.buildType())
  }

  def GeoField(name: String, proj: projection.Projection, binding: Class[_]): GeoField = {
    val builder = new org.geotools.feature.AttributeTypeBuilder
    builder.setName(name)
    builder.setBinding(binding)
    builder.setCRS(proj)
    builder.buildDescriptor(name, builder.buildGeometryType)
  }

  def setSchemaName(name: String, schema: Schema): Schema = {
    val builder = new org.geotools.feature.simple.SimpleFeatureTypeBuilder
    builder.init(schema)
    builder.setName(name)
    builder.buildFeatureType
  }

  def fromAttributes(attributes: (String, Any)*): Feature = {
    val fields = attributes.map { case (n, v) => Field(n, v.getClass) }
    val schema = Schema("internal", fields)
    val builder = new org.geotools.feature.simple.SimpleFeatureBuilder(schema)
    for ((key, value) <- attributes) builder.set(key, value)
    builder.buildFeature(null)
  }

  def named[A : Bindable](name: String): AbsoluteFieldSet[A] = 
    new NamedFieldSet(name)

  implicit def bindingSugarForString(name: String) =
    new {
      def binds[T : Bindable] = named[T](name)
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

  sealed trait AbsoluteFieldSet[T] {
    def apply(t: T): Feature = {
      val schema = Schema("builder", this)
      val builder = new org.geotools.feature.simple.SimpleFeatureBuilder(schema)
      build(builder, t)
      builder.buildFeature(null)
    }
    def build(builder: org.geotools.feature.simple.SimpleFeatureBuilder, t: T): Unit
    def extract(xs: Feature): T 
    def unapply(xs: Feature): Some[T] = Some(extract(xs))

    def ~[U](that: AbsoluteFieldSet[U]): AbsoluteFieldSet[T ~ U] =
      new CombinedAbsoluteFieldSet[T, U](this, that)

    def toSeq: Seq[Field]
  }

  object AbsoluteFieldSet {
    import projection._
    implicit def geometryFieldsHaveProjection[G](implicit ev: G <:< Geometry): HasProjection[AbsoluteFieldSet[G]] =
      new HasProjection[AbsoluteFieldSet[G]] {
        def setProjection(p: Projection)(x: AbsoluteFieldSet[G]): AbsoluteFieldSet[G] = 
          new AbsoluteFieldSet[G] {
            def build(builder: org.geotools.feature.simple.SimpleFeatureBuilder, g: G) =
              x.build(builder, g)

            def extract(xs: Feature): G = x.extract(xs)

            def toSeq: Seq[Field] = x.toSeq.map {
              case (gf: GeoField) => force(p, gf)
              case _ => sys.error("Trying to set projection on aspatial field")
            }
          }

        def transform(p: Projection)(x: AbsoluteFieldSet[G]): AbsoluteFieldSet[G] =
          setProjection(p)(x)
      }
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
    def feature(attributes: Seq[Any]): Feature = {
      import org.geotools.feature.simple.SimpleFeatureBuilder
      val builder = new SimpleFeatureBuilder(schema)
      for ((value, idx) <- attributes.zipWithIndex)
        builder.set(idx, value)
      builder.buildFeature(null)
    }
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
    def crs: projection.Projection = field.getType.getCoordinateReferenceSystem
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
