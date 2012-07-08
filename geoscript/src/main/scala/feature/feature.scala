package org.geoscript

import org.opengis.feature.`type`.AttributeDescriptor
import scala.collection.JavaConverters._

package object feature {
  type Feature = org.opengis.feature.simple.SimpleFeature
  type Field = org.opengis.feature.`type`.AttributeDescriptor
  type GeoField = org.opengis.feature.`type`.GeometryDescriptor
  type Schema = org.opengis.feature.simple.SimpleFeatureType

  def bind[T : Manifest](name: String): Field = {
    val builder = new org.geotools.feature.AttributeTypeBuilder
    builder.setName(name)
    builder.setBinding(manifest[T].erasure)
    builder.buildDescriptor(name, builder.buildType())
  }

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
}

package feature {
  object Schema {
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
    def geometry = schema.getGeometryDescriptor
    def get(name: String): Field = schema.getDescriptor(name)
    def get(index: Int): Field = schema.getDescriptor(index)
    def withName(name: String): Schema = Schema(name, fields)
    def feature(attributes: Seq[Any]): Feature =
      org.geoscript.feature.feature(schema, attributes)
  }

  class RichFeature(feature: Feature) {
    def id: String = feature.getID
    def get[A](index: Int): A = feature.getAttribute(index).asInstanceOf[A]
    def get[A](key: String): A = feature.getAttribute(key).asInstanceOf[A]
    def geometry: org.geoscript.geometry.Geometry =
      feature.getDefaultGeometry.asInstanceOf[org.geoscript.geometry.Geometry]
  }

  class RichField(field: Field) {
    def name: String = field.getName.getLocalPart
    def binding: Class[_] = field.getType.getBinding
  }

  class RichGeoField(field: GeoField) {
    def withProjection(proj: projection.Projection): GeoField = sys.error("Unimplemented")
  }
}
