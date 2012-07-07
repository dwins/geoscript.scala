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

  def fromAttributes(attributes: (String, Any)*): Feature =
   sys.error("Unimplemented")

  def feature(schema: Schema, attributes: Seq[Any]): Feature = {
    import org.geotools.feature.simple.SimpleFeatureBuilder
    val builder = new SimpleFeatureBuilder(schema)
    for ((value, idx) <- attributes.zipWithIndex)
      builder.set(idx, value)
    builder.buildFeature(null)
  }

  def widen(a: Seq[Field], b: Seq[Field]): Seq[Field] = sys.error("Unimplemented")
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
    def withName(name: String): Schema = sys.error("Unimplemented")
    def feature(attributes: Seq[Any]): Feature =
      org.geoscript.feature.feature(schema, attributes)
  }

  class RichFeature(feature: Feature) {
    def id: String = sys.error("unimplemented")
    def get[A](index: Int): A = sys.error("Unimplemented")
    def get[A](key: String): A = sys.error("Unimplemented")
    def geometry: org.geoscript.geometry.Geometry = sys.error("Unimplemented")
  }

  class RichField(field: Field) {
    def name: String = field.getName.getLocalPart
    def binding: Class[_] = field.getType.getBinding
  }

  class RichGeoField(field: GeoField) {
    def withProjection(proj: projection.Projection): GeoField = sys.error("Unimplemented")
  }
}
