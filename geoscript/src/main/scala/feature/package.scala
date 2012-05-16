package org.geoscript

import org.opengis.feature.`type`.AttributeDescriptor
import scala.collection.JavaConverters._

package object feature {
  type Schema = org.opengis.feature.simple.SimpleFeatureType
  type Field = org.opengis.feature.`type`.AttributeDescriptor
  type GeoField = org.opengis.feature.`type`.GeometryDescriptor

  def bind[T : Manifest](name: String): Track[Field, T] =
    sys.error("Unimplemented")

  def bind[T <: geometry.Geometry : Manifest]
    (name: String, proj: projection.Projection): Track[GeoField, G] = sys.error("Unimplemented")
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
  }

  class RichField(field: Field) {
    def name: String = field.getName.getLocalPart
    def binding: Class[_] = field.getType.getBinding
  }

  class RichGeoField(field: GeoField) {
    def withProjection(proj: projection.Projection): GeoField = sys.error("Unimplemented")
  }
}
