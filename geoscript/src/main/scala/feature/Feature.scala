package org.geoscript //.feature

import org.geoscript.projection._
import scala.collection.JavaConverters._

/**
 * Facilities for manipulating vector data.
 */
package object feature {
  /**
   * A Feature is a single entry in a geospatial dataset.  For example, a
   * Feature might represent a single row in a relational database. Fields of a
   * feature are not known at compile time but a runtime representation of the
   * schema is available.
   */
  type Feature = org.opengis.feature.simple.SimpleFeature

  /**
   * A FeatureCollection represents a (possiby lazy) collection of Features, all
   * of which have the same schema..
   */
  type FeatureCollection = org.geotools.feature.FeatureCollection[Schema, Feature]

  /**
   * A Schema is a runtime representation of the constraints on values that a
   * Feature may have, including but not limited to the name, types, and order
   * of the fields that appear in the Feature.
   */
  type Schema = org.opengis.feature.simple.SimpleFeatureType

  /**
   * A Field is a runtime representation of the acceptable values for a field in
   * a feature.
   * @note Geometric fields should use [[org.geoscript.feature.GeoField]]
   * instead, which can preserve projection information.
   */
  type Field = org.opengis.feature.`type`.AttributeDescriptor

  /**
   * A GeoField is a runtime representation of the acceptable values for a
   * field, including the projection information.
   */
  type GeoField = org.opengis.feature.`type`.GeometryDescriptor

  /**
   * A schema factory with default configuration.
   * @see [[org.geoscript.feature.SchemaBuilder]]
   */
  val schemaFactory: org.opengis.feature.`type`.FeatureTypeFactory =
    new org.geotools.feature.`type`.FeatureTypeFactoryImpl

  /**
   * A feature factory with default configuration.
   * @see [[org.geoscript.feature.builder]]
   */
  val featureFactory: org.opengis.feature.FeatureFactory =
    org.geotools.factory.CommonFactoryFinder.getFeatureFactory(null)

  /**
   * An object with convenience methods for manipulating schemas.  This includes 
   * extractors for performing pattern matching against a schema.
   */
  val schemaBuilder = new SchemaBuilder(schemaFactory)

  implicit class RichSchema(val schema: Schema) extends AnyVal {
    def name: String = schema.getName.getLocalPart
    def fields: Seq[Field] = schema.getAttributeDescriptors.asScala
    def field(name: String): Field = schema.getDescriptor(name)
    def geometryField: GeoField  = schema.getGeometryDescriptor
    def mkFeature(attributes: AnyRef*)(id: String = "", factory: org.opengis.feature.FeatureFactory = featureFactory) =
      factory.createSimpleFeature(attributes.toArray, schema, id)
  }

  implicit class RichField(val field: Field) extends AnyVal {
    def name: String = field.getName.getLocalPart
    def binding: Class[_] = field.getType.getBinding
  }

  implicit class RichGeoField(val field: GeoField) extends AnyVal{
    def projection: org.geoscript.projection.Projection = 
      field.getType.getCoordinateReferenceSystem
  }

  implicit class RichFeature(val feature: Feature) extends AnyVal {
    def attributes: Map[String, Any] = {
      val kvPairs = 
        for (p <- feature.getProperties.asScala)
        yield (p.getName.getLocalPart, p.getValue)
      kvPairs.toMap
    }
    def attributes_= (values: Iterable[(String, Any)]) =
      for ((k, v) <- values) feature.setAttribute(k, v)
    def id: String = feature.getID
    def geometry: org.geoscript.geometry.Geometry =
      feature.getDefaultGeometry.asInstanceOf[org.geoscript.geometry.Geometry]
    def geometry_=(g: org.geoscript.geometry.Geometry): Unit =
      feature.setDefaultGeometry(g)
    def get[T](name: String) = feature.getAttribute(name).asInstanceOf[T]
  }

  implicit class RichFeatureCollection(val collection: FeatureCollection)
  extends Traversable[Feature]
  {
    def foreach[U](f: Feature => U): Unit = {
      val iter = collection.features
      try
        while (iter.hasNext) f(iter.next)
      finally
        iter.close()
    }
  }
}

package feature {
  class SchemaBuilder(factory: org.opengis.feature.`type`.FeatureTypeFactory) {
    implicit object GeoFieldHasProjection extends HasProjection[GeoField] {
      def reproject(t: GeoField, projection: Projection): GeoField =
        GeoField(t.name, t.binding, projection)
    }

    implicit object SchemaHasProjection extends HasProjection[Schema] {
      def reproject(t: Schema, projection: Projection): Schema =
        t.copy(fields = t.fields map {
          case (g: GeoField) => org.geoscript.projection.reproject(g, projection)
          case other => other
        })
    }

    implicit class FieldModifiers(val field: Field) {
      def copy(
        name: String = field.name,
        binding: Class[_] = field.binding)
      : Field = Field(name, binding)
    }

    implicit class GeoFieldModifiers(val field: GeoField) {
      def copy(
        name: String = field.name,
        binding: Class[_] = field.binding,
        projection: Projection = field.projection)
      : Field = GeoField(name, binding, projection)
    }

    implicit class SchemaModifiers(val schema: Schema) {
      def copy(
        name: String = schema.name,
        fields: Seq[Field] = schema.fields)
      : Schema = Schema(name, fields)
    }

    object Field {
      def apply(name: String, binding: Class[_]): Field = {
        val qname = new org.geotools.feature.NameImpl(name)
        val attType = factory.createAttributeType(
          qname, // type name
          binding, // java class binding
          false, // is identifiable?
          false, // is abstract?
          java.util.Collections.emptyList(), // list of filters for value constraints
          null, // supertype
          null) // internationalized string for title
        factory.createAttributeDescriptor(
          attType,
          qname,
          1, // minoccurs
          1, // maxoccurs
          true, // isNillable
          null) // default value
      }
      def unapply(field: Field): Some[(String, Class[_])] =
        Some((field.name, field.binding))
    }

    object GeoField {
      def apply(
        name: String, binding: Class[_], projection: Projection)
      : GeoField = {
        val qname = new org.geotools.feature.NameImpl(name)
        val attType = factory.createGeometryType(
          qname, // type name
          binding, // java class binding
          projection, // coordinate reference system
          false, // is this type identifiable?
          false, // is this type abstract?
          java.util.Collections.emptyList(), // list of filters for value constraints
          null, // supertype
          null) // internationalized string for title
        factory.createGeometryDescriptor(
          attType, // attribute type
          qname, // qualified name
          1, // minoccurs
          1, // maxoccurs
          true, // isNillable
          null) // default value
      }

      def unapply(field: GeoField): Some[(String, Class[_], Projection)] =
        Some((field.name, field.binding, field.projection))
    }

    object Schema {
      def apply(name: String, fields: Seq[Field]): Schema = {
        val qname = new org.geotools.feature.NameImpl(name)
        factory.createSimpleFeatureType(
          qname, // qualified name
          fields.asJava, // fields (order matters)
          fields.collectFirst { case (g: GeoField) => g }.orNull, // default geometry field
          false, // is this schema abstract?
          Seq.empty[org.geoscript.filter.Filter].asJava, // list of filters defining runtime constraints
          null, // supertype
          null  // internationalized description
        )
      }
      def unapply(schema: Schema): Some[(String, Seq[Field])] = 
        Some((schema.name, schema.fields))
    }
  }
}
