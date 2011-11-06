package org.geoscript.feature

import com.vividsolutions.jts.{geom => jts}
import org.geoscript.geometry._
import org.geoscript.projection._
import org.{geotools => gt}
import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}
import org.opengis.feature.`type`.{AttributeDescriptor, GeometryDescriptor}

/**
 * A Schema enumerates the types and names of the properties of records in a
 * particular dataset.  For example, a Schema for a road dataset might have
 * fields like "name", "num_lanes", "the_geom".
 */
trait Schema {
  /**
   * The name of the dataset itself.  This is not a property of the data records.
   */
  def name: String

  /**
   * The geometry field for this layer, regardless of its name.
   */
  def geometry: GeoField

  /**
   * All fields in an iterable sequence.
   */
  def fields: Seq[Field]

  /**
   * The names of all fields in an iterable sequence.
   */
  def fieldNames: Seq[String] = fields map { _.name } 

  /**
   * Retrieve a field by name.
   */
  def get(fieldName: String): Field

  /**
   * Create an instance of a feature according to this Schema, erroring if:
   * <ul>
   *   <li> any values are omitted </li>
   *   <li> any values are the wrong type </li>
   *   <li> any extra values are present </li>
   * </ul>
   */
  def create(data: (String, AnyRef)*): Feature = {
    if (
      data.length != fields.length ||
      data.exists { case (key, value) => !get(key).gtBinding.isInstance(value) }
    ) {
      throw new RuntimeException(
        "Can't create feature; properties are: %s, but fields require %s.".format(
          data.mkString, fields.mkString
        )
      )
    }
    Feature(data: _*)
  }

  override def toString: String = {
    "<Schema name: %s, fields: %s>".format(
      name,
      fields.mkString("[", ", ", "]")
    )
  }
}

/**
 * A companion object for Schema that provides various ways of creating Schema
 * instances.
 */
object Schema {
  def apply(wrapped: SimpleFeatureType) = {
    new Schema {
      def name = wrapped.getTypeName()
      def geometry = Field(wrapped.getGeometryDescriptor())

      def fields: Seq[Field] = { 
        var buffer = new collection.mutable.ArrayBuffer[Field]
        val descriptors = wrapped.getAttributeDescriptors().iterator()
        while (descriptors hasNext) { buffer += Field(descriptors.next) }
        buffer.toSeq
      }

      def get(fieldName: String) = Field(wrapped.getDescriptor(fieldName))
    }
  }

  def apply(n: String, f: Field*): Schema = apply(n, f.toSeq)

  def apply(n: String, f: Iterable[Field]): Schema = {
    new Schema {
      def name = n
      def geometry = 
        f.find(_.isInstanceOf[GeoField])
         .getOrElse(null)
         .asInstanceOf[GeoField]

      def fields = f.toSeq
      def get(fieldName: String) = f.find(_.name == fieldName).get
    }
  }
}

/**
 * A Field represents a particular named, typed property in a Schema.
 */
trait Field {
  def name: String
  def gtBinding: Class[_]
  override def toString = "%s: %s".format(name, gtBinding.getSimpleName)
}

/**
 * A Field that represents a Geometry. GeoFields add projection information to
 * normal fields.
 */
trait GeoField extends Field {
  override def gtBinding: Class[_]
  /**
   * The Projection used for this field's geometry.
   */
  def projection: Projection

  def copy(projection: Projection): GeoField = {
    val n = name
    val gb = gtBinding
    val p = projection

    new GeoField {
      val name = n
      override val gtBinding = gb
      val projection = p
    }
  }

  override def toString = "%s: %s [%s]".format(name, gtBinding.getSimpleName, projection)
}

/**
 * A companion object providing various methods of creating Field instances.
 */
object Field {
  /**
   * Create a GeoField by wrapping an OpenGIS GeometryDescriptor
   */
  def apply(wrapped: GeometryDescriptor): GeoField = 
    new GeoField {
      def name = wrapped.getLocalName
      override def gtBinding = wrapped.getType.getBinding
      def projection = Projection(wrapped.getCoordinateReferenceSystem())
    }

  /**
   * Create a Field by wrapping an OpenGIS AttributeDescriptor
   */
  def apply(wrapped: AttributeDescriptor): Field = {
    wrapped match {
      case geom: GeometryDescriptor => apply(geom)
      case wrapped => 
        new Field {
          def name = wrapped.getLocalName
          def gtBinding = wrapped.getType.getBinding
        }
    }
  }

  def apply[G : BoundGeometry](n: String, b: Class[G], p: Projection): GeoField =
    new GeoField {
      def name = n
      def gtBinding = implicitly[BoundGeometry[G]].binding
      def projection = p
    }

  def apply[S : BoundScalar](n: String, b: Class[S]): Field =
    new Field {
      def name = n
      def gtBinding = implicitly[BoundScalar[S]].binding
    }
}

/**
 * A Feature represents a record in a geospatial data set.  It should generally
 * identify a single "thing" such as a landmark or observation.
 */
trait Feature {
  /**
   * An identifier for this feature in the dataset.
   */
  def id: String
  
  /**
   * Retrieve a property of the feature, with an expected type. Typical usage is:
   * <pre>
   * val name = feature.get[String]("name")
   * </pre>
   */
  def get[A](key: String): A

  /**
   * Get the geometry for this feature.  This allows you to access the geometry
   * without worrying about its property name.
   */
  def geometry: Geometry

  /**
   * Get all properties for this feature as a Map.
   */
  def properties: Map[String, Any]

  def update(data: (String, Any)*): Feature = update(data.toSeq)

  def update(data: Iterable[(String, Any)]): Feature = {
    val props = properties
    assert(data.forall { x => props contains x._1 })
    Feature(props ++ data)
  }

  /**
   * Write the values in this Feature to a particular OGC Feature object.
   */
  def writeTo(feature: org.opengis.feature.simple.SimpleFeature) {
    for ((k, v) <- properties) feature.setAttribute(k, v) 
  }

  override def toString: String = 
    properties map {
      case (key, value: jts.Geometry) => 
        "%s: <%s>".format(key, value.getGeometryType())
      case (key, value) => 
        "%s: %s".format(key, value)
    } mkString("<Feature ", ", ", ">")
}

/**
 * A companion object for Feature providing several methods for creating
 * Feature instances.
 */
object Feature {
  /**
   * Create a GeoScript feature by wrapping a GeoAPI feature instance.
   */
  def apply(wrapped: SimpleFeature): Feature = {
    new Feature {
      def id: String = wrapped.getID

      def get[A](key: String): A = 
        wrapped.getAttribute(key).asInstanceOf[A]

      def geometry: Geometry = 
        wrapped.getDefaultGeometry().asInstanceOf[Geometry]

      def properties: Map[String, Any] = {
        Map((0 until wrapped.getAttributeCount) map { i => 
          val key = wrapped.getType().getDescriptor(i).getLocalName
          ( key, get(key) )
        }: _*)
      }
    }
  }

  def apply(props: (String, Any)*): Feature = apply(props)

  /**
   * Create a feature from name/value pairs.  Example usage looks like:
   * <pre>
   * val feature = Feature("geom" -&gt; Point(12, 37), "type" -&gt; "radio tower")
   * </pre>
   */
  def apply(props: Iterable[(String, Any)]): Feature = {
    new Feature {
      def id: String = null

      def geometry = 
        props.collectFirst({ 
          case (name, geom: Geometry) => geom
        }).get

      def get[A](key: String): A = 
        props.find(_._1 == key).map(_._2.asInstanceOf[A]).get

      def properties: Map[String, Any] = Map(props.toSeq: _*)
    }
  }
}

/**
 * A collection of features, possibly not all loaded yet.  For example, queries
 * against Layers produce feature collections, but the query may not actually
 * be sent until you access the contents of the collection.
 *
 * End users will generally not need to create FeatureCollections directly.
 */
class FeatureCollection(
  wrapped: gt.data.FeatureSource[SimpleFeatureType, SimpleFeature],
  query: gt.data.Query
) extends Traversable[Feature] {
  override def foreach[U](op: Feature => U) {
    val iter = wrapped.getFeatures().features()
    try
      while (iter hasNext) op(Feature(iter.next))
    finally
      iter.close()
  }
}
