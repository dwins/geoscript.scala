package org.geoscript.layer

import java.io.File

import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}
import org.opengis.feature.`type`.{AttributeDescriptor, GeometryDescriptor}
import org.{geotools => gt}
import com.vividsolutions.jts.{geom => jts}
import com.vividsolutions.jts.geom.Envelope

import org.geoscript.geometry.Geometry
import org.geoscript.projection.Projection
import org.geoscript.util.ClosingIterator
import org.geoscript.workspace.{Directory,Workspace}

trait Schema {
  def name: String
  def fields: Seq[Field]
  def fieldNames: Seq[String] = fields map { _.name } 
  def get(fieldName: String): Field
  override def toString: String = {
    "<Schema name: %s, fields: %s>".format(
      name,
      fields.mkString("[", ", ", "]")
    )
  }
}

object Schema{
  def apply(wrapped: SimpleFeatureType) = {
    new Schema {
      def name = wrapped.getTypeName()

      def fields: Seq[Field] = { 
        var buffer = new collection.mutable.ArrayBuffer[Field]
        val descriptors = wrapped.getAttributeDescriptors().iterator()
        while (descriptors hasNext) { buffer += Field(descriptors.next) }
        buffer.toSeq
      }

      def get(fieldName: String) = Field(wrapped.getDescriptor(fieldName))
    }
  }

  def apply(n: String, f: Field*) = {
    new Schema {
      def name = name
      def fields = f
      def get(fieldName: String) = f.find(_.name == fieldName).get
    }
  }
}

trait Field {
  def name: String
  def binding: Class[_]
  def projection: Projection
  def gtBinding: Class[_] = binding
  override def toString = "%s: %s".format(name, binding.getSimpleName)
}

trait Feature {
  def id: String
  def get[A](key: String): A
  def geometry: Geometry
  def properties: Map[String, Any]
  override def toString: String = 
    properties map {
      case (key, value: jts.Geometry) => 
        "%s: <%s>".format(key, value.getGeometryType())
      case (key, value) => 
        "%s: %s".format(key, value)
    } mkString("<Feature ", ", ", ">")
}

object Field {
  def apply(wrapped: AttributeDescriptor) = {
    wrapped match {
      case geom: GeometryDescriptor => new Field {
        def name = geom.getLocalName
        def binding =
          Geometry.wrapperClass(
            geom.getType.getBinding.asInstanceOf[Class[jts.Geometry]]
          )
        def projection = Projection(geom.getCoordinateReferenceSystem())
        override def gtBinding = geom.getType.getBinding
      }
      case wrapped => 
        new Field {
          def name = wrapped.getLocalName
          def binding = wrapped.getType.getBinding
          def projection = null
        }
    }
  }

  def apply(n: String, b: Class[_ <: Geometry], p: Projection) = 
    new Field {
      def name = n
      def binding = b
      def projection = p
      override def gtBinding = Geometry.jtsClass(b)
    }

  def apply(n: String, b: Class[_]) = {
    new Field {
      def name = n
      def binding = b
      def projection = null
    }
  }
}

object Feature {
  def apply(wrapped: SimpleFeature): Feature = {
    new Feature {
      def id: String = wrapped.getID

      def get[A](key: String): A = 
        wrapped.getAttribute(key) match {
          case geom: jts.Geometry => Geometry(geom).asInstanceOf[A]
          case x => x.asInstanceOf[A]
        }

      def geometry: Geometry = 
        Geometry(
          wrapped.getDefaultGeometry()
            .asInstanceOf[com.vividsolutions.jts.geom.Geometry]
        )

      def properties: Map[String, Any] = {
        Map((0 until wrapped.getAttributeCount) map { i => 
          val key = wrapped.getType().getDescriptor(i).getLocalName
          ( key, get(key) )
        }: _*)
      }
    }
  }

  def apply(props: (String, Any)*): Feature = {
    new Feature {
      def id: String = null

      def geometry = 
        Geometry(
          props
            .find(_._1.isInstanceOf[com.vividsolutions.jts.geom.Geometry])
            .map(_._2).get.asInstanceOf[com.vividsolutions.jts.geom.Geometry]
        )

      def get[A](key: String): A = 
        props.find(_._1 == key).map(_._2.asInstanceOf[A]).get

      def properties: Map[String, Any] = Map(props: _*)
    }
  }
}

class FeatureCollection(
  wrapped: gt.data.FeatureSource[SimpleFeatureType, SimpleFeature],
  query: gt.data.Query
) extends Iterable[Feature] {

  override def elements = {
    val collection = wrapped.getFeatures()
    val raw = collection.iterator()
    val rawIter = new Iterator[Feature] {
      def hasNext = raw.hasNext
      def next = Feature(raw.next)
    }

    new ClosingIterator(rawIter) {
      def close() { collection.close(raw) }
    }
  }
}

trait Layer {
  val name: String
  def store: gt.data.DataStore
  def workspace: Workspace

  private def source = store.getFeatureSource(name)

  def schema: Schema = Schema(store.getSchema(name))

  def features: FeatureCollection = {
    new FeatureCollection(source, new gt.data.DefaultQuery()) 
  }
  
  def filter(pred: org.opengis.filter.Filter): FeatureCollection = {
    new FeatureCollection(source, new gt.data.DefaultQuery(name, pred)) 
  }

  def count: Int = source.getCount(new gt.data.DefaultQuery())

  def bounds: (Double, Double, Double, Double, String) = {
    val bbox = source.getBounds()
    val crs = gt.referencing.CRS.lookupEpsgCode(
      bbox.getCoordinateReferenceSystem(), true
    )
    (bbox.getMinX, bbox.getMinY, bbox.getMaxX, bbox.getMaxY, "EPSG:" + crs)
  }

  def += (f: Feature) { this ++= Seq.singleton(f) }

  def ++= (features: Iterable[Feature]) {
    val tx = new gt.data.DefaultTransaction
    val writer = store.getFeatureWriterAppend(name, tx)

    for (f <- features) {
      val toBeWritten = writer.next()
      for ((key, value) <- f.properties) toBeWritten.setAttribute(key, value)
      writer.write()
    }

    writer.close()
    tx.commit()
    tx.close()
  }

  override def toString: String = 
    "<Layer name: %s, count: %d>".format(name, count)
}

object Shapefile {
  private def basename(f: File) = f.getName().replaceFirst("\\.[^.]+$", "")
    
  def apply(path: String): Layer = apply(new File(path))

  def apply(file: File): Layer = {
    val ws = Directory(file.getParent())
    ws.layer(basename(file))
  }
}
