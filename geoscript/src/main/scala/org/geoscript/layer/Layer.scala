package org.geoscript.layer

import java.io.File

import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}
import org.opengis.feature.`type`.AttributeDescriptor
import org.{geotools => gt}
import com.vividsolutions.jts.geom.Envelope

import org.geoscript.geometry.Geometry
import org.geoscript.util.ClosingIterator
import org.geoscript.workspace.Workspace

trait Schema {
  def name: String
  def fields: Seq[Field]
  def apply(fieldName: String): Field
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

      def apply(fieldName: String) = Field(wrapped.getDescriptor(fieldName))
    }
  }

  def apply(n: String, f: Field*) = {
    new Schema {
      def name = name
      def fields = f
      def apply(fieldName: String) = f.find(_.name == fieldName).get
    }
  }
}

trait Field {
  def name: String
  def binding: Class[_]
}

trait Feature {
  def id: String
  def get[A](key: String): A
  def geometry: Geometry
  def properties: Map[String, Any]
}

object Field {
  def apply(wrapped: AttributeDescriptor) = {
    new Field {
      def name = wrapped.getLocalName
      def binding = wrapped.getType.getBinding
    }
  }

  def apply(n: String, b: Class[_]) = {
    new Field {
      def name = n
      def binding = b
    }
  }
}

object Feature {
  def apply(wrapped: SimpleFeature): Feature = {
    new Feature {
      def id: String = wrapped.getID

      def get[A](key: String): A = wrapped.getAttribute(key).asInstanceOf[A]

      def geometry: Geometry = 
        Geometry(
          wrapped.getDefaultGeometry()
            .asInstanceOf[com.vividsolutions.jts.geom.Geometry]
        )

      def properties: Map[String, Any] = {
        val m = collection.mutable.Map[String, Any]()
        var i = 0
        while (i < wrapped.getAttributeCount) {
          m.update(
            wrapped.getType().getDescriptor(i).getLocalName,
            wrapped.getAttribute(i)
          )
        }
        m.readOnly.asInstanceOf[Map[String,Any]]
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

class Layer(val name: String, store: gt.data.DataStore) {
  private def source = store.getFeatureSource(name)

  def workspace: Workspace = new Workspace(store)

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
}

object Shapefile {
  def apply(path: String): Layer = {
    val ds = new gt.data.shapefile.ShapefileDataStore(
      new File(path).toURI.toURL
    )
    val name = ds.getTypeNames()(0)
    new Layer(name, ds)
  }
}
