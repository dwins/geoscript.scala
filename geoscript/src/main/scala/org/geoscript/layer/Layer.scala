package org.geoscript.layer

import java.io.File

import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}
import org.opengis.feature.`type`.AttributeDescriptor
import org.{geotools => gt}
import com.vividsolutions.jts.geom.Envelope

import org.geoscript.util.ClosingIterator
import org.geoscript.workspace.Workspace

class Schema(wrapped: SimpleFeatureType) {
  def name = wrapped.getTypeName()

  def fields: Seq[Field] = { 
    var buffer = new collection.mutable.ArrayBuffer[Field]
    val descriptors = wrapped.getAttributeDescriptors().iterator()
    while (descriptors hasNext) { buffer += Field(descriptors.next) }
    buffer.toSeq
  }

  def apply(fieldName: String) = Field(wrapped.getDescriptor(fieldName))
}

trait Field {
  def name: String
  def binding: Class[_]
}

trait Feature {
  def id: String
  def apply[A](key: String): A
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
      def apply[A](key: String): A = wrapped.getAttribute(key).asInstanceOf[A]
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

      def apply[A](key: String): A = 
        props.find(_._1 == key).map(_._2.asInstanceOf[A]).get

      def properties: Map[String, Any] = Map(props: _*)
    }
  }
}

class FeatureCollection(
  wrapped: gt.data.FeatureSource[SimpleFeatureType, SimpleFeature]
) extends Iterable[SimpleFeature] {
  override def elements = {
    val collection = wrapped.getFeatures()
    val raw = collection.iterator()
    val rawIter = new Iterator[SimpleFeature] {
      def hasNext = raw.hasNext
      def next = raw.next
    }

    new ClosingIterator(rawIter) {
      def close() { collection.close(raw) }
    }
  }
}

class Layer(val name: String, store: gt.data.DataStore) {
  private def source = store.getFeatureSource(name)

  def workspace: Workspace = new Workspace(store)

  def schema: Schema = new Schema(store.getSchema(name))

  def features: FeatureCollection = { new FeatureCollection(source) }

  def count: Int = source.getCount(new gt.data.DefaultQuery())

  def bounds: (Double, Double, Double, Double, String) = {
    val bbox = source.getBounds()
    val crs = gt.referencing.CRS.lookupEpsgCode(
      bbox.getCoordinateReferenceSystem(), true
    )
    (bbox.getMinX, bbox.getMinY, bbox.getMaxX, bbox.getMaxY, "EPSG:" + crs)
  }

  def += (f: Feature) {
    val tx = new gt.data.DefaultTransaction
    val writer = store.getFeatureWriterAppend(name, tx)
    val toBeWritten = writer.next()
    for ((key, value) <- f.properties) toBeWritten.setAttribute(key, value)
    writer.write()
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
