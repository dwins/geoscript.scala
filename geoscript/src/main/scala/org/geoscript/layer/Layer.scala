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
  def apply(fieldName: String) = 
    new Field(wrapped.getDescriptor(fieldName))
}

class Field(wrapped: AttributeDescriptor) {
  def name = wrapped.getLocalName
  def binding = wrapped.getType.getBinding
}

class RichFeatureCollection(
  wrapped: gt.feature.FeatureCollection[SimpleFeatureType, SimpleFeature]
) extends Iterable[SimpleFeature] {
  override def elements = {
    val raw = wrapped.iterator()
    val rawIter = new Iterator[SimpleFeature] {
      def hasNext = raw.hasNext
      def next = raw.next
    }

    new ClosingIterator(rawIter) {
      def close() { wrapped.close(raw) }
    }
  }
}

class Layer(val name: String, store: gt.data.DataStore) {
  private def source = store.getFeatureSource(name)

  def workspace: Workspace = new Workspace(store)

  def schema: Schema = new Schema(store.getSchema(name))

  def features: RichFeatureCollection = {
    new RichFeatureCollection(source.getFeatures)
  }

  def count: Int = source.getCount(new gt.data.DefaultQuery())

  def bounds: (Double, Double, Double, Double, String) = {
    val bbox = source.getBounds()
    val crs = gt.referencing.CRS.lookupEpsgCode(
      bbox.getCoordinateReferenceSystem(), true
    )
    (bbox.getMinX, bbox.getMinY, bbox.getMaxX, bbox.getMaxY, "EPSG:" + crs)
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
