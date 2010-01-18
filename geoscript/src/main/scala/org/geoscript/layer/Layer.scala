package org.geoscript.layer

import java.io.File

import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}
import org.geotools.data.{DataStore, DataStoreFinder, DefaultQuery}
import org.geotools.data.shapefile.ShapefileDataStore
import org.geotools.feature.FeatureCollection
import org.geotools.referencing.CRS
import com.vividsolutions.jts.geom.Envelope

class Schema(wrapped: SimpleFeatureType) {
  def name = wrapped.getTypeName()
}

class RichFeatureCollection(
  wrapped: FeatureCollection[SimpleFeatureType, SimpleFeature]
) {
  def foreach(op: SimpleFeature => Unit) {
    val iter = wrapped.iterator()
    try {
      while (iter hasNext) { buff += op(iter.next) }
    } finally { wrapped.close(iter) }
  }

  def filter(pred: SimpleFeature => Boolean): Iterable[SimpleFeature] = {
    val buff = new collection.mutable.ArrayBuffer[SimpleFeature]()
    
    val iter = wrapped.iterator()
    try {
      while (iter hasNext) {
        val f = iter.next
        if (pred(f)) { buff += f }
      }
    } finally { wrapped.close(iter) }

    buff.toArray
  }

  def find(pred: SimpleFeature => Boolean): Option[SimpleFeature] = {
    val iter = wrapped.iterator()
    try {
      while (iter hasNext) {
        val f = iter.next
        if (pred(f)) return Some(f)
      }
      None
    } finally { wrapped.close(iter) }
  }

  def map[A](op: SimpleFeature => A): Iterable[A] = {
    val buff = new collection.mutable.ArrayBuffer[A]()
    
    val iter = wrapped.iterator()
    try {
      while (iter hasNext) { buff += op(iter.next) }
    } finally { wrapped.close(iter) }

    buff.toArray
  }
}

class Layer(val name: String, store: DataStore) {
  private def source = store.getFeatureSource(name)

  def schema: Schema = new Schema(store.getSchema(name))

  def features: RichFeatureCollection = {
    new RichFeatureCollection(source.getFeatures)
  }

  def count: Int = source.getCount(new DefaultQuery())

  def bounds: (Double, Double, Double, Double, String) = {
    val bbox = source.getBounds()
    val crs = CRS.lookupEpsgCode(bbox.getCoordinateReferenceSystem(), true)
    (bbox.getMinX, bbox.getMinY, bbox.getMaxX, bbox.getMaxY, "EPSG:" + crs)
  }
}

object Shapefile {
  def apply(path: String): Layer = {
    val ds = new ShapefileDataStore(new File(path).toURI.toURL)
    val name = ds.getTypeNames()(0)
    new Layer(name, ds)
  }
}
