package org.geoscript.layer

import java.io.File

import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}
import org.geotools.data.{DataStore, DataStoreFinder, DefaultQuery}
import org.geotools.data.shapefile.ShapefileDataStore
import org.geotools.feature.FeatureCollection
import org.geotools.referencing.CRS
import com.vividsolutions.jts.geom.Envelope

class RichFeatureCollection(
  wrapped: FeatureCollection[SimpleFeatureType, SimpleFeature]
) {
  def map[A](op: SimpleFeature => A): Iterable[A] = {
    val iter = wrapped.iterator()
    Stream.fromIterator(
      new Iterator[A] {
        var open = true

        override def next = 
          try {
            op(iter.next )
          } catch {
            case ex => wrapped.close(iter); throw ex
          }

        override def hasNext = 
          if (iter.hasNext) true
          else {
            if (open) wrapped.close(iter)
            open = false
            false
          }
      }
    )
  }
}

class Layer(val name: String, store: DataStore) {
  private def source = store.getFeatureSource(name)

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
