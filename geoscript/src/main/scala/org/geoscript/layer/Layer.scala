package org.geoscript.layer

import java.io.File

import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}
import org.opengis.feature.`type`.{AttributeDescriptor, GeometryDescriptor}
import org.{geotools => gt}
import com.vividsolutions.jts.{geom => jts}
import com.vividsolutions.jts.geom.Envelope

import org.geoscript.feature._
import org.geoscript.geometry._
import org.geoscript.projection._
import org.geoscript.util.ClosingIterator
import org.geoscript.workspace.{Directory,Workspace}

/**
 * A Layer represents a geospatial dataset.
 */
trait Layer {
  /**
   * The name of this data set
   */
  val name: String

  /**
   * The GeoTools datastore that is wrapped by this Layer.
   */
  def store: gt.data.DataStore
  
  /**
   * The workspace containing this layer.
   */
  def workspace: Workspace

  /** 
   * Retrieve a GeoTools feature source for this layer.
   */
  private def source = store.getFeatureSource(name)

  /** 
   * The Schema describing this layer's contents.
   */
  def schema: Schema = Schema(store.getSchema(name))

  /**
   * Get a feature collection that supports the typical Scala collection
   * operations.
   */
  def features: FeatureCollection = {
    new FeatureCollection(source, new gt.data.DefaultQuery()) 
  }
  
  /** 
   * Get a filtered feature collection.
   */
  def filter(pred: org.opengis.filter.Filter): FeatureCollection = {
    new FeatureCollection(source, new gt.data.DefaultQuery(name, pred)) 
  }

  /**
   * Get the number of features currently in the layer.
   */
  def count: Int = source.getCount(new gt.data.DefaultQuery())

  /**
   * Get the bounding box of this Layer, in the format:
   * (minlong, minlat, maxlong, maxlat, srid)
   *
   * @todo: Use an Envelope object for this
   */
  def bounds: (Double, Double, Double, Double, String) = {
    val bbox = source.getBounds()
    val crs = gt.referencing.CRS.lookupEpsgCode(
      bbox.getCoordinateReferenceSystem(), true
    )
    (bbox.getMinX, bbox.getMinY, bbox.getMaxX, bbox.getMaxY, "EPSG:" + crs)
  }

  /**
   * Add a single Feature to this data set.
   */
  def += (f: Feature) { this ++= Seq.singleton(f) }

  /**
   * Add multiple features to this data set.  This should be preferred over
   * repeated use of += when adding multiple features.
   */
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

/**
 * Handy object for loading a Shapefile directly.  The Shapefile layer is
 * implicitly created in a Directory datastore.
 */
object Shapefile {
  private def basename(f: File) = f.getName().replaceFirst("\\.[^.]+$", "")
    
  def apply(path: String): Layer = apply(new File(path))

  def apply(file: File): Layer = {
    val ws = Directory(file.getParent())
    ws.layer(basename(file))
  }
}
