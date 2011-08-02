package org.geoscript.layer

import java.io.File

import org.opengis.feature.simple.{ SimpleFeature, SimpleFeatureType }
import org.opengis.feature.`type`.{ AttributeDescriptor, GeometryDescriptor }
import org.{ geotools => gt }
import com.vividsolutions.jts.{ geom => jts }

import org.geoscript.feature._
import org.geoscript.filter._
import org.geoscript.geometry._
import org.geoscript.projection._
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
  def source = store.getFeatureSource(name)

  /** 
   * The Schema describing this layer's contents.
   */
  def schema: Schema = Schema(store.getSchema(name))

  /**
   * Get a feature collection that supports the typical Scala collection
   * operations.
   */
  def features: FeatureCollection = {
    new FeatureCollection(source, new gt.data.Query())
  }
  
  /** 
   * Get a filtered feature collection.
   */
  def filter(pred: Filter): FeatureCollection = {
    new FeatureCollection(source, new gt.data.Query(name, pred.underlying))
  }

  /**
   * Get the number of features currently in the layer.
   */
  def count: Int = source.getCount(new gt.data.Query())

  /**
   * Get the bounding box of this Layer, in the format:
   */
  def bounds: Bounds = Bounds(source.getBounds()) in schema.geometry.projection

  /**
   * Add a single Feature to this data set.
   */
  def += (f: Feature) { this ++= Seq(f) }

  /**
   * Add multiple features to this data set.  This should be preferred over
   * repeated use of += when adding multiple features.
   */
  def ++= (features: Traversable[Feature]) {
    val tx = new gt.data.DefaultTransaction
    val writer = store.getFeatureWriterAppend(name, tx)

    try {
      for (f <- features) {
        val toBeWritten = writer.next()
        f.writeTo(toBeWritten)
        writer.write()
      }
      tx.commit()
    } catch {
      case ex =>
        tx.rollback()
        throw ex
    } finally {
      writer.close()
      tx.close()
    }
  }

  def -= (feature: Feature) { this --= Seq(feature) }

  def --= (features: Iterable[Feature]) {
    exclude(Filter.or(
      features.toSeq filter { null != } map { f =>  Filter.id(Seq(f.id)) }
    ))
  }

  def exclude(filter: Filter) { 
    store.getFeatureSource(name)
      .asInstanceOf[gt.data.FeatureStore[SimpleFeatureType, SimpleFeature]]
      .removeFeatures(filter.underlying) 
  }

  def update(replace: Feature => Feature) {
    update(Filter.Include)(replace)
  }

  def update(filter: Filter)(replace: Feature => Feature) {
    val tx = new gt.data.DefaultTransaction
    val writer = filter match {
      case Filter.Include => store.getFeatureWriter(name, tx)
      case filter => store.getFeatureWriter(name, filter.underlying, tx)
    }

    while (writer hasNext) {
      val existing = writer.next()
      replace(Feature(existing)).writeTo(existing)
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
