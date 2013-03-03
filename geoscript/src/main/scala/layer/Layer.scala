package org.geoscript

import java.io.File

import com.vividsolutions.jts.{ geom => jts }

import org.geoscript.feature._
import org.geoscript.filter._
import org.geoscript.geometry._
import org.geoscript.projection._
import org.geoscript.workspace._

package object layer {
  type Layer = org.geotools.data.FeatureSource[Schema, Feature]
  type WritableLayer = org.geotools.data.FeatureStore[Schema, Feature]

  /**
   * A Layer represents a geospatial dataset.
   */
  implicit class RichLayer(val source: Layer) extends AnyVal {
    /**
     * The name of this data set
     */
    def name: String = schema.name

    /** 
     * The Schema describing this layer's contents.
     */
    def schema: Schema = source.getSchema

    /**
     * Get a feature collection that supports the typical Scala collection
     * operations.
     */
    def features: FeatureCollection =
      source.getFeatures(new org.geotools.data.Query)
    
    /** 
     * Get a filtered feature collection.
     */
    def filter(pred: Filter): FeatureCollection =
      source.getFeatures(new org.geotools.data.Query(name, pred))

    /**
     * Get the number of features currently in the layer.
     */
    def count: Int = source.getCount(new org.geotools.data.Query())

    /**
     * Get the bounding box of this Layer, in the format:
     */
    def envelope: Envelope = source.getBounds() // in schema.geometry.projection

    /**
     * Test whether the data source supports modifications and return a
     * WritableLayer if so.  Otherwise, a None is returned.
     */
    def writable: Option[WritableLayer] =
      source match {
        case (writable: WritableLayer) => Some(writable)
        case _ => None
      }
  }

  implicit class RichWritableLayer(val store: WritableLayer) extends AnyVal {
    /**
     * Add a single Feature to this data set.
     */
    def += (f: Feature*) { this ++= f }

    private
    def dstore = store.getDataStore.asInstanceOf[org.geotools.data.DataStore]

    /**
     * Add multiple features to this data set.  This should be preferred over
     * repeated use of += when adding multiple features.
     */
    def ++= (features: Traversable[Feature]) {
      val tx = new org.geotools.data.DefaultTransaction
      val writer = dstore.getFeatureWriterAppend(store.name, tx)

      try {
        for (f <- features) {
          writer.next.attributes = f.attributes
          writer.write()
        }
        tx.commit()
      } catch {
        case (ex: java.io.IOException) =>
          tx.rollback()
          throw ex
      } finally {
        writer.close()
        tx.close()
      }
    }

    def -= (feature: Feature*) { this --= feature }

    def --= (features: Traversable[Feature]) {
      exclude(Filter.id(
        features.filter { null !=  _ }
                .map { f => f.id }
                .toSeq
      ))
    }

    def exclude(filter: Filter) { 
      store.removeFeatures(filter) 
    }

    def update(replace: Feature => Unit) {
      update(Include)(replace)
    }

    def update(filter: Filter)(replace: Feature => Unit) {
      val tx = new org.geotools.data.DefaultTransaction
      val writer = filter match {
        case Include => dstore.getFeatureWriter(store.name, tx)
        case filter => dstore.getFeatureWriter(store.name, filter, tx)
      }

      while (writer.hasNext) {
        val existing = writer.next()
        replace(existing)
        writer.write()
      }

      writer.close()
      tx.commit()
      tx.close()
    }
  }
}

package layer {
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
}
