package org.geoscript

import scala.collection.JavaConverters._

import feature.{ Feature, Field, Schema }
import filter.Filter
import workspace.Workspace

package object layer {
  type Layer = org.geotools.data.simple.SimpleFeatureStore
  type Query = org.geotools.data.Query

  def Layer(name: String, fields: Seq[Field]): Layer = {
    Layer(Schema(name, fields))
  }

  def Layer(s: feature.Schema): Layer = {
    val workspace = new org.geotools.data.memory.MemoryDataStore
    workspace.createSchema(s)
    workspace.getFeatureSource(s.name).asInstanceOf[Layer]
  }

  def Layer(name: String, fs: Traversable[feature.Feature]): Layer = {
    val schemata = (fs map (_.getFeatureType)).toSet

    // TODO: Maybe it would be cool to infer a more general schema from the layers
    require(schemata.size == 1, "Can't mix schemas when creating a new layer")

    val layer = Layer(schemata.head)
    layer ++= fs
    layer
  }
}

package layer {
  class RichLayer(layer: Layer) extends Traversable[Feature] {
    def count: Int = layer.getCount(new Query)
    def envelope: geometry.Envelope = layer.getBounds
    def name: String = layer.getName.getLocalPart
    def schema: feature.Schema = layer.getSchema

    def foreach[T](f: Feature => T) { foreachInCollection(layer.getFeatures)(f) }

    def filtered(filt: Filter): Traversable[Feature] =
      new Traversable[Feature] {
        def foreach[T](func: Feature => T) {
          foreachInCollection(layer.getFeatures(filt))(func)
        }
      }

    private def foreachInCollection
      (fc: org.geotools.data.simple.SimpleFeatureCollection)
      (f: Feature => _): Unit
    = {
      val iter = fc.features()
      try while (iter.hasNext) f(iter.next)
      finally iter.close()
    }

    def workspace: Workspace = layer.getDataStore().asInstanceOf[Workspace]

    def += (fs: feature.Feature*) = this ++= fs

    def ++= (features: Traversable[feature.Feature]) {
      val tx = new org.geotools.data.DefaultTransaction
      layer.setTransaction(tx)
      try {
        val featureColl = org.geotools.feature.FeatureCollections.newCollection()
        featureColl.addAll(features.toSeq.asJavaCollection)
        layer.addFeatures(featureColl)
        tx.commit()
      } catch {
        case ex => tx.rollback(); throw ex
      } finally {
        tx.close()
        layer.setTransaction(org.geotools.data.Transaction.AUTO_COMMIT)
      }
    }
  }

  object Shapefile {
    private def basename(f: java.io.File) =
      f.getName().replaceFirst("\\.[^.]+$", "")

    def apply(path: String): Layer = apply(new java.io.File(path))
    def apply(path: java.io.File): Layer = {
      val ws = workspace.Directory(path.getParent())
      ws(basename(path))
    }
  }
}
