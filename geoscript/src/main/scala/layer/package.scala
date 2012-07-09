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
    implicit def featureHasSchema(f: Feature): { def schema: Schema } =
      new {
        def schema = f.getFeatureType
      }

    implicit def widen(f0: Seq[Field], f1: Seq[Field]): Seq[Field] = {
      val names0 = (f0 map (_.name))
      val names1 = (f1 map (_.name))
      val names = names0 ++ (names1.filterNot(names0.contains))
      val byName0 = (f0 map (x => (x.name, x))).toMap
      val byName1 = (f1 map (x => (x.name, x))).toMap

      names.map { n => 
        ((byName0 get n), (byName1 get n)) match {
          case (None,    None) =>
            sys.error("Unexpected state... name " + n + "not found in either schema in widen method")
          case (Some(a), None) =>
            a
          case (None,    Some(b)) =>
            b
          case (Some(a), Some(b)) =>
            if (a != b)
              sys.error("Incompatible field types: " + a + ", " + b)
            else
              a
        }
      }
    }
    val attributes =
      (fs foldLeft Seq.empty[Field]) { 
        (fs: Seq[Field], f: Feature) => 
          widen(fs, f.schema.fields)
      }
    val schema: Schema = Schema(name, attributes)
    sys.error("undefined")
  }
}

package layer {
  class RichLayer(layer: Layer) {
    def count: Int = layer.getCount(new Query)
    def envelope: geometry.Envelope = layer.getBounds
    def name: String = layer.getName.getLocalPart
    def schema: feature.Schema = layer.getSchema
    def withAll[A](f: Iterator[feature.Feature] => A): A =
      this.withCollection(layer.getFeatures())(f)

    private def withCollection[A]
      (collection: org.geotools.data.simple.SimpleFeatureCollection)
      (f: Iterator[feature.Feature] => A)
      : A
    = {
      val iter = collection.features()
      val features = new Iterator[feature.Feature] {
        def hasNext: Boolean = iter.hasNext
        def next: feature.Feature = iter.next
      }
      try
        f(features)
      finally 
        iter.close()
    }

    def withFiltered[A]
      (filter: Filter)
      (f: Iterator[feature.Feature] => A)
      : A
    = this.withCollection(layer.getFeatures(filter))(f)

    def workspace: Workspace = layer.getDataStore().asInstanceOf[Workspace]

    def += (fs: feature.Feature*) = this ++= fs

    def ++= (features: Iterable[feature.Feature]) {
      val tx = new org.geotools.data.DefaultTransaction
      layer.setTransaction(tx)
      try {
        val featureColl = org.geotools.feature.FeatureCollections.newCollection()
        featureColl.addAll(features.asJavaCollection)
        layer.addFeatures(featureColl)
        tx.commit()
      } catch {
        case ex => tx.rollback(); throw ex
      }
    }

    def ++= (features: Iterator[feature.Feature]) {
      this ++= features.toIterable
    }
  }

  object Shapefile {
    private def basename(f: java.io.File) =
      f.getName().replaceFirst("\\.[^.]+$", "")

    def apply(path: String): Layer = apply(new java.io.File(path))
    def apply(path: java.io.File): Layer = {
      val ws = workspace.Directory(path.getParent())
      ws.layerNamed(basename(path))
    }
  }
}
