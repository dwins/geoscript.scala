package org.geoscript

import scala.collection.JavaConverters._

import feature.{ Feature, Field, Schema }
import filter.Filter
import workspace.Workspace

package object layer {
  type Layer = org.geotools.data.simple.SimpleFeatureStore
  type Query = org.geotools.data.Query

  def Layer(s: feature.Schema): Layer = sys.error("Undefined")

  def Layer(name: String, fs: Iterable[feature.Feature]): Layer = {
    def undefined = sys.error("Undefined")
    implicit def f(f: Feature): { def schema: Schema } = undefined
    implicit def widen(f0: Seq[Field], f1: Seq[Field]): Seq[Field] = undefined
    val attributes = fs.map(_.schema.fields).foldLeft(Seq.empty[feature.Field])(widen)
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
      layer.getFeatures().addAll(features.asJavaCollection) 
      withAll { fs => fs.foreach(println) }
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
