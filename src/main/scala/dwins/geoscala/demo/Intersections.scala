package dwins.geoscala.demo

import java.io.File
import java.net.URL
import java.util.HashMap
import com.vividsolutions.jts.geom.{Geometry, Point}
import org.opengis.feature.simple.SimpleFeatureType
import org.geotools.data.{DataStore,DataStoreFinder,DefaultQuery,DefaultTransaction,FeatureStore}
import org.geotools.feature.{DefaultFeatureCollection, NameImpl}
import org.geotools.feature.simple.{SimpleFeatureBuilder, SimpleFeatureTypeBuilder}
import org.opengis.feature.simple.{SimpleFeature, SimpleFeatureType}

object Intersections extends GeoCrunch with SchemaHelpers{
  def process(store: DataStore, dest: FeatureStore[SimpleFeatureType,SimpleFeature]) = {
    val typename = store.getTypeNames()(0)
    println("Processing " + typename)
    val source = store.getFeatureSource(typename)
    val tx = new DefaultTransaction("tx")
    val intersections = new DefaultFeatureCollection("foo", dest.getSchema)
    dest.setTransaction(tx)
    val fb = new SimpleFeatureBuilder(dest.getSchema)
    var step = 0
    foreach (source.getFeatures) { x => {
      if (step % 1000 == 0) println("Processed %s features...".format(step)) 
      step += 1

      val xg = x.getDefaultGeometry.asInstanceOf[Geometry]
      val filter = filters.intersects(
        filters.property(source.getSchema.getGeometryDescriptor.getName),
        filters.literal(xg)
      )

      foreach (source.getFeatures(filter)) { y => {
        val yg = y.getDefaultGeometry.asInstanceOf[Geometry]
        if (x.getID < y.getID) {
          fb.add(xg intersection yg)
          intersections.add(fb.buildFeature(null))
        }
      }}
    }}
    println("Found %s intersections".format(intersections.size))
    dest.addFeatures(intersections)
    tx.commit
    tx.close
  }

  def rewrite(ft: SimpleFeatureType) = {
    val name = ft.getName.getLocalPart + "_intersect"
    val geom = ft.getGeometryDescriptor

    buildFeatureType(
      name,
      (geom.getName.getLocalPart, classOf[Point], geom.getCoordinateReferenceSystem)
    )
  }

  def main(args: Array[String]) = {
    val src = new File(args(0)).toURI.toURL
    val dst = new File(args(1)).toURI.toURL

    connect("url" -> src) match {
      case null => println("Couldn't open datastore at " + src)
      case store => {
        create("url" -> dst, "create spatial index" -> java.lang.Boolean.TRUE) match {
          case null => println("Couldn't open datastore at " + dst)
          case dest => {
            val dtype = rewrite(store.getFeatureSource(store.getTypeNames()(0)).getSchema)
            dest.createSchema(dtype)
            val dtable = dest.getFeatureSource(dtype.getName)
              .asInstanceOf[FeatureStore[SimpleFeatureType, SimpleFeature]]
            process(store, dtable)
          }
        }
      }
    }
  }
}
