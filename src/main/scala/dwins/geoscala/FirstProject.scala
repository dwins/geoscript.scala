package dwins.geoscala

import scala.collection.jcl.HashMap
import scala.collection.jcl.MutableIterator

import java.io.File
import java.io.FileNotFoundException
import java.io.Serializable
import java.util.Map

import javax.swing.JFileChooser
import javax.swing.filechooser.FileFilter

import org.geotools.data.DataStore
import org.geotools.data.DataStoreFinder
import org.geotools.data.FeatureSource
import org.geotools.factory.GeoTools
import org.geotools.feature.FeatureCollection
import org.geotools.feature.FeatureIterator
import org.opengis.feature.simple.SimpleFeature
import org.opengis.feature.simple.SimpleFeatureType

import com.vividsolutions.jts.geom.Geometry

object FirstProject extends org.geoscala.feature.GeoCrunch {
  def main(args: Array[String]) = {
    println("Welcome to GeoTools:" + GeoTools.getVersion)

    val file = promptShapeFile

    // Connection parameters
    val parameters = (new HashMap[String, Serializable] +
      ("url" -> file.toURI.toURL) +
      ("create spatial index" -> true)
    ).underlying

    val dataStore = DataStoreFinder.getDataStore(parameters)

    // we are now connected
    val typeNames = dataStore.getTypeNames
    val featureSource = dataStore.getFeatureSource(typeNames(0))

    var totalLength = 0d
    foreach (featureSource.getFeatures) {
      totalLength += _.getDefaultGeometry.asInstanceOf[Geometry].getLength
    }

    println("Total Length " + totalLength);
  }
}
