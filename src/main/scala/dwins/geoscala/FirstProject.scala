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

object FirstProject {

  @throws(classOf[Exception])
  def main(args: Array[String]) = {
    println("Welcome to GeoTools:" + GeoTools.getVersion());

    val file = promptShapeFile(args);
    try {
      // Connection parameters
      val connectParameters = new HashMap[String, Serializable]() +
        ("url" -> file.toURI.toURL) +
        ("create spatial index" -> true)

      val dataStore = DataStoreFinder.getDataStore(connectParameters.underlying)

      // we are now connected
      val typeNames = dataStore.getTypeNames()
      val typeName = typeNames.elements.next

      println("Reading content " + typeName);

      val featureSource = dataStore.getFeatureSource(typeName)
      val collection = featureSource.getFeatures
      val iterator = collection.features

      var totalLength = 0.0d;
      try {
        while (iterator.hasNext) {
          totalLength += 
            iterator.next.getDefaultGeometry.asInstanceOf[Geometry].getLength
        }
      } finally { 
        if (iterator != null) collection.close(iterator)
      }

      println("Total Length " + totalLength);
    } catch {
      case ex => {
        ex.printStackTrace();
        System.exit(1);
      }
    }
    System.exit(0);
  }

  private def promptShapeFile(args: Array[String]): File = {
    val file = 
      if (args.length == 0) {
        val chooser = new JFileChooser();
        chooser.setDialogTitle("Open Shapefile for Reprojection");

        object filter extends FileFilter {
          def accept(f: File): Boolean = {
            f.isDirectory || f.getPath.toLowerCase.endsWith("shp")
          }

          def getDescription(): String = "ShapeFiles"
        }

        chooser.setFileFilter(filter)

        if (chooser.showOpenDialog(null) != JFileChooser.APPROVE_OPTION) {
          System.exit(0)
        }

        println("You chose to open this file: " + chooser.getSelectedFile.getName);
        chooser.getSelectedFile
      } else {
        new File(args(0));
      }
    if (!file.exists) {
      throw new FileNotFoundException(file.getAbsolutePath);
    }
    return file
  }
}
