package org.geoscala.example

import scala.collection.jcl.HashMap
import java.io.File
import java.io.FileNotFoundException
import java.io.Serializable
import java.util.Set

import javax.swing.JFileChooser
import javax.swing.JOptionPane
import javax.swing.filechooser.FileFilter

import org.geotools.data.DataStore
import org.geotools.data.DataStoreFactorySpi
import org.geotools.data.DataStoreFinder
import org.geotools.data.DataUtilities
import org.geotools.data.DefaultQuery
import org.geotools.data.DefaultTransaction
import org.geotools.data.FeatureSource
import org.geotools.data.FeatureStore
import org.geotools.data.Transaction
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.factory.GeoTools
import org.geotools.feature.FeatureCollection
import org.geotools.referencing.CRS
import org.opengis.feature.simple.SimpleFeature
import org.opengis.feature.simple.SimpleFeatureType
import org.opengis.referencing.crs.CoordinateReferenceSystem

object Shp2Shp extends org.geoscala.feature.GeoCrunch {
  def main(args: Array[String]) = {
    println("Welcome to GeoTools:" + GeoTools.getVersion())

    val file = promptShapeFile

    val connect = new HashMap[String, Object]() + 
      ("url" -> file.toURI.toURL)

    val dataStore = DataStoreFinder.getDataStore(connect.underlying)
    val typeNames = dataStore.getTypeNames()
    val typeName = typeNames.elements.next

    println("Reading content " + typeName)
    val featureSource = dataStore.getFeatureSource(typeName)

    val simpleFeatureType = featureSource.getSchema
    println("Header: " + DataUtilities.spec(simpleFeatureType))

    val query = new DefaultQuery()
    query.setTypeName(typeName)

    var prj = simpleFeatureType.getCoordinateReferenceSystem;
    if (prj == null) {
      prj = getCoordinateReferenceSystem("No projection found for "
        + file + "; please choose one:");
      query.setCoordinateSystem(prj);
    }

    val crs = getCoordinateReferenceSystem("Project " + file + " to:");
    query.setCoordinateSystemReproject(crs);

    val collection = featureSource.getFeatures(query);
    val newFile = getNewShapeFile(file);

    val factory = new ShapefileDataStoreFactory();

    val create = new HashMap[AnyRef,AnyRef]() + 
      ("url" -> newFile.toURI.toURL) +
      ("create spatial index" -> java.lang.Boolean.TRUE)

    def createStore[A,B]: (java.util.HashMap[A,B]) => DataStore = factory.createNewDataStore

    val newDataStore = createStore(create.underlying)

    newDataStore.createSchema(collection.getSchema);
    val transaction = new DefaultTransaction("Reproject");
    val featureStore = 
      newDataStore.getFeatureSource(typeName)
      .asInstanceOf[FeatureStore[SimpleFeatureType, SimpleFeature]];

    featureStore.setTransaction(transaction);

    try {
      featureStore.addFeatures(collection);
      transaction.commit();
    } catch {
      case ex => {
        ex.printStackTrace();
        transaction.rollback();
      }
    } finally {
      transaction.close();
    }

    System.exit(0);
  }

  private def getNewShapeFile(file: File): File = {
    val path = file.getAbsolutePath();
    val newPath = path.substring(0, path.length() - 4) + "2.shp";

    val chooser = new JFileChooser();
    chooser.setDialogTitle("Save reprojected shapefile");
    chooser.setSelectedFile(new File(newPath));

    object filter extends FileFilter {
      def accept(f: File): Boolean = {
        f.isDirectory || f.getPath.toLowerCase.endsWith("shp")
      }
      def getDescription = "Shapefiles"
    }

    if (chooser.showSaveDialog(null) != JFileChooser.APPROVE_OPTION) {
      System.exit(0);
    }

    val newFile = chooser.getSelectedFile();

    if (newFile == file) {
      println("Cannot replace " + file);
      System.exit(0);
    }

    return newFile;
  }

  @throws(classOf[Exception])
  private def getCoordinateReferenceSystem
    (message: String): CoordinateReferenceSystem = {
      val codes = CRS.getSupportedCodes("EPSG");
      val selected: String = JOptionPane.showInputDialog(
        null, 
        message,
        "Choose a Projection", 
        JOptionPane.QUESTION_MESSAGE, 
        null,
        codes.toArray, 
        "4326"
      ).asInstanceOf[String];

      if (selected == null) {
        System.exit(0);
      }

      return CRS.decode("EPSG:" + selected);
  }
}

