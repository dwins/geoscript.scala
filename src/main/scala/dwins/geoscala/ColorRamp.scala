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
import org.geotools.factory.CommonFactoryFinder
import org.geotools.feature.FeatureCollection
import org.geotools.feature.FeatureIterator
import org.geotools.styling.SLDTransformer
import org.geotools.styling.Style
import org.geotools.styling.Rule

import org.opengis.feature.simple.SimpleFeature
import org.opengis.feature.simple.SimpleFeatureType

object ColorRamp {

  def pairwise[A](s: List[A]): List[(A,A)] = {
    s zip (s drop 1)
  }

  def colorRamp(s: FeatureSource[SimpleFeatureType,SimpleFeature],
    property: String): Style = {
    val styles = CommonFactoryFinder.getStyleFactory(null)
    val filters = CommonFactoryFinder.getFilterFactory(null)

    def rule(range: (Double, Double)): Rule = {
      val rule = styles.createRule
      rule.setFilter(filters.between(
        filters.property(property),
        filters.literal(range._1),
        filters.literal(range._2)
      ))
      rule.symbolizers.add(styles.createPolygonSymbolizer(
        styles.getDefaultStroke,
        styles.getDefaultFill,
        null
      ))
      return rule
    }

    val style = styles.createStyle
    val ramp = ranges(s.getFeatures, "PERSONS")
    val ftStyle = styles.createFeatureTypeStyle(ramp.map(rule).toArray)
    style.featureTypeStyles.add(ftStyle)

    return style
  }

  def ranges(col: FeatureCollection[SimpleFeatureType, SimpleFeature], 
    p: String) : List[(Double, Double)] = {
    // Use the value for the first feature as the starting value 
    // for both max and min
    val it = col.features

    var min = it.next.getAttribute(p).asInstanceOf[Double]
    var max = min

    try {
      while (it.hasNext) {
        val current = it.next.getAttribute(p).asInstanceOf[Double]
        min = min.min(current)
        max = max.max(current)
      }
    } finally { 
      if (it!= null) col.close(it)
    }

    // create 10 pairs representing ranges between min and max, linearly spaced
    pairwise((0 to 10).toList.map(min + (max - min) * (_)))
  }

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

      val xformer = new SLDTransformer
      xformer.setIndentation(2)
      xformer.transform(colorRamp(featureSource, "PERSONS"), System.out)
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
