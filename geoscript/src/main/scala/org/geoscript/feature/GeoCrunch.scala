package org.geoscript.feature

import java.io.Serializable
import java.io.{File, FileNotFoundException}

import javax.swing.JFileChooser
import javax.swing.filechooser.FileFilter

import org.geotools.data.{DataStore, DataStoreFinder}
import org.geotools.factory.CommonFactoryFinder
import org.geotools.feature.FeatureCollection

trait GeoCrunch {
  def styles = CommonFactoryFinder.getStyleFactory(null)
  def filters = CommonFactoryFinder.getFilterFactory2(null)
  private val shp = new org.geotools.data.shapefile.ShapefileDataStoreFactory

  def create(params: (String, Object)*): DataStore = {
    var map = new java.util.HashMap[String, Object]
    for (val (key, value) <- params) map.put(key, value)
    shp.createNewDataStore(map)
  }

  def create(params: Map[String, Object]): DataStore = {
    var javaMap = new scala.collection.jcl.HashMap[String, Object]
    javaMap ++= params
    shp.createNewDataStore(javaMap.underlying)
  }

  def connect(params: (String, Object)*): DataStore = {
    var map = new java.util.HashMap[String,Object]
    for (val (key, value) <- params) map.put(key, value)
    DataStoreFinder.getDataStore(map)
  }

  def connect(params: Map[String,Serializable]) = {
    var javaMap = new scala.collection.jcl.HashMap[String, Serializable]
    javaMap ++= params
    DataStoreFinder.getDataStore(javaMap.underlying)
  }

  def foreach[F](fc: FeatureCollection[_,F]) (callback: F => Unit) = {
    val it = fc.iterator
    try {
      while (it.hasNext) try {
          callback(it.next)
      }
    } finally { fc.close(it) }
  }

  def promptOpenFile(ff: FileFilter*): java.io.File = {
    val chooser = new javax.swing.JFileChooser
    if (ff.length > 0) chooser.setFileFilter(ff(0))
    (ff.elements drop 1).foreach(chooser.addChoosableFileFilter)
    if (chooser.showOpenDialog(null) != javax.swing.JFileChooser.APPROVE_OPTION) {
       System.exit(0) 
    }
    return chooser.getSelectedFile
  }

  def promptSaveFile(ff: FileFilter*): java.io.File = {
    val chooser = new javax.swing.JFileChooser
    if (ff.length > 0) chooser.setFileFilter(ff(0))
    (ff.elements drop 1).foreach(chooser.addChoosableFileFilter)
    if (chooser.showSaveDialog(null) != javax.swing.JFileChooser.APPROVE_OPTION) {
       System.exit(0) 
    }
    return chooser.getSelectedFile
  }

  def promptShapeFile: File = {
    object ShapefileFilter extends FileFilter {
      def accept(f: File) = 
        f.isDirectory || f.getPath.toLowerCase.endsWith("shp")
      def getDescription = "Shapefiles"
    }

    promptOpenFile(ShapefileFilter)
  }
}
