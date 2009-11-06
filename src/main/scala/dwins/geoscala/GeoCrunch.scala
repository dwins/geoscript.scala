package dwins.geoscala

import java.io.Serializable
import java.io.{File, FileFilter, FileNotFoundException}

import javax.swing.JFileChooser

import org.geotools.data.DataStoreFinder
import org.geotools.factory.CommonFactoryFinder
import org.geotools.feature.FeatureCollection

trait GeoCrunch {
  def styles = CommonFactoryFinder.getStyleFactory(null)
  def filters = CommonFactoryFinder.getFilterFactory2(null)
  def connect(params: Map[String,Serializable]) = {
    var javaMap = new scala.collection.jcl.HashMap[String, Serializable]()
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

  def promptOpenFile(ff: javax.swing.filechooser.FileFilter*): java.io.File = {
    val chooser = new javax.swing.JFileChooser
    if (ff.length > 0) chooser.setFileFilter(ff(0))
    (ff.elements drop 1).foreach(chooser.addChoosableFileFilter)
    if (chooser.showOpenDialog(null) != javax.swing.JFileChooser.APPROVE_OPTION) {
       System.exit(0) 
    }
    return chooser.getSelectedFile
  }

  def promptSaveFile(ff: javax.swing.filechooser.FileFilter*): java.io.File = {
    val chooser = new javax.swing.JFileChooser
    if (ff.length > 0) chooser.setFileFilter(ff(0))
    (ff.elements drop 1).foreach(chooser.addChoosableFileFilter)
    if (chooser.showSaveDialog(null) != javax.swing.JFileChooser.APPROVE_OPTION) {
       System.exit(0) 
    }
    return chooser.getSelectedFile
  }

  @throws(classOf[FileNotFoundException])
  def promptShapeFile(args: Array[String]): File = {
    val file =
      if (args.length == 0) {
        val chooser = new JFileChooser()
        chooser.setDialogTitle("Open Shapefile for Reprojection")
        object filter extends FileFilter {
          def accept(f: File) = {
            f.isDirectory || f.getPath.toLowerCase.endsWith("shp")
          }
          def getDescription = "Shapefiles"
        }

        if (chooser.showOpenDialog(null) != JFileChooser.APPROVE_OPTION) {
          System.exit(0)
        }

        println("You chose to open this file: " + chooser.getSelectedFile.getName());
        chooser.getSelectedFile
      } else {
        new File(args(0))
      }

    if (!file.exists) {
      throw new FileNotFoundException(file.getAbsolutePath())
    }

    return file;
  }
}
