package dwins.geoscala

import java.io.Serializable

import org.geotools.factory.CommonFactoryFinder
import org.geotools.data.DataStoreFinder

trait GeoCrunch {
  def styles = CommonFactoryFinder.getStyleFactory(null)
  def filters = CommonFactoryFinder.getFilterFactory2(null)
  def connect(params: Map[String,Serializable]) = {
    var javaMap = new scala.collection.jcl.HashMap[String, Serializable]()
    javaMap ++= params
    DataStoreFinder.getDataStore(javaMap.underlying)
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
}
