package org.geoscript.style

import org.geotools.{ styling => gt }
import org.geotools.factory.CommonFactoryFinder.getStyleFactory

trait Style {
  def underlying: gt.Style
}

class WrappedSLD(raw: gt.Style) extends Style {
  def underlying = raw
}

object SLD {
  def fromFile(path: String): Style = {
    val file = new java.io.File(path)
    new WrappedSLD(
      new gt.SLDParser(getStyleFactory(null), file).readXML()(0)
    )
  }

  def fromURL(url: String): Style = {
    val resolved = new java.net.URL(new java.io.File(".").toURI.toURL, url)
    new WrappedSLD(
      new gt.SLDParser(getStyleFactory(null), url).readXML()(0)
    )
  }

  def fromString(sld: String): Style = {
    val reader = new java.io.StringReader(sld)
    new WrappedSLD(
      new gt.SLDParser(getStyleFactory(null), reader).readXML()(0)
    )
  }

  def fromXML(sld: xml.Node): Style = {
    val pprinter = new xml.PrettyPrinter(0, 0)
    fromString(pprinter.format(sld))
  }
}

object CSS {
  import org.geoscript.geocss.CssParser.parse
  val Translator = new org.geoscript.geocss.Translator
  import Translator.css2sld

  def fromFile(path: String): Style = {
    val reader = new java.io.FileReader(path)
    val cssRules = parse(reader)
    new WrappedSLD(css2sld(cssRules.get))
  }

  def fromURL(url: String): Style = { 
    val resolved = new java.net.URL(new java.io.File(".").toURI.toURL, url)
    val cssRules = parse(resolved.openStream)
    new WrappedSLD(css2sld(cssRules.get))
  }

  def fromString(css: String): Style = {
    val cssRules = parse(css)
    new WrappedSLD(css2sld(cssRules.get))
  }
}
