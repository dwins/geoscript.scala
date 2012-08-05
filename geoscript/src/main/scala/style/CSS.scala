package org.geoscript

import org.geotools.{ styling => gt }
import org.geotools.factory.CommonFactoryFinder.getStyleFactory

package object style {
  type Style = org.geotools.styling.Style
}

package style {
  object SLD {
    def fromFile(path: String): gt.Style = {
      val file = new java.io.File(path)
      new gt.SLDParser(getStyleFactory(null), file).readXML()(0)
    }

    def fromURL(url: String): gt.Style = {
      val resolved = new java.net.URL(new java.io.File(".").toURI.toURL, url)
      new gt.SLDParser(getStyleFactory(null), url).readXML()(0)
    }

    def fromString(sld: String): gt.Style = {
      val reader = new java.io.StringReader(sld)
      new gt.SLDParser(getStyleFactory(null), reader).readXML()(0)
    }

    def fromXML(sld: xml.Node): gt.Style = {
      val pprinter = new xml.PrettyPrinter(0, 0)
      fromString(pprinter.format(sld))
    }
  }

  object CSS {
    import org.geoscript.geocss.CssParser.parse
    val Translator = new org.geoscript.geocss.Translator
    import Translator.css2sld

    def fromFile(path: String): gt.Style = {
      val reader = new java.io.FileReader(path)
      val cssRules = parse(reader)
      css2sld(cssRules.get)
    }

    def fromURL(url: String): gt.Style = { 
      val resolved = new java.net.URL(new java.io.File(".").toURI.toURL, url)
      val cssRules = parse(resolved.openStream)
      css2sld(cssRules.get)
    }

    def fromString(css: String): gt.Style = {
      val cssRules = parse(css)
      css2sld(cssRules.get)
    }
  }
}
