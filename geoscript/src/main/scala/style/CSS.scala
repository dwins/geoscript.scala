package org.geoscript

import org.geotools.{ styling => gt }
import org.geotools.factory.CommonFactoryFinder.getStyleFactory

package style {
  object CSS {
    import org.geoscript.geocss.CssParser.parse
    val Translator = new org.geoscript.geocss.Translator
    import Translator.css2sld

    def fromFile(path: String): Style = {
      val reader = new java.io.FileReader(path)
      val cssRules = parse(reader)
      css2sld(cssRules.get)
    }

    def fromURL(url: String): Style = { 
      val resolved = new java.net.URL(new java.io.File(".").toURI.toURL, url)
      val cssRules = parse(resolved.openStream)
      css2sld(cssRules.get)
    }

    def fromString(css: String): Style = {
      val cssRules = parse(css)
      css2sld(cssRules.get)
    }
  }
}

package object style {
  type Style = org.geotools.styling.Style
  val factory = org.geotools.factory.CommonFactoryFinder.getStyleFactory()
}
