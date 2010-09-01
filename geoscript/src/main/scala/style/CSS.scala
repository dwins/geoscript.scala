package org.geoscript

import org.geotools.{ styling => gt }
import org.geotools.factory.CommonFactoryFinder.getStyleFactory

package style {
  trait Style {
    def unwrapped: gt.Style
  }

  class WrappedSLD(raw: gt.Style) extends Style {
    def unwrapped = raw
  }

  object SLD {
    def load(url: String): Style = {
      val resolved = new java.net.URL(new java.io.File(".").toURI.toURL, url)
      new WrappedSLD(
        new gt.SLDParser(getStyleFactory(null), url).readXML()(0)
      )
    }
  }

  object CSS {
    import geocss.CssParser.parse
    import geocss.Translator.css2sld

    def load(url: String): Style = {
      val resolved = new java.net.URL(new java.io.File(".").toURI.toURL, url)
      val cssRules = parse(resolved.openStream())
      new WrappedSLD(css2sld(cssRules.get))
    }
  }
}
