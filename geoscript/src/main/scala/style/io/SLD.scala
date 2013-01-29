package org.geoscript.style
package io

import org.geoscript.io.{ Format, Source, Sink }

object SLD extends Format[Style] {
  def read(source: Source): Style = 
    source { in =>
      new org.geotools.styling.SLDParser(factory, in).readXML()(0)
    }
  def write[T](s: Style, sink: Sink[T]): T = {
    sink { out =>
      new org.geotools.styling.SLDTransformer().transform(s, out)
    }
  }
}
//   def fromFile(path: String): Style = {
//     val file = new java.io.File(path)
//     new WrappedSLD(
//       new gt.SLDParser(getStyleFactory(null), file).readXML()(0)
//     )
//   }
// 
//   def fromURL(url: String): Style = {
//     val resolved = new java.net.URL(new java.io.File(".").toURI.toURL, url)
//     new WrappedSLD(
//       new gt.SLDParser(getStyleFactory(null), url).readXML()(0)
//     )
//   }
// 
//   def fromString(sld: String): Style = {
//     val reader = new java.io.StringReader(sld)
//     new WrappedSLD(
//       new gt.SLDParser(getStyleFactory(null), reader).readXML()(0)
//     )
//   }
// 
//   def fromXML(sld: xml.Node): Style = {
//     val pprinter = new xml.PrettyPrinter(0, 0)
//     fromString(pprinter.format(sld))
//   }
// }
