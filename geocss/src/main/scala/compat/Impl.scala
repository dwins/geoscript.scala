package org.geoscript.geocss

import org.geotools.styling.Style

package compat {
  class Impl extends CSS2SLD.Converter {
    override def convert(input: java.io.InputStream): Style =
      CssParser.parse(input) match {
        case CssParser.Success(style, _) => (new Translator).css2sld(style)
        case (ns: CssParser.NoSuccess) => error(ns.toString)
      }
  }
}
