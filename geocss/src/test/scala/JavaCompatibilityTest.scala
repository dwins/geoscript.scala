package org.geoscript.geocss

import org.specs2._
import org.geoscript.geocss.compat.CSS2SLD

class JavaCompatibilityTest extends Specification { 
  def is =
    "Java compatibility layer can convert CSS to Style" ! {
        val stream = getClass.getResourceAsStream("/minimal.css")
        val reader = new java.io.InputStreamReader(stream)
        val style = CSS2SLD.convert(reader)
        style must not(beNull)
    }
}


