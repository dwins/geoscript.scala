package org.geoscript.geocss

import org.specs._
import org.geoscript.geocss.compat.CSS2SLD

class JavaCompatibilityTest extends Specification {
    
    "Java compatibility layer can convert CSS to Style" in {
        val stream = getClass.getResourceAsStream("/minimal.css")
        val style = CSS2SLD.convert(stream)
        style must notBeNull
    }

}


