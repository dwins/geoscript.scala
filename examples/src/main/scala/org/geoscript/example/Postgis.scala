package org.geoscript.example

import com.vividsolutions.jts.geom.Geometry
import org.geoscript._
import layer._

object PostgisTest extends GeoScript { 
  def main(args: Array[String]) {
    val shrubbery = workspace.Postgis(
      "database" -> "newyorkcity",
      "passwd" -> "hope"
      )
     println(shrubbery.count)
     println(shrubbery.layer("nyc_roadbed"))
  }
} 
