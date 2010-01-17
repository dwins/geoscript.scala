package org.geoscript

import org.specs._

import geometry._
import projection._

class UsageTests extends Specification with GeoScript {
  "geometries" should  { 
    "work like on the geoscript homepage" in { 
    var p = Point(-111, 45.7)
    var p2 = Projection("epsg:4326") to Projection("epsg:26912") apply p
    var poly = p.buffer(100)
    
    p2.x must beCloseTo(499999.0, 0.5)
    p2.y must beCloseTo(5060716.0, 0.5)
    poly.area must beCloseTo(31214.45, 0.01)
  }
  "lineStrings should be easy" in { 
    LineString((10.0, 10.0), (20.0, 20.0), (30.0, 40.0)) 
  }
  "polygon should be easy" in { 
    Polygon(Seq((10.0, 10.0),(10.0, 20.0),(20.0, 20.0), (20.0, 15.0), (10.0, 10.0)), Seq.empty)
  }
  "mulit point should be easy" in {
    MultiPoint(Seq((20.0,20.0),(10.0,10.0))) 
  } 
 }  
}
