package org.geoscript.feature

import com.vividsolutions.jts.geom.Geometry
import org.opengis.feature.`type`.{Name,AttributeDescriptor}
import org.opengis.feature.simple.SimpleFeatureType
import org.opengis.referencing.crs.{CoordinateReferenceSystem=>CRS}
import org.geotools.feature.{AttributeTypeBuilder,NameImpl}
import org.geotools.feature.simple.SimpleFeatureTypeBuilder

trait SchemaHelpers {
  private val attbuilder = new AttributeTypeBuilder
  private val ftbuilder = new SimpleFeatureTypeBuilder

  implicit def name(s: String) = new NameImpl(s)

  implicit def descriptor(att: (String, Class[_<:Geometry], CRS))
  : AttributeDescriptor = 
  {
    attbuilder.name(att._1).binding(att._2).crs(att._3)
    val gtype = attbuilder.buildGeometryType
    attbuilder.buildDescriptor(att._1, gtype)
  }

  implicit def descriptor(att: (String, Class[_])): AttributeDescriptor = {
    attbuilder.name(att._1).binding(att._2)
    val atype = attbuilder.buildType
    attbuilder.buildDescriptor(att._1, atype)
  }

  def buildFeatureType(name: Name, props: AttributeDescriptor*)
  : SimpleFeatureType = 
  {
    ftbuilder.setName(name)
    props.foreach(ftbuilder.add)
    ftbuilder.buildFeatureType
  }
}
