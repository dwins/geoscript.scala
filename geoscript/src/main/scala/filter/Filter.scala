package org.geoscript.filter

import scala.collection.JavaConverters._

import com.vividsolutions.jts.{geom=>jts}
import org.{geotools => gt}
import org.opengis.{filter => ogc}

import org.geoscript.geometry.Geometry

object Filter {
  private val factory = gt.factory.CommonFactoryFinder.getFilterFactory2(null)

  def intersects(geometry: Geometry): Filter =
    factory.intersects(null, factory.literal(geometry))

  def id(ids: Seq[String]): Filter = 
    factory.id(ids.map(factory.featureId).toSet.asJava)

  def or(filters: Seq[Filter]): Filter =
    factory.or(filters.asJava)
}
