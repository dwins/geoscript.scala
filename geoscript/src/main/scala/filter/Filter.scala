package org.geoscript.filter

import scala.collection.JavaConverters._
import org.geoscript.geometry.Geometry

object Filter {
  def intersects(geometry: Geometry): Filter =
    factory.intersects(null, factory.literal(geometry))

  def id(ids: Seq[String]): Filter = 
    factory.id(ids.map(factory.featureId).toSet.asJava)

  def or(filters: Seq[Filter]): Filter =
    factory.or(filters.asJava)
}
