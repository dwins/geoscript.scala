package org.geoscript.geocss

/**
 * A styling property
 */
case class Property(name: String, values: Seq[Seq[Value]]) {
  override def toString = {
    "%s: %s".format(
      name,
      values.map(_.mkString("[", ",", "]")).mkString(",")
    )
  }
}
