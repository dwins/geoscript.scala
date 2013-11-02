package org.geoscript.geocss

sealed trait Warning
object Warning {
  def deprecated(old: String, replacement: String) = new Warning {}
}
