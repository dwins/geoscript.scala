package org.geoscript.geocss

import scala.util.parsing.input.Position

final class Warning private(val position: Position, val message: String) {
  override def toString =
    s"$position: $message"
}
object Warning {
  def deprecated(position: Position, old: String, replacement: String) =
    new Warning(position, s"Deprecated term: $old. Use $replacement instead.")
}
