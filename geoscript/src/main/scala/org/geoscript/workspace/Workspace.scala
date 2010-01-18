package org.geoscript.workspace

import org.geotools.data.DataStore

import org.geoscript.layer._

class Workspace(wrapped: DataStore) {
  def count = wrapped.getTypeNames.length
  def layers: Seq[String] = wrapped.getTypeNames
  def layer(name: String): Layer = new Layer(name, wrapped)
}
