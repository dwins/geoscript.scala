package org.geoscript.workspace

import org.{geotools => gt}

import org.geoscript.layer._

class Workspace(wrapped: gt.data.DataStore) {
  def count = wrapped.getTypeNames.length
  def layers: Seq[String] = wrapped.getTypeNames
  def layer(name: String): Layer = new Layer(name, wrapped)

  def create(name: String, fields: Field*): Layer = {
    val builder = new gt.feature.simple.SimpleFeatureTypeBuilder
    builder.setName(name)
    for (field <- fields) builder.add(field.name, field.binding)
    val ftype = builder.buildFeatureType()
    wrapped.createSchema(ftype)
    layer(name)
  }
   
  def create(schema: Schema): Layer = create(schema.name, schema.fields: _*)
}

object Memory {
  def apply() = new Workspace(new gt.data.memory.MemoryDataStore())
}
