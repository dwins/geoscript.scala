package org.geoscript.workspace

import java.io.{File, Serializable}
import org.geoscript.feature._
import org.geoscript.layer._
import org.{geotools => gt}

class Workspace(
  val underlying: gt.data.DataStore,
  val params: java.util.HashMap[String, java.io.Serializable]
) {
  def count = underlying.getTypeNames.length
  def names: Seq[String] = underlying.getTypeNames
  def layer(theName: String): Layer = new Layer {
    val name = theName
    val workspace = Workspace.this
    val store = underlying
  }

  def create(name: String, fields: Field*): Layer = {
    val builder = new gt.feature.simple.SimpleFeatureTypeBuilder
    builder.setName(name)
    for (field <- fields) {
      field match {
        case field: GeoField => 
          builder.crs(field.projection.crs)
          builder.add(field.name, field.gtBinding)
        case field => 
          builder.add(field.name, field.gtBinding)
      }
    }
    val ftype = builder.buildFeatureType()
    underlying.createSchema(ftype)
    layer(name)
  }
   
  def create(schema: Schema): Layer = create(schema.name, schema.fields: _*)
  override def toString = "<Workspace: %s>".format(params)
}

object Memory {
  def apply() = 
    new Workspace(
      new gt.data.memory.MemoryDataStore(),
      new java.util.HashMap
    )
}

object Postgis {
  val factory = new gt.data.postgis.PostgisNGDataStoreFactory
  val create: (java.util.HashMap[_,_]) => gt.data.DataStore = 
    factory.createDataStore

  def apply(params: (String,java.io.Serializable)*) = { 
    val connection = new java.util.HashMap[String,java.io.Serializable] 
    connection.put("port", "5432")
    connection.put("host", "localhost")
    connection.put("user", "postgres")
    connection.put("passwd","")
    connection.put("charset","utf-8")
    connection.put("dbtype", "postgis")
    for ((key,value) <- params)  { 
      connection.put(key,value)           
    }
    new Workspace(create(connection), connection)
 } 
}

object SpatiaLite {
  val factory = new gt.data.spatialite.SpatiaLiteDataStoreFactory 
  private val create: (java.util.HashMap[_,_]) => gt.data.DataStore =
    factory.createDataStore

  def apply(params: (String,java.io.Serializable)*) = { 
    val connection = new java.util.HashMap[String,java.io.Serializable] 
    connection.put("dbtype","spatialite")
    for ((key,value) <- params) { 
      connection.put(key,value)  
    }
    new Workspace(create(connection), connection) 
  } 
} 

object Directory {
  private val factory = new gt.data.directory.DirectoryDataStoreFactory

  def apply(path: String): Workspace = apply(new File(path))

  def apply(path: File): Workspace = {
    val params = new java.util.HashMap[String, java.io.Serializable]
    params.put("url", path.toURL)
    new Workspace(factory.createDataStore(params), params) {
      override def toString = "<Directory: [%s]>".format(params.get("url"))
    }
  }
}
