package org.geoscript

import java.io.{File, Serializable}
import org.geoscript.feature._
import org.geoscript.layer._
import org.{geotools => gt}
import scala.collection.JavaConversions._

package object workspace {
  type Workspace = org.geotools.data.DataStore

  implicit class RichWorkspace(val workspace: Workspace) {
    def count = workspace.getTypeNames.length
    def names: Seq[String] = workspace.getTypeNames
    def layer(theName: String): Layer = workspace.getFeatureSource(theName)
    def layers: Seq[Layer] = names.view.map(layer(_))

    def create(name: String, fields: Field*): Layer = create(name, fields) 

    def create(name: String, fields: Traversable[Field]): Layer = {
      val builder = new gt.feature.simple.SimpleFeatureTypeBuilder
      builder.setName(name)
      fields foreach {
        case field: GeoField =>
          builder.crs(field.projection)
          builder.add(field.name, field.binding)
        case field =>
          builder.add(field.name, field.binding)
      }
      workspace.createSchema(builder.buildFeatureType())
      layer(name)
    }
     
    def create(schema: Schema): Layer = create(schema.name, schema.fields: _*)
  }
}

package workspace {
  object Memory {
    def apply() = new org.geotools.data.memory.MemoryDataStore()
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
      create(connection)
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
      create(connection: java.util.HashMap[_,_])
    } 
  }

  object Directory {
    private val factory = new gt.data.shapefile.ShapefileDataStoreFactory

    def apply(path: String): Workspace = apply(new File(path))

    def apply(path: File): Workspace = {
      val params = new java.util.HashMap[String, java.io.Serializable]
      params.put("url", path.toURI.toURL)
      factory.createDataStore(params: java.util.Map[_, _])
    }
  }
}
