package org.geoscript

import layer._

import scala.collection.JavaConverters._

import org.geotools.data.DataStoreFinder.getDataStore

package object workspace {
  type Connector = org.geotools.data.DataStoreFactorySpi
  type Workspace = org.geotools.data.DataStore

  def Workspace(params: (String, java.io.Serializable)*): Workspace =
    Workspace(params.toMap)

  def Workspace(params: Map[String, java.io.Serializable]): Workspace =
    getDataStore(params.asJava)

  def withWorkspace[A]
    (params: (String, java.io.Serializable)*)
    (f: Workspace => A)
    : A
  = withWorkspace(params.toMap)(f)

  def withWorkspace[A]
    (params: Map[String, java.io.Serializable])
    (f: Workspace => A)
    : A 
  = {
    val workspace = getDataStore(params.asJava)
    if (workspace == null) sys.error("No datastore loaded for params: " + params)
    try
      f(workspace)
    finally
      workspace.dispose()
  }

  def withMemoryWorkspace[A](f: Workspace => A): A = {
    val workspace = new org.geotools.data.memory.MemoryDataStore()
    try 
      f(workspace)
    finally
      workspace.dispose()
  }
}

package workspace {
  object Params {
    import java.io.Serializable
    private type Params = Map[String, Serializable]

    def directory(path: java.io.File): Params = {
      import org.geotools.data.shapefile.ShapefileDirectoryFactory.URLP
      Map(URLP.key -> path.toURI.toURL)
    }

    def database(
      dbtype: String,
      database: String,
      host: String = null,
      port: Int = -1,
      maxOpenPreparedStatements: Int = 0,
      minConnections: Int = 0,
      maxConnections: Int = 0,
      maxWait: Int = 0,
      exposePrimaryKeys: Boolean = false,
      validateConnections: Boolean = true)
      : Params 
    = {
      import org.geotools.jdbc.JDBCDataStoreFactory._
      Map(
        DBTYPE.key -> Option(dbtype),
        DATABASE.key -> Option(database),
        HOST.key -> Option(host),
        PORT.key -> Some(port).filter(_ > 0),
        MAX_OPEN_PREPARED_STATEMENTS.key -> Some(maxOpenPreparedStatements).filter(_ > 0),
        MINCONN.key -> Some(minConnections).filter(_ > 0),
        MAXCONN.key -> Some(maxConnections).filter(_ > 0),
        MAXWAIT.key -> Some(maxWait).filter(_ > 0),
        EXPOSE_PK.key -> Some(exposePrimaryKeys),
        VALIDATECONN.key -> Some(validateConnections)
      ).collect {
        case (k, Some(v)) => (k, v.asInstanceOf[Serializable])
      }
    }

    def postgis(
      database: String,
      host: String = null,
      port: Int = -1,
      maxOpenPreparedStatements: Int = 0,
      minConnections: Int = 0,
      maxConnections: Int = 0,
      maxWait: Int = 0,
      exposePrimaryKeys: Boolean = false,
      validateConnections: Boolean = true)
      : Params 
    = {
      import org.geotools.jdbc.JDBCDataStoreFactory._
      Map(
        DBTYPE.key -> Option("postgis"),
        DATABASE.key -> Option(database),
        HOST.key -> Option(host),
        PORT.key -> Some(port).filter(_ > 0),
        MAX_OPEN_PREPARED_STATEMENTS.key -> Some(maxOpenPreparedStatements).filter(_ > 0),
        MINCONN.key -> Some(minConnections).filter(_ > 0),
        MAXCONN.key -> Some(maxConnections).filter(_ > 0),
        MAXWAIT.key -> Some(maxWait).filter(_ > 0),
        EXPOSE_PK.key -> Some(exposePrimaryKeys),
        VALIDATECONN.key -> Some(validateConnections)
      ).collect {
        case (k, Some(v)) => (k, v.asInstanceOf[Serializable])
      }
    }
  }

  object Directory {
    def apply(s: String): Workspace = apply(new java.io.File(s))
    def apply(f: java.io.File): Workspace = Workspace(Params.directory(f))
  }

  class RichWorkspace(ws: Workspace) {
    def count = ws.getTypeNames.length
    def create(schema: feature.Schema): Layer = {
      ws.createSchema(schema.underlying)
      layerNamed(schema.name)
    }

    def layerNamed(name: String): Layer = 
      ws.getFeatureSource(name).asInstanceOf[Layer]

    def names: Seq[String] = ws.getTypeNames
  }

  class RichConnector(connector: Connector) {
    def withWorkspace[A]
      (params: (String, java.io.Serializable)*)
      (f: Workspace => A)
      : A
    = withWorkspace(params.toMap)(f)

    def withWorkspace[A]
      (params: Map[String, java.io.Serializable])
      (f: Workspace => A)
      : A 
    = {
      val workspace = connector.createDataStore(params.asJava)
      try
        f(workspace)
      finally
        workspace.dispose()
    }
  }
}
