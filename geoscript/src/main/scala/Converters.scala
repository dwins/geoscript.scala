package org
package object geoscript {
  import geometry._
  import layer._
  import workspace._

  implicit def enrichGeometry(g: Geometry): RichGeometry =
    new RichGeometry(g)

  implicit def enrichPoint(p: Point): RichPoint =
    new RichPoint(p)

  implicit def enrichEnvelope(e: Envelope) = 
    new RichEnvelope(e)

  implicit def enrichLayer(layer: Layer): RichLayer =
    new RichLayer(layer)

  implicit def enrichWorkspace(workspace: Workspace): RichWorkspace =
    new RichWorkspace(workspace)

  implicit def enrichConnector(connector: Connector): RichConnector =
    new RichConnector(connector)
}
