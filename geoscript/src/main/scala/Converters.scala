package org
package object geoscript {
  import feature._
  import geometry._
  import layer._
  import workspace._
  import render._
  import projection.{ Projection, RichProjection, Projectable, Referenced }

  implicit def enrichGeometry(g: Geometry): RichGeometry =
    new RichGeometry(g)

  implicit def enrichPoint(p: Point): RichPoint = new RichPoint(p)

  implicit def enrichEnvelope(e: Envelope) = new RichEnvelope(e)

  implicit def enrichTransform(t: Transform) = new RichTransform(t)

  implicit def enrichField(f: Field) = new RichField(f)

  implicit def enrichFeature(f: Feature) = new RichFeature(f)

  implicit def enrichGeoField(f: GeoField) = new RichGeoField(f)

  implicit def enrichSchema(s: Schema) = new RichSchema(s)

  implicit def enrichLayer(l: layer.Layer): RichLayer = new RichLayer(l)

  implicit def enrichWorkspace(workspace: Workspace): RichWorkspace =
    new RichWorkspace(workspace)

  implicit def enrichConnector(connector: Connector): RichConnector =
    new RichConnector(connector)

  implicit def enrichContent(content: Content): RichContent =
    new RichContent(content)

  implicit def enrichProjection(p: Projection): RichProjection = 
    new RichProjection(p)

  class RichProjectable[P : Projectable](p: P) {
    def in(proj: Projection): Referenced[P] = Referenced(p, proj)
    def lift: Option[Referenced[P]] = implicitly[Projectable[P]].lift(p)
  }

  implicit def enrichProjectable[P : Projectable](p: P) = new RichProjectable(p)
}
