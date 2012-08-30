package org.geoscript
package projection

trait HasProjection[G] {
  def setProjection(p: Projection)(g: G): G
  def transform(p: Projection)(g: G): G
}

object HasProjection {
  import feature._

  implicit object FeaturesHaveProjections extends HasProjection[Feature] {
    def setProjection(p: Projection)(g: Feature): Feature = {
      val adjusted = SchemataHaveProjections.setProjection(p)(g.schema)
      adjusted(g.attributes)
    }

    def transform(p: Projection)(g: Feature): Feature = {
      val it = Iterator(g)
      val dummyIterator = new org.geotools.feature.FeatureIterator[Feature] {
        def close() {}
        def hasNext = it.hasNext
        def next = it.next
      }

      new org.geotools.data.crs.ReprojectFeatureIterator(
        dummyIterator,
        SchemataHaveProjections.setProjection(p)(g.schema),
        lookupTransform(g.schema.projection, p)
      ).next.asInstanceOf[Feature]
    }
  }

  implicit object GeoFieldsHaveProjections extends HasProjection[GeoField] {
    def setProjection(p: Projection)(g: GeoField): GeoField = {
      val builder = new org.geotools.feature.AttributeTypeBuilder
      builder.init(g)
      builder.setCRS(p)
      builder.buildDescriptor(g.getName.getLocalPart).asInstanceOf[GeoField]
    }

    def transform(p: Projection)(g: GeoField): GeoField = setProjection(p)(g)
  }

  implicit object SchemataHaveProjections extends HasProjection[Schema] {
    def setProjection(p: Projection)(g: Schema): Schema = {
      val builder = new org.geotools.feature.simple.SimpleFeatureTypeBuilder
      builder.init(g)
      builder.setAttributes(null: java.util.List[Field])
      g.fields.foreach {
        case (g: GeoField) => builder.add(force(p, g))
        case (f: Field) => builder.add(f)
      }
      builder.buildFeatureType()
    }
    def transform(p: Projection)(g: Schema): Schema = setProjection(p)(g)
  }
}
