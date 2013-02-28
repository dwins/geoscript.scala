package org.geoscript.feature

/**
 * Utilities for working with [[org.geoscript.feature.Feature]] in a typesafe way.
 *
 * [[org.geoscript.feature.Feature]] is defined in terms of java.lang.Object and
 * requires casting to use.  The classes in this package provide some
 * convenience around doing the casting - in particular, we define a trait
 * [[Fields]] which can be used to retrieve and update fields from and to
 * features.
 *
 * A ``Fields`` may be constructed from a name and a type. The Fields then provides
 * an ``unapply`` method for extracting values from features, and an update
 * method for updating a feature (in place.)  This enables pattern-matching with
 * fields instances, and use of scala's syntactic sugar for updating
 * collections.  (By convention, fields instances should have names with an
 * initial capital for use with pattern matching.)
 *
 * {{{ 
 * val feature: Feature
 * val Title: Fields[String] = "title".of[String]
 * Title.unapply(feature): Option[String]
 * val Title(t) = feature
 * Title.update(feature, "Grand Poobah")
 * Title(feature) = "Grand Poobah"
 * }}}
 *
 * Fields instances may be combined by use of the ``~`` operator.  In this case,
 * the primitive values used with the Field must also be combined or
 * deconstructed using ``~``.
 * {{{
 * val Record: Fields[String ~ Int ~ String] = 
 *   "title".of[String] ~ "releasedate".of[Int] ~ "artist".of[String]
 * val Record(title ~ releaseDate ~ artist) = feature
 * Record(feature) = ("The White Album" ~ 1968 ~ "The Beatles")
 * }}}
 *
 * A ``Fields`` also provides the ``mkSchema`` method for creating a
 * [[org.geoscript.feature.Schema]].  Since a ``Schema`` requires a name and any
 * geometry fields must specify a [[org.geoscript.projection.Projection]], these
 * must be passed in to ``mkSchema``.
 * {{{
 * val Place = "name".of[String] ~ "loc".of[Geometry]
 * val schema = Place.mkSchema("places", LatLon)
 * }}}
 * 
 * It is possible to create Features instead of modifying them.  However, a
 * Schema is required.  The ``factoryForSchema`` method tests a schema for
 * compatibility with a Fields and produces a feature factory function if the
 * schema is compatible.
 *
 * {{{
 * val placeSchema: Schema
 * Place.factoryForSchema(placeSchema) match {
 *   case Some(mkPlace) => mkPlace("Library" ~ Point(1,2))
 *   case None => sys.error("The datastore is not compatible with place features")
 * }
 * }}}
 *
 * Finally, the ``schemaAndFactory`` method can be used to create a compatible
 * schema and return it along with the feature factory.  It takes the same
 * inputs as the ``mkSchema`` method.
 *
 * {{{
 * val (schema, mkPlace) = Place.schemaAndFactory("name", LatLon)
 * }}}
 */
package object builder {
  /**
   * Provides syntactic sugar for combining values into instances of the ``~``
   * class.
   *
   * @see [[org.geoscript.feature.builder]]
   */
  implicit class Appendable[A](a: A) {
    def ~ [B](b: B): (A ~ B) = new ~ (a, b)
  }

  /**
   * Provides syntactic sugar for creating Fields instances.
   *
   * @see [[org.geoscript.feature.builder]]
   */
  implicit class FieldSetBuilder(val name: String) extends AnyVal {
    def of[T : Manifest]: Fields[T] = {
      val clazz = implicitly[Manifest[T]].runtimeClass.asInstanceOf[Class[T]]
      new NamedField(name, clazz)
    }
  }
}

package builder {
  /**
   * A Fields represents one or more fields that features may have, and provides
   * facilities for retrieving and updating those fields in features.
   *
   * @see [[org.geoscript.feature.builder]]
   */
  sealed trait Fields[T] {
    def conformsTo(schema: Schema): Boolean
    def fields: Seq[Field]
    def values(t: T): Seq[AnyRef]
    def unapply(feature: Feature): Option[T]
    def update(feature: Feature, value: T): Unit

    final
    def schemaAndFactory
      (name: String,
       proj: org.geoscript.projection.Projection, 
       schemaFactory: org.opengis.feature.`type`.FeatureTypeFactory = schemaFactory,
       featureFactory: org.opengis.feature.FeatureFactory = featureFactory)
    : (Schema, T => Feature) = {
      val schema = mkSchema(name, proj, schemaFactory)
      (schema, factoryForSchema(schema, featureFactory).get)
    }

    final
    def ~[U](that: Fields[U]): Fields[T ~ U] =
      new ChainedFields[T, U](this, that)

    final
    def factoryForSchema
      (schema: Schema,
       featureFactory: org.opengis.feature.FeatureFactory = featureFactory)
    : Option[T => Feature] =
      if (conformsTo(schema))
        Some(unsafeFactory(schema, featureFactory))
      else
        None

    final
    def mkSchema
      (name: String,
       proj: org.geoscript.projection.Projection,
       schemaFactory: org.opengis.feature.`type`.FeatureTypeFactory = schemaFactory)
    : Schema = {
      val builder = new SchemaBuilder(schemaFactory)
      import builder._
      import org.geoscript.geometry.Geometry
      Schema(
        name,
        fields = this.fields.map {
          case Field(name, binding) if classOf[Geometry].isAssignableFrom(binding) =>
            GeoField(name, binding, proj)
          case f => f
        })
    }

    private[builder]
    def unsafeFactory
      (schema: Schema,
       featureFactory: org.opengis.feature.FeatureFactory)
    : T => Feature = {
      t => 
        val feature = featureFactory.createSimpleFeature(values(t).toArray, schema, "")
        update(feature, t)
        feature
    }
  }

  private[builder]
  class ChainedFields[T, U](
    tFields: Fields[T],
    uFields: Fields[U]
  ) extends Fields[T ~ U] {
    def conformsTo(schema: Schema): Boolean =
      (tFields conformsTo schema) && (uFields conformsTo schema)
    def fields = tFields.fields ++ uFields.fields
    def values(x: T ~ U): Seq[AnyRef] = {
      val (t ~ u) = x
      tFields.values(t) ++ uFields.values(u)
    }
    def update(feature: Feature, value: T ~ U) {
      val (t ~ u) = value
      tFields(feature) = t
      uFields(feature) = u
    }
    def unapply(feature: Feature): Option[T ~ U] = 
      for {
        t <- tFields.unapply(feature)
        u <- uFields.unapply(feature)
      } yield t ~ u
  }

  private[builder]
  class NamedField[T](name: String, clazz: Class[T]) extends Fields[T] {
    def conformsTo(schema: Schema): Boolean = schema.fields.exists(field =>
      field.name == name && field.binding.isAssignableFrom(clazz))
    def fields = Seq(schemaBuilder.Field(name, clazz))
    def values(t: T): Seq[AnyRef] = Seq(t)
    def update(feature: Feature, value: T) {
      feature.setAttribute(name, value)
    }
    def unapply(feature: Feature): Option[T] = {
      val att = feature.getAttribute(name)
      if (att == null || clazz.isInstance(att))
        Some(clazz.cast(att))
      else 
        None
    }
  }

  /**
   * A simple container for pairs of values, with nice syntax for destructuring
   * nested pairs.
   */
  case class ~[A,B](a: A, b: B)
}
