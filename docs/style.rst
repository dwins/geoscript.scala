Creating Styles
===============

In order to render geospatial data, GeoScript requires a style associated with each layer.
Essentially, a style represents a set of rules for converting data into pixels.
GeoScript provides a few different ways of getting those styles into your scripts.

.. seealso:: 
    :doc:`/render` for information on applying Styles to Layers in order to produce image files or draw data on-screen.

Load an SLD Document
--------------------

SLD (also known as the Styled Layer Descriptor format) is a standardized format for encoding styling rules for geospatial data in XML.
You can load an SLD document from a file::

   import org.geoscript._
   val myStyle = style.SLD.fromFile("path/to/style.sld")

If you find it useful, you can also embed SLDs in your Scala source code as string literals (if you do, I highly recommend using `triple-quoted strings <http://ofps.oreilly.com/titles/9780596155957/TypeLessDoMore.html#StringLiterals>`_.)
For example::

   import org.geoscript._

   val rawSLD = """
   <UserStyle xmlns="http://www.opengis.net/sld">
     <FeatureTypeStyle>
       <Rule>
         <PolygonSymbolizer>
           <Fill/>
         </PolygonSymbolizer>
       </Rule>
     </FeatureTypeStyle>
   </UserStyle>
   """
   val myStyle = style.SLD.fromString(rawSLD)

A nicer technique, especially if you are generating SLD programmatically, is to use Scala's `XML literal <http://www.scala-lang.org/node/131>` syntax::

   import org.geoscript._

   val casings = 
     Seq(("#FF0000", 12), ("#DD0000", 8), ("#AA0000", 5), ("#770000", 3))

   val sldXML = 
     <UserStyle xmlns="http://www.opengis.net/sld">
       <FeatureTypeStyle>
         <Rule>
           { for ((color, width) <- casings) yield
               <LineSymbolizer>
                 <Stroke>
                   <CssParameter name="stroke">{color}</CssParameter>
                   <CssParameter name="stroke-width">{width}</CssParameter>
                 </Stroke>
               </LineSymbolizer>
           }
         </Rule>
       </FeatureTypeStyle>
     </UserStyle>

   val myStyle = style.SLD.fromXML(sldXML)

.. seealso:: 

    The official specification for SLD is available from the OGC website at http://www.opengeospatial.org/standards/sld .
    A more `friendly guide <http://docs.geoserver.org/stable/en/user/styling/>`_ to the format is also available in the GeoServer users manual.

Load a CSS Document
-------------------

CSS (also known as Cascading Style Sheets) is a format for encoding document styling information.
While CSS was originally designed for styling textual data on the Web, GeoScript adapts it for use with geographic information (borrowing heavily on work done for the SVG and SLD specifications.)
The CSS variant used in GeoScript is intended to be comfortable for users familiar with CSS as used in Web browsers, but also approachable to users familiar with the SLD standard.

As with SLD, GeoScript allows loading from files::

   import org.geoscript._
   val myStyle = style.CSS.fromFile("path/to/style.css")

and String literals::

   import org.geoscript._
   val rawCSS = """* { fill: grey }"""
   val myStyle = style.CSS.fromString(rawCSS)

Since CSS is not an XML format, Scala's XML literals are not supported.

.. note::

   While SLD can encode styling information for both raster and vector layers, CSS is currently only usable with vector layers.

.. seealso::

    For general information about the structure and syntax used in CSS, see the standard documentation from the W3C at http://www.w3.org/Style/CSS/ .
    A `guide <http://docs.geoserver.org/stable/en/user/community/css/>` to the changes to CSS syntax used in GeoScript is available in the GeoServer users manual.
    This may seem a bit odd, but the CSS styling support was originally developed as a GeoServer extension, so that is where the documentation originated.

Building Styles in Code
-----------------------

GeoScript Scala provides a third option specifically for building styles programmatically, as a typesafe and convenient alternative to constructing styles as strings or even XML literals.
It is based upon the idea of combinatorial logic - a small number of primitives which may be used individually for simple tasks, or combined or modified in various ways to achieve more complex effects.

Style Constructors
..................

In the Style combinator API, those primitives are three different basic approaches to turning geometries into images:

* A ``Fill`` is a Style which colors pixels inside of geometries

* A ``Stroke`` is a Style which colors pixels along the outlines of geometries

* A ``Graphic`` is a Style which draws some image from a file centered on the centroid of geometries (good for points especially.)

* A ``Symbol`` is similar to a ``Graphic`` but generates the image itself.
  This allows some types of data-driven symbology that are difficult to achieve with static files.
  Additionally, ``Symbols`` are preserved as vector graphics until the last stage of the rendering process and may produce better image quality than ``Graphics``.

* A ``Label`` draws some text as close to a geometry as possible.
  Usually this text is derived from the non-geometry attributes of a feature.

Each type of Style has a few unique properties that control its behavior.
For example, a ``Fill`` style has a ``fill`` property that tells it how to choose the color for each pixel.

General-purpose Combinators
...........................

There are only a couple of general-purpose modifiers for Styles in the combinator API.
These can be applied to *any* Style, even ones that are the result of previous combinator applications.

* **Conjunction:** Two Styles can be combined into one which will achieve the effects of both by using the ``Style#and`` method.
  For example, it's common to render polygon data with both a Fill and a Stroke::

    import org.geoscript.style.combinator._
    val myStyle = Fill("#aaaaaa") and Stroke("#000000")

* **Data Constraints:** A style can be constrained to only apply to a subset of features, based on data attributes.
  For example, you might want to only draw features where the IMPORTANCE attribute is at least 10::

    import org.geoscript.style.combinator._
    import org.geotools.filter.text.ecql.ECQL.{ toFilter => cql }
    val myStyle = Fill("#aaaaaa") where cql("IMPORTANCE >= 10")

* **Scale constraints:** Similar to filtering by data is rendering by scale.
  Here, we only show Features when the scale denominator (the ratio of real-world units to on-screen units) is below 10000 - that is, when the map is being rendered at 1:10000 scale::

    import org.geoscript.style.combinator._
    val myStyle = Fill("#aaaaaa") scaleBelow(10000)

  There is a ``scaleAbove`` which works similarly. Note that you can chain these modifiers::

    import org.geoscript.style.combinator._
    val myStyle = Fill("#aaaaaa") scaleAbove(1e4) scaleBelow(1e6)

Nesting Styles
..............

The primitive Styles used in the combinator API allow a bit more tweaking than just appending and filtering.
For one thing, they are all implemented as Scala `case classes <http://www.scala-lang.org/node/107>` so they support pattern matching and a ``copy`` method with named parameters::

    import org.geoscript.style.combinator._
    val fancyStroke = Stroke("#0000FF", width=2, opacity=0.6, linecap="round", linejoin="round")
    val fancyButGreen = fancyStroke.copy(stroke="#00FF00")

Additionally, the ``fill`` property of a ``Fill`` instance and the stroke property of a ``Stroke`` instance are not simply colors: they actually have the type ``org.geoscript.style.combinator.Paint``.
There is a ``Color`` class which extends that type, but that's not the only one - ``Graphic`` and ``Symbol`` instances are also instances of ``Paint``!
When they are used, the image that normally would be used as a point marker is repeated along the area for the Stroke or Fill, so you can have a pattern fill effect.

Conversely, a ``Symbol`` can be configured with a Stroke and Fill to customize the rendering.
If you like, you can even have a pattern fill with symbols using pattern fills with symbols, nested arbitrarily deep.

Combinator Examples
...................

See the ``examples/cookbook/`` directory for some examples of style combinators in action.
This directory contains scripts emulating the styles of the `SLD Cookbook <http://docs.geoserver.org/stable/en/user/styling/cookbook/>`_ section from the GeoServer manual.
They have the data necessary for rendering embedded in the file, so feel free to download the examples project and experiment with modifying the data in different ways.
