package org.geoscript.geocss

import math._
import util.Sorting.stableSort

import org.geoscript.support.logic.reduce

import org.geotools.feature.NameImpl
import org.geotools.{styling => gt}
import org.geotools.styling.{
  LineSymbolizer,
  PointSymbolizer,
  PolygonSymbolizer,
  RasterSymbolizer,
  Symbolizer,
  TextSymbolizer,
  TextSymbolizer2,
  FeatureTypeStyle
}

import org.opengis.{ filter => ogc }

/**
 * The Translator object houses some facilities for converting token lists to
 * GeoTools Style objects.  The css2sld method does the actual conversion.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
class Translator(val baseURL: Option[java.net.URL]) {
  def this() = this(None)
  def this(url: String) = this(Some(new java.net.URL(url)))

  import CssOps.{ Color, Specificity, Symbol, URL, colors, expand }
  val styles = org.geotools.factory.CommonFactoryFinder.getStyleFactory()
  type OGCExpression = org.opengis.filter.expression.Expression

  val gtTextVendorOpts = Seq(
    "-gt-label-padding" -> "spaceAround",
    "-gt-label-group" -> "group",
    "-gt-label-max-displacement" -> "maxDisplacement",
    "-gt-label-min-group-distance" -> "minGroupDistance",
    "-gt-label-repeat" -> "repeat",
    "-gt-label-all-group" -> "allGroup",
    "-gt-label-remove-overlaps" -> "removeOverlaps",
    "-gt-label-allow-overruns" -> "allowOverrun",
    "-gt-label-follow-line" -> "followLine",
    "-gt-label-max-angle-delta" -> "maxAngleDelta",
    "-gt-label-auto-wrap" -> "autoWrap",
    "-gt-label-force-ltr" -> "forceLeftToRight",
    "-gt-label-conflict-resolution" -> "conflictResolution",
    "-gt-label-fit-goodness" -> "goodnessOfFit",
    "-gt-shield-resize" -> "graphic-resize",
    "-gt-shield-margin" -> "graphic-margin"
  )
  
  val gtPolygonVendorOpts = Seq(
    "-gt-graphic-margin" -> "graphic-margin",
    "-gt-fill-label-obstacle" -> "labelObstacle"
  )
  
  val gtPointVendorOpts = Seq(
    "-gt-mark-label-obstacle" -> "labelObstacle"
  )
  
  val gtLineVendorOpts = Seq(
     "-gt-stroke-label-obstacle" -> "labelObstacle"
  )

  private val defaultRGB = filters.literal(colors("grey"))

  def resolve(path: String): String = {
    val base = baseURL.getOrElse(new java.net.URL("file://"))
    new java.net.URL(base, path).toString
  }

// externalGraphic, well-known graphic , color
  def fill(xs: Seq[Value]): (Option[String], Option[String], Option[OGCExpression]) = {
    (xs take 2) match {
      case Seq(URL(url), Color(color)) =>
        (Some(resolve(url)), None, Some(color))
      case Seq(URL(url)) =>
        (Some(resolve(url)), None, None)
      case Seq(Symbol(sym)) =>
        (None, Some(sym), None)
      case Seq(Color(color)) =>
        (None, None, Some(color))
      case _ =>
        (None, None, Some(defaultRGB))
    }
  }

  def buildGraphic(
    prefix: String,
    props: Map[String, Seq[Value]],
    markProps: Seq[Property]
  ): Option[gt.Graphic] = {
    def p(name: String) = 
      props.get(prefix + "-" + name) orElse
      markProps.find(_.name == name).flatMap(_.values.headOption)

    val (url, wellKnownName, _) = fill(props(prefix))
    val mimetype = p("mime") flatMap keyword
    val size = p("size") flatMap length
    val rotation = p("rotation") flatMap angle
    val opacity = p("opacity") flatMap scale

    val mark = buildMark(
      wellKnownName,
      size,
      rotation getOrElse filters.literal(0),
      markProps
    )

    val externalGraphic = buildExternalGraphic(url, mimetype)

    if (mark.isDefined || externalGraphic.isDefined)
      Some(styles.createGraphic(
        externalGraphic.orNull,
        mark.orNull,
        null,
        opacity.orNull,
        size.orNull,
        rotation.orNull
      ))
    else
      None
  }

  def buildMark(
    markName: Option[String], 
    width: Option[OGCExpression], 
    rotation: OGCExpression,
    markProps: Seq[Property]
  ): Option[Array[gt.Mark]] =
    for (wellKnownName <- markName) yield {
      val strokeAndFill = (
        expand(markProps, "stroke").headOption.map(extractStroke(_, Nil)),
        expand(markProps, "fill").headOption.map(extractFill(_, Nil))
      )

      val (stroke, fill) = strokeAndFill match {
        case (None, None) =>
          (styles.getDefaultStroke(), styles.getDefaultFill())
        case (s, f) => (s.orNull, f.orNull)
      }

      Array(styles.createMark(
        filters.literal(wellKnownName),
        stroke,
        fill,
        filters.literal(16),
        filters.literal(0)
      ))
    } 

  def buildExternalGraphic(url: Option[String], mimetype: Option[String])
  : Option[Array[gt.ExternalGraphic]] =
    for { u <- url } 
    yield Array(styles.createExternalGraphic(u, mimetype.getOrElse("image/jpeg")))

  def color(v: Value) = Color.unapply(v).getOrElse(defaultRGB) 

  def angle(xs: Seq[Value]): Option[OGCExpression] =
    xs match {
      case Seq(Literal(body), _*) => Some(filters.literal(body.replaceFirst("deg$", "")))
      case Seq(Expression(cql), _*) => Some(org.geotools.filter.text.ecql.ECQL.toExpression(cql))
      case _ => None
    }

  def length(xs: Seq[Value]): Option[OGCExpression] = 
    xs match {
      case Seq(Literal(body), _*) => Some(filters.literal(body.replaceFirst("px$", "")))
      case Seq(Expression(cql), _*) => Some(org.geotools.filter.text.ecql.ECQL.toExpression(cql))
      case _ => None
    }

  def expression(xs: Seq[Value]): Option[OGCExpression] = 
    xs match {
      case Seq(Literal(body), _*) => Some(filters.literal(body))
      case Seq(Expression(cql), _*) => Some(org.geotools.filter.text.ecql.ECQL.toExpression(cql))
      case _ => None
    }

  def concatenatedExpression(xs: Seq[Value]): OGCExpression =
    xs collect {
      case Literal(body) => filters.literal(body)
      case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    } reduceLeft { filters.function("strConcat", _, _) }

  def keyword(xs: Seq[Value]): Option[String] = 
    xs match {
      case Seq(Literal(body), _*) => Some(body)
      case _ => None
    }

  def getLiteralDouble(xs: Seq[Value]): Option[Double] =
    Some(xs) collect {
      case Seq(Literal(Double(z))) => z
    }

  def scale(s: String): Float = {
    if (s.endsWith("%")) s.replaceFirst("%$","").toFloat / 100.0f
    else s.toFloat
  }

  def scale(xs: Seq[Value]): Option[OGCExpression] = 
    xs match {
      case Seq(Literal(l), _*) => Some(filters.literal(scale(l)))
      case Seq(Expression(cql), _*) => Some(org.geotools.filter.text.ecql.ECQL.toExpression(cql))
      case _ => None
    }

  def anchor(xs: Seq[Value]): Option[gt.AnchorPoint] = {
    object Offset {
      def unapply(v: Value): Option[OGCExpression] =
        Some(v) collect {
          case Literal(l) => filters.literal(scale(l))
          case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
        }
    }

    xs match {
      case Seq(Offset(x), Offset(y)) => Some(styles.createAnchorPoint(x, y))
      case _ => None
    }
  }

  def displacement(xs: Seq[Value]): Seq[Option[OGCExpression]] = 
    xs map {
      case Literal(body) =>
        Some(filters.literal(body.replaceFirst("px$", "")))
      case Expression(cql) =>
        Some(org.geotools.filter.text.ecql.ECQL.toExpression(cql))
      case _ => None
    }

  def lengthArray(xs: Seq[Value]): Array[Float] = {
    xs.flatMap(_ match {
      case Literal(body) => Some(body.toFloat)
      case _ => None
    }).toArray
  }

  def channelSelection(xs: Seq[Value]): gt.ChannelSelection = {
    object Channel {
      def unapply(v: Value): Option[gt.SelectedChannelType] = 
        v match {
          case Literal("auto") => None
          case Literal(text) => 
            Some(styles.createSelectedChannelType(
              text, null: gt.ContrastEnhancement))
          case _ => None
        }
    }

    val channels = 
      Some(xs) collect {
        case Seq(Channel(grey)) => Array(grey)
        case Seq(Channel(r), Channel(g), Channel(b)) => Array(r, g, b)
      }

    channels.map(styles.createChannelSelection).orNull // TODO: return Option[ChannelSelection] instead.
  }

  object Double {
    def unapply(s: String): Option[Double] =
      try
        Some(s.toDouble)
      catch {
        case (_: NumberFormatException) => None
      }
  }

  def colorMap(rampType: Option[Int])(xs: Seq[Value]): Option[gt.ColorMap] = {
    def getColorMapEntry(c: OGCExpression, v: Double, o: Double) = {
      val e = styles.createColorMapEntry
      e.setColor(c)
      e.setQuantity(filters.literal(v))
      e.setOpacity(filters.literal(o))
      e
    }
    def getColorMap(entries: Seq[gt.ColorMapEntry]) = {
      val m = styles.createColorMap
      rampType.foreach(m.setType)
      entries.foreach(m.addColorMapEntry)
      m
    }
    def tryEntry(v: Value): Option[gt.ColorMapEntry] = 
      Some(v) collect {
        case Function("color-map-entry", Seq(Color(c), Literal(Double(v)))) =>
          getColorMapEntry(c, v, 1)
        case Function("color-map-entry", Seq(Color(c), Literal(Double(v)), Literal(Double(o)))) =>
          getColorMapEntry(c, v, o)
      }

    def sequence[A](as: Seq[Option[A]]): Option[Seq[A]] =
      (as foldRight (Some(Nil): Option[Seq[A]])) {
        (aOpt, accum) => for (a <- aOpt; res <- accum) yield a +: res
      }

    sequence(xs map tryEntry).map(getColorMap)
  }

  def getOverlapBehavior(xs: Seq[Value]): Option[org.opengis.style.OverlapBehavior] = {
    import scala.util.control.Exception.catching
    xs.headOption flatMap {
      case Literal(name) => 
        catching(classOf[IllegalArgumentException]).opt {
          org.opengis.style.OverlapBehavior.valueOf(name)
        }
      case _ => None
    }
  }

  def getContrastMethod(xs: Seq[Value]): Option[org.opengis.style.ContrastMethod] =
    xs.headOption collect {
      case Literal("none") => org.opengis.style.ContrastMethod.NONE
      case Literal("normalize") => org.opengis.style.ContrastMethod.NORMALIZE
      case Literal("histogram") => org.opengis.style.ContrastMethod.HISTOGRAM
    }

  def getColorMapType(xs: Seq[Value]): Option[Int] =
    xs.headOption collect {
      case Literal("ramp") => org.geotools.styling.ColorMap.TYPE_RAMP
      case Literal("intervals") => org.geotools.styling.ColorMap.TYPE_INTERVALS
      case Literal("values") => org.geotools.styling.ColorMap.TYPE_VALUES
    }

  def valToExpression(v: Value): Option[OGCExpression] =
    v match {
      case Expression(cql) =>
        Some(org.geotools.filter.text.ecql.ECQL.toExpression(cql))
      case l: Literal =>
        Some(filters.literal(l.body))
      case _ => None
    }

  def extractFill(props: Map[String, Seq[Value]], markProps: Seq[Property]) = {
    val (externalGraphicUrl, wellKnownMarkName, color) = fill(props("fill"))
    val size = props.get("fill-size") flatMap length
    val rotation = props.get("fill-rotation") flatMap angle
    val opacity = props.get("fill-opacity") flatMap scale

    val graphic = {
      val mark = buildMark(
        wellKnownMarkName,
        size,
        rotation.getOrElse(filters.literal(0)),
        Nil
      )

      val externalGraphic =
        buildExternalGraphic(externalGraphicUrl, props.get("fill-mime").flatMap(keyword))

      if (mark.isDefined || externalGraphic.isDefined) {
        styles.createGraphic(
          externalGraphic.orNull,
          mark.orNull,
          null,
          null,
          size.orNull,
          rotation.orNull
        )
      } else null
    }

    styles.createFill(color.orNull, null, opacity.orNull, graphic)
  }

  def extractStroke(props: Map[String, Seq[Value]], markProps: Seq[Property]) = {
    val (externalGraphicUrl, wellKnownMarkName, color) = fill(props("stroke"))
    val dashArray = props.get("stroke-dasharray") map lengthArray
    val dashOffset = props.get("stroke-dashoffset") flatMap length
    val linecap = props.get("stroke-linecap") flatMap expression
    val linejoin = props.get("stroke-linejoin") flatMap expression
    val opacity = props.get("stroke-opacity") flatMap scale
    val width = props.get("stroke-width") flatMap length
    val strokeRepeat = props.get("stroke-repeat") flatMap keyword getOrElse "repeat"
    val rotation = props.get("stroke-rotation") flatMap angle getOrElse filters.literal(0)

    val graphic = {
      val mark = buildMark(
        wellKnownMarkName,
        width,
        filters.literal(0),
        Nil
      )

      val externalGraphic =
        buildExternalGraphic(
        externalGraphicUrl,
        props.get("stroke-mime").flatMap(keyword)
      )

      if (mark.isDefined || externalGraphic.isDefined) {
        styles.createGraphic(externalGraphic.orNull, mark.orNull, null, null, null, rotation)
      } else null
    }

    val graphicStroke = if (strokeRepeat == "repeat") graphic else null
    val graphicFill = if (strokeRepeat == "stipple") graphic else null

    styles.createStroke(
      color.orNull,
      width.getOrElse(filters.literal(1)),
      opacity.orNull,
      linejoin.orNull,
      linecap.orNull,
      dashArray.orNull,
      dashOffset.orNull,
      graphicFill,
      graphicStroke
    )
  }

  /**
   * Convert a set of properties to a set of Symbolizer objects attached to the
   * given Rule.
   */
  def symbolize(rule: Rule): Seq[Pair[Double, Symbolizer]] = {
    val properties = rule.properties

    def orderedMarkRules(symbolizerType: String, order: Int): Seq[Property] =
      rule.context(symbolizerType, order)
      
    /**
     * Applies the specified vendor options to the symbolizer, taking them from the collected properties values
     */
    def applyVendorOptions(sym: Symbolizer, props: Map[String, Seq[Value]], vendorOptions: Seq[(String, String)]): Unit = {
      for {
        (cssName, sldName) <- vendorOptions
        value <- props.get(cssName)
      } {
        sym.getOptions().put(
          sldName,
          value.collect({ case Literal(x) => x }).mkString(" ")
        )
      }
    }

    val lineSyms: Seq[(Double, LineSymbolizer)] = 
      (expand(properties, "stroke").toStream zip
       (Stream.from(1) map { orderedMarkRules("stroke", _) })
      ).map { case (props, markProps) =>
        val (_, _, stroke) = fill(props("stroke"))
        val dashArray = props.get("stroke-dasharray") map lengthArray
        val dashOffset = props.get("stroke-dashoffset") flatMap length
        val linecap = props.get("stroke-linecap") flatMap expression
        val linejoin = props.get("stroke-linejoin") flatMap expression
        val miterLimit = props.get("stroke-miterlimit")
        val opacity = props.get("stroke-opacity") flatMap scale
        val width = props.get("stroke-width") flatMap length
        val strokeRepeat = props.get("stroke-repeat") flatMap keyword getOrElse "repeat"
        val rotation = props.get("stroke-rotation") map angle getOrElse filters.literal(0)
        val geom = 
          props.get("stroke-geometry") orElse props.get("geometry") flatMap expression
        val zIndex: Double = 
          props.get("stroke-z-index") orElse props.get("z-index") flatMap getLiteralDouble getOrElse(0d)

        val graphic = buildGraphic("stroke", props, markProps)

        val graphicStroke = 
          for (g <- graphic if strokeRepeat == "repeat") yield g
        val graphicFill =
          for (g <- graphic if strokeRepeat == "stipple") yield g 

        val sym = 
          styles.createLineSymbolizer(
            styles.createStroke(
              stroke.orNull,
              width.getOrElse(filters.literal(1)),
              opacity.orNull,
              linejoin.orNull,
              linecap.orNull,
              dashArray.orNull,
              dashOffset.orNull,
              graphicFill.orNull,
              graphicStroke.orNull
            ),
            null
          )
        geom.foreach { sym.setGeometry }
        
        // collect the vendor options for line symbolizers
        applyVendorOptions(sym, props, gtLineVendorOpts)
        
        (zIndex, sym)
      }

    val polySyms: Seq[(Double, PolygonSymbolizer)] = 
      (expand(properties, "fill").toStream zip
       (Stream.from(1) map { orderedMarkRules("fill", _) })
      ).map { case (props, markProps) =>
        val fillParams = fill(props("fill"))
        val size = props.get("fill-size") map length
        val rotation = props.get("fill-rotation") map angle
        val opacity = props.get("fill-opacity") flatMap scale
        val geom =
          props.get("fill-geometry") orElse props.get("geometry") flatMap expression
        val zIndex: Double = 
          props.get("fill-z-index") orElse props.get("z-index") flatMap getLiteralDouble getOrElse(0d)

        val graphic = buildGraphic("fill", props, markProps) 

        val sym = styles.createPolygonSymbolizer(
          null,
          styles.createFill(
            fillParams._3.orNull,
            null,
            opacity.orNull,
            graphic.orNull
          ),
          null
        )
        geom.foreach { sym.setGeometry(_) }
        
        // collect the vendor options for polygon symbolizers
        applyVendorOptions(sym, props, gtPolygonVendorOpts)
        
        (zIndex, sym)
      }

    val pointSyms: Seq[(Double, PointSymbolizer)] = 
      (expand(properties, "mark").toStream zip
       (Stream.from(1) map { orderedMarkRules("mark", _) })
      ).flatMap { case (props, markProps) => 
        val geom = (props.get("mark-geometry") orElse props.get("geometry"))
          .flatMap(expression)
        val zIndex: Double = 
          props.get("mark-z-index") orElse props.get("z-index") flatMap getLiteralDouble getOrElse(0d)

        val graphic = buildGraphic("mark", props, markProps)

        for (g <- graphic) yield {
          val sym = styles.createPointSymbolizer(g, null)
          geom.foreach { sym.setGeometry(_) }
          
          // collect the vendor options for point symbolizers
          applyVendorOptions(sym, props, gtPointVendorOpts)
          
          (zIndex, sym)
        }
      }

    val textSyms: Seq[(Double, TextSymbolizer)] =
      (expand(properties, "label").toStream zip
       (Stream.from(1) map { orderedMarkRules("shield", _) })
      ).map { case (props, shieldProps) => 
        val fillParams = props.get("font-fill").map(fill)
        val fontFamily = props.get("font-family")
        val fontOpacity = props.get("font-opacity").flatMap(scale)
        val anchorPoint = props.get("label-anchor").flatMap(anchor)
        val offset = props.get("label-offset").map(displacement)
        val rotation = props.get("label-rotation").flatMap(angle)
        val geom = (props.get("label-geometry") orElse props.get("geometry"))
          .flatMap(expression)
        val zIndex: Double = 
          props.get("label-z-index") orElse props.get("z-index") flatMap(getLiteralDouble) getOrElse 0d

        val font = fontFamily.getOrElse(Nil).flatMap(valToExpression).map { familyName => {
          val fontStyle =
            props.get("font-style").flatMap(expression).getOrElse(filters.literal("normal"))
          val fontWeight =
            props.get("font-weight").flatMap(expression).getOrElse(filters.literal("normal"))
          val fontSize =
            props.get("font-size").flatMap(length).getOrElse(filters.literal("10"))
          styles.createFont(familyName, fontStyle, fontWeight, fontSize)
        }}.toArray

        val fontFill = fillParams.map(fillParams => {
          val mark = buildMark(
            fillParams._2,
            Some(filters.literal(16)),
            filters.literal(0),
            Nil // yeah we're not going to support well-known marks for font fills yet.
          )
          val externalGraphic =
            buildExternalGraphic(fillParams._1, props.get("fill-mime").flatMap(keyword))
          if (mark.isDefined || externalGraphic.isDefined) {
            styles.createGraphic(
              externalGraphic.orNull,
              mark.orNull,
              null,
              null,
              null,
              null
            )
          } else null
        }).orNull

        val haloRadius = props.get("halo-radius").flatMap(length)

        val halo = if (haloRadius.isDefined) {
          val haloColor = props.get("halo-color").map(x => color(x.head))
          val haloOpacity = props.get("halo-opacity").flatMap(scale)
          styles.createHalo(
            styles.createFill(haloColor.orNull, haloOpacity.orNull),
            haloRadius.get
          )
        } else null

        val shield = 
          if (props contains "shield")
            buildGraphic("shield", props, shieldProps)
          else
            None

        val linePlacementOption = 
          offset.collect {
            case Seq(Some(d)) => styles.createLinePlacement(d)
          }

        val pointPlacementOption = 
          offset.collect {
            case Seq(Some(x), Some(y)) =>
              styles.createPointPlacement(
                anchorPoint.getOrElse(styles.getDefaultPointPlacement.getAnchorPoint),
                styles.createDisplacement(x, y),
                rotation.getOrElse(styles.getDefaultPointPlacement.getRotation))
          }

        val anchorPlacementOption =
          anchorPoint.map { anchor =>
            styles.createPointPlacement(
              anchor, 
              styles.getDefaultPointPlacement.getDisplacement,
              rotation.getOrElse(styles.getDefaultPointPlacement.getRotation))
          }

        val placement = 
          linePlacementOption orElse pointPlacementOption orElse anchorPlacementOption
       //  offset match {
       //    case Some(Seq(Some(d))) => styles.createLinePlacement(d)
       //    case Some(Seq(Some(x), Some(y))) =>
       //      styles.createPointPlacement(
       //        anchorPoint.getOrElse(styles.getDefaultPointPlacement().getAnchorPoint()),
       //        styles.createDisplacement(x, y),
       //        rotation.getOrElse(styles.getDefaultPointPlacement().getRotation())
       //      )
       //    case _ => null
       //  }

        val sym = styles.createTextSymbolizer(
          styles.createFill(fillParams.flatMap(_._3).orNull, null, fontOpacity.getOrElse(null), fontFill),
          font,
          halo,
          concatenatedExpression(props("label")),
          placement.orNull,
          null  //the geometry, but only as a string. the setter accepts an expression so we use that instead
        )
        geom.foreach { sym.setGeometry(_) }

        // Looks like, depending on GeoTools configuration, this might not be
        // the sort of Symbolizer which supports graphics. Let's at least not
        // cast unless we need to.  
        // TODO: see if there's a nicer way to deal with this that
        shield.foreach { sym.asInstanceOf[TextSymbolizer2].setGraphic(_) }

        for (priority <- props.get("-gt-label-priority") flatMap expression) {
          sym.setPriority(priority)
        }

        // collect the vendor options for text symbolizers
        applyVendorOptions(sym, props, gtTextVendorOpts)

        (zIndex, sym)
      }

    val rasterSyms: Seq[(Double, RasterSymbolizer)] =
      (expand(properties, "raster-channels").toStream zip
       (Stream.from(1) map { orderedMarkRules("outline", _) })
      ).map { case (props, outlineProps) =>
        val geom = 
          (props get "raster-geometry")
            .orElse(props get "geometry")
            .flatMap(expression)
        val opacity =
          (props get "raster-opacity") flatMap expression
        val channels =
          (props get "raster-channels") map channelSelection
        val overlap = (props get "raster-overlap-behavior") flatMap getOverlapBehavior
        val colorMapType = (props get "raster-color-map-type") flatMap getColorMapType
        val colorMapEntries =
          (props get "raster-color-map") flatMap colorMap(colorMapType)
        val contrastMethod =
          (props get "raster-contrast-enhancement") flatMap getContrastMethod
        val gamma = 
          (props get "raster-gamma") flatMap getLiteralDouble
        val relief = (null: org.geotools.styling.ShadedRelief)
        val outline = (null: Symbolizer)
        val zIndex: Double = 
          (props get "raster-z-index")
            .orElse(props get "z-index")
            .flatMap(getLiteralDouble)
            .getOrElse(0d)

        val contrastEnhancement = styles.createContrastEnhancement()
        contrastMethod.foreach(contrastEnhancement.setMethod)
        gamma.foreach(g => contrastEnhancement.setGammaValue(filters.literal(g)))

        val sym = styles.createRasterSymbolizer(
          null, // This should be the geometry property, but it only accepts a string so we use setGeometry() after creation to pass an expression.
          opacity.orNull,
          channels.orNull,
          overlap.map(filters.literal).orNull,
          colorMapEntries.orNull,
          contrastEnhancement,
          relief,
          outline)
        sym.setGeometry(geom.orNull)
        (zIndex, sym)
      }

    Seq(polySyms, lineSyms, pointSyms, textSyms, rasterSyms).flatten
  }

  type StyleSheet = Seq[Rule]
  type ZIndex = Double

  /**
   * Convert a list of tokens (as generated by CssParser.parse) into a GeoTools
   * Style object.  This works by creating individual rules for every possible
   * combination of rules from the input stylesheet, so it takes exponential
   * time (O(2^N) for N= number of input rules).  Be warned.
   *
   * @see org.geotools.styling.SLDTransformer if you want to serialize the
   *   resultant Style object to an XML file
   */
  def css2sld(s: StyleSheet): gt.Style = {
    val sorted: StyleSheet = sort(s)
    val byType: Seq[(Option[String], StyleSheet)] = splitOnType(sorted)
    val resolved: Seq[(Option[String], StyleSheet)] =
      for ((t, rs) <- byType) yield (t, cascading2exclusive(rs))
    val withSymbolizers: Seq[(Option[String], Seq[(ZIndex, Rule, Seq[Symbolizer])])] =
      for ((t, rs) <- resolved) yield (t, interpret(rs))
    val gtRules: Seq[(Option[String], Seq[Seq[gt.Rule]])] =
      for ((t, rs) <- withSymbolizers) yield (t, createSLDRuleLayers(rs))
    val featureTypeStyles: Seq[gt.FeatureTypeStyle] =
      gtRules.flatMap(createFeatureTypeStyles)
    createStyle(featureTypeStyles)
  }

  def sort(s: StyleSheet): StyleSheet =
    stableSort(s, (x: Rule, y: Rule) => Specificity.order(y, x))

  def splitOnType(s: StyleSheet): Seq[(Option[String], StyleSheet)] = {
    def isForTypename(t: Option[String])(r: Rule): Boolean =
      t.forall(name => reduce(allOf(Typename(name) +: r.selectors)) != Exclude)

    def extractTypename(r: Rule): Option[String] = 
      flatten(And(r.selectors)).collectFirst { case Typename(t) => t }

    def stripTypenames(r: Rule): Rule = {
      val selectors0 = r.selectors.map {
        case Typename(_) => Accept
        case selector => selector
      }

      r.copy(selectors = selectors0)
    }

    val typenames = (s map extractTypename).distinct

    for (t <- typenames)
    yield (t, s.filter(isForTypename(t)).map(stripTypenames))
  }

  def extractScaleRanges(rule: Rule): Seq[Pair[Option[Double], Option[Double]]] = {
    val scales = 
      flatten(And(rule.selectors))
        .collect { 
          case PseudoSelector("scale", _, d) => d
          case Not(PseudoSelector("scale", _, d)) => d
        }
        .sorted
        .distinct

    val limits = None +: (scales map (Some(_))) :+ None
    limits zip limits.tail
  }

  def interpret(s: StyleSheet): Seq[(ZIndex, Rule, Seq[Symbolizer])] = 
    for {
      r <- s
      (z, syms) <- groupByZ(symbolize(r))
    } yield (z, r, syms)

  def createSLDRuleLayers(rs: Seq[(ZIndex, Rule, Seq[Symbolizer])]): Seq[Seq[gt.Rule]] =
    for ((_, group) <- flattenByZ(rs)) yield createSLDRuleLayer(group)

  def createSLDRuleLayer(group: Seq[(Rule, Seq[Symbolizer])]): Seq[gt.Rule] =
    for {
      (rule, syms) <- group if syms.nonEmpty
      range @ (min, max) <- extractScaleRanges(rule)
      minSelector = min.map(x => PseudoSelector("scale", ">", x))
      maxSelector = max.map(x => PseudoSelector("scale", "<", x))
      filter = reduce(allOf(rule.selectors ++ minSelector ++ maxSelector))
      if (filter != Exclude)
    } yield createSLDRule(min, max, realize(filter), rule.description.title, rule.description.abstrakt, syms)

  def createSLDRule(
    min: Option[Double],
    max: Option[Double],
    filter: Option[org.opengis.filter.Filter],
    title: Option[String],
    `abstract`: Option[String],
    symbolizers: Seq[gt.Symbolizer]): gt.Rule =
  {
    val rule = styles.createRule()
    min.foreach { rule.setMinScaleDenominator }
    max.foreach { rule.setMaxScaleDenominator }
    filter.foreach { rule.setFilter }
    title.foreach { t => rule.getDescription().setTitle(t) }
    `abstract`.foreach { a => rule.getDescription().setAbstract(a) }
    symbolizers.foreach { rule.symbolizers.add }
    rule
  }

  def createFeatureTypeStyles(spec: (Option[String], Seq[Seq[gt.Rule]])): Seq[gt.FeatureTypeStyle] =
    spec._2.map { rules => 
      createFeatureTypeStyle((spec._1, rules))
    }

  def createFeatureTypeStyle(spec: (Option[String], Seq[gt.Rule])): gt.FeatureTypeStyle = {
    val (typename, rules) = spec
    val ftStyle = styles.createFeatureTypeStyle()
    typename.foreach { t => ftStyle.featureTypeNames().add(new NameImpl(t)) }
    rules.foreach { ftStyle.rules.add }
    ftStyle
  }

  def createStyle(fts: Seq[gt.FeatureTypeStyle]): gt.Style = {
    val style = styles.createStyle()
    fts.foreach { style.featureTypeStyles.add }
    style
  }

  private def flattenByZ[R, S](zGroups: Seq[(Double, R, Seq[S])])
  : Seq[(Double, Seq[(R, Seq[S])])]
  = {
    val zFlattened = zGroups map { case (z, r, s) => (z, (r, s)) }
    zFlattened
      .groupBy(_._1)
      .mapValues(_ map (_._2))
      .toSeq
      .sortBy(_._1)
  }


  private def groupByZ(syms: Seq[(Double, Symbolizer)])
  : Seq[(Double, Seq[Symbolizer])] = {
    // we make a special case for labels; they will be rendered last anyway, so
    // we can fold them into one layer
    val (labels, symbols) = syms partition { _.isInstanceOf[TextSymbolizer] }
    val grouped =
      for {
        (z, syms) <- symbols.groupBy(_._1).toSeq.sortBy(_._1)
      } yield (z, syms map (_._2))
    grouped ++ Seq((0d, labels map (_._2)))
  }

  def simplifyList(sels: Seq[Selector]): Seq[Selector] = {
    if (sels.isEmpty) Seq()
    else {
      val reduced = 
        sels.map(consolidate).reduce {
          (a,b) => reduce[Selector](And(Seq(a, b)))
        }
      reduced match {
        case And(sels) => sels
        case sel       => Seq(sel)
      }
    }
  }

  def consolidate(s: Selector): Selector = {
    def f(s: Selector): Seq[Selector] =
      s match {
        case And(children) => 
          val children0 = children flatMap f
          Seq(
            children0 match {
              case Seq() => Accept
              case Seq(s) => s
              case children0 => And(children0)
            }
          )
        case Or(children) =>
          val children0 = children flatMap f
          Seq(
            children0 match {
              case Seq() => Exclude
              case Seq(s) => s
              case children0 => Or(children0)
            }
          )
        case Not(Not(child)) => Seq(consolidate(child))
        case Not(child) => Seq(Not(consolidate(child)))
        case p => Seq(p)
      }

    f(s).head
  }

  def simplifySelector(r: Rule): Rule =
    r.copy(selectors = simplifyList(r.selectors))

  def merge(a: Rule, b: Rule): Rule = (a merge b)

  def constrain(a: Rule, b: Seq[Selector]): Rule =
    a.copy(selectors = (a.selectors ++ b))

  def cascading2exclusive(xs: Seq[Rule]): Seq[Rule] = {
    import org.geoscript.support.graph._

    val mutuallyExclusive = (a: Rule, b: Rule) =>
      reduce[Selector](And(a.selectors ++ b.selectors)) == Exclude
     
    val cliques = maximalCliques(xs.toSet, mutuallyExclusive)
    val combinations = visit(cliques).map(_.toSet).toSet
    // val oldCombinations = enumerateCombinations(cliques)
    // println(s"${oldCombinations.size}, ${combinations.size}")

    val ExclusiveRule = EmptyRule.copy(selectors = Seq(Exclude))

    val negate = (x: Rule) =>
      x.copy(selectors = Seq(Not(And(x.selectors))))
    val include = (in: Set[Rule]) =>
      if (in.isEmpty) ExclusiveRule else (xs.view filter(in) reduceLeft(merge))
    val exclude = (xs: Seq[Rule]) =>
      xs.map { r => Not(And(r.selectors)) }

    val rulesets = 
      for {
        combo <- combinations
        remainder = xs filterNot(combo contains _)
        included = include(combo.toSet)
        excluded = exclude(remainder)
        constrained = constrain(included, excluded)
        ruleset = simplifySelector(constrained)
        if ruleset.isSatisfiable 
      } yield ruleset

    // println(rulesets.size)
    // rulesets.toSeq
    Nil
    rulesets.toSeq
  }

  import scala.annotation.tailrec

  // @tailrec
  def visit(cliques: Set[Set[Rule]]): Seq[List[Rule]] = {
    def work[Rule](path: List[Rule], cliques: List[Set[Rule]]): Seq[List[Rule]] =
        cliques match {
          case Nil => 
            Seq(path)
          case top :: remainingCliques =>
            val includingThisLevel: Seq[List[Rule]] = 
              top.toSeq flatMap { i =>
                val culledRemaining = 
                  remainingCliques.filterNot(_ contains i).map(_ -- top)
                work(i :: path, culledRemaining) // remainingCliques filterNot (_ contains i))
              }
           val excludingThisLevel: Seq[List[Rule]] =
             work(path, remainingCliques)

          includingThisLevel ++ excludingThisLevel
        }
    work(Nil, cliques.toList.sortBy(- _.size))
  }
}
