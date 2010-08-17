package org.geoscript.geocss

import math._
import util.Sorting.stableSort

import org.geoscript.geocss.filter.FilterOps

import org.geotools.feature.NameImpl
import org.geotools.{styling => gt}
import org.geotools.styling.{
  LineSymbolizer,
  PointSymbolizer,
  PolygonSymbolizer,
  Symbolizer,
  TextSymbolizer,
  FeatureTypeStyle
}

/**
 * The Translator object houses some facilities for converting token lists to
 * GeoTools Style objects.  The css2sld method does the actual conversion.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
object Translator { //  extends CssOps with SelectorOps {
  import SelectorOps.{ Exclude, negate, simplify }
  import CssOps.{ Color, Specificity, Symbol, URL, colors, expand }
  import FilterOps.{ filters }
  val styles = org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)
  type OGCExpression = org.opengis.filter.expression.Expression

  val gtVendorOpts = List(
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
    "-gt-label-fit-goodness" -> "goodnessOfFit"
  )

  private val defaultRGB = filters.literal(colors("grey"))

// externalGraphic, well-known graphic , color
  def fill(xs: List[Value]): (String, String, OGCExpression) = {
    val wellKnownMarks =
      List("circle", "square", "triangle", "star", "arrow", "hatch", "x")

    (xs take 2) match {
      case URL(url) :: Color(color) :: Nil => (url, null, filters.literal(color))
      case URL(url) :: Nil => (url, null, defaultRGB)
      case Symbol(sym) :: _ => (null, sym, defaultRGB)
      case Color(color) :: _  => (null, null, filters.literal(color))
      case _ => (null, null, defaultRGB)
    }
  }

  def buildGraphic(
    prefix: String,
    props: Map[String, List[Value]],
    markProps: List[Property]
  ): gt.Graphic = {
    def p(name: String) = 
      props.get(prefix + "-" + name) orElse
      markProps.find(_.name == name).flatMap(_.values.headOption)

    val (url, wellKnownName, _) = fill(props(prefix))
    val mimetype = p("mime") map keyword 
    val size = p("size") map length
    val rotation = p("rotation") map angle
    val opacity = p("opacity") map scale getOrElse null

    val mark = buildMark(
      wellKnownName,
      size,
      rotation getOrElse filters.literal(0),
      markProps
    )

    val externalGraphic = buildExternalGraphic(url, mimetype)

    if (mark != null || externalGraphic != null) {
      styles.createGraphic(
        externalGraphic,
        mark,
        null,
        opacity,
        size getOrElse null,
        rotation getOrElse null
      )
    } else { 
      null
    }
  }

  def buildMark(
    markName: String, 
    width: Option[OGCExpression], 
    rotation: OGCExpression,
    markProps: List[Property]
  ): Array[gt.Mark] = {
    if (markName != null) {
      val strokeAndFill = (
        expand(markProps, "stroke").headOption.map(extractStroke(_, Nil)),
        expand(markProps, "fill").headOption.map(extractFill(_, Nil))
      )

      val (stroke, fill) = strokeAndFill match {
        case (None, None) =>
          (styles.getDefaultStroke(), styles.getDefaultFill())
        case (s, f) => (s.getOrElse(null), f.getOrElse(null))
      }

      Array(styles.createMark(
        filters.literal(markName),
        stroke,
        fill,
        filters.literal(16),
        filters.literal(0)
      ))
    } else {
      null
    }
  }

  def buildExternalGraphic(url: String, mimetype: Option[String]) = {
    if (url != null) {
      Array(styles.createExternalGraphic(url, mimetype.getOrElse("image/jpeg")))
    } else null
  }

  def color(v: Value) = v match {
    case Color(c) => filters.literal(c)
    case _ => defaultRGB
  }

  // GeoTools only knows about #RRGGBB, but CSS lets you use nice names as well...
  def color(p: Property) = p.values.head.head match {
    case Literal(body) => filters.literal(colors.getOrElse(body, body))
    case _ => null
  }

  def angle(xs: List[Value]) = xs.head match {
    case l: Literal =>
      filters.literal(l.body.replaceFirst("deg$", ""))
    case Expression(cql) =>
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def length(xs: List[Value]): OGCExpression = xs.head match {
    case Literal(body) =>
      filters.literal(body.replaceFirst("px$", ""))
    case Expression(cql) =>
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def expression(xs: List[Value]): OGCExpression = xs.head match {
    case Literal(body) => filters.literal(body)
    case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def keyword(default: String, xs: List[Value]): String = xs.head match {
    case Literal(body) => body
    case _ => default
  }

  def keyword(xs: List[Value]): String = keyword(null:String, xs)

  def scale(s: String): Float = {
    if (s.endsWith("%")) s.replaceFirst("%$","").toFloat / 100.0f
    else s.toFloat
  }

  def scale(xs: List[Value]): OGCExpression = xs.head match {
    case Literal(l) => filters.literal(scale(l))
    case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def anchor(xs: List[Value]): gt.AnchorPoint = xs map {
    case Literal(l) => filters.literal(scale(l))
    case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  } take 2 match {
    case List(x, y) => styles.createAnchorPoint(x, y)
    case _ => null
  }

  def displacement(xs: List[Value]): List[OGCExpression] = xs map {
    case Literal(body) =>
      filters.literal(body.replaceFirst("px$", ""))
    case Expression(cql) =>
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def lengthArray(xs: List[Value]): Array[Float] = {
    xs.flatMap(_ match {
      case Literal(body) => Some(body.toFloat)
      case _ => None
    }).toArray
  }

  implicit def stringToFilter(literal: String): org.opengis.filter.Filter = {
    val cql = literal.substring(1, literal.length - 1)
    org.geotools.filter.text.ecql.ECQL.toFilter(cql)
  }

  /**
   * A conversion from Strings to filter Expressions using the ECQL
   * parser.  Strings are assumed to be ECQL expressions if they are wrapped in
   * square braces; otherwise they are taken as literals.
   */
  def stringToExpression(literal: String): OGCExpression =
    if (literal == null) null
    else if ((literal startsWith "[") && (literal endsWith "]")) {
      val cql = literal.substring(1, literal.length - 1)
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    } else {
      filters.literal(literal)
    }

  implicit def valToExpression(v: Value): OGCExpression =
    v match {
      case Expression(cql) =>
        org.geotools.filter.text.ecql.ECQL.toExpression(cql)
      case l: Literal =>
        filters.literal(l.body)
      case _ => null
    }

  def extractFill(props: Map[String, List[Value]], markProps: List[Property]) = {
    val (externalGraphicUrl, wellKnownMarkName, color) = fill(props("fill"))
    val size = props.get("fill-size") map length
    val rotation = props.get("fill-rotation") map angle
    val opacity = props.get("fill-opacity") map scale getOrElse null

    val graphic = {
      val mark = buildMark(
        wellKnownMarkName,
        size,
        rotation.getOrElse(filters.literal(0)),
        Nil
      )

      val externalGraphic =
        buildExternalGraphic(externalGraphicUrl, props.get("fill-mime").map(keyword))

      if (mark != null || externalGraphic != null) {
        styles.createGraphic(
          externalGraphic,
          mark,
          null,
          null,
          size.getOrElse(null),
          rotation.getOrElse(null)
        )
      } else null
    }

    styles.createFill(color, null, opacity, graphic)
  }

  def extractStroke(props: Map[String, List[Value]], markProps: List[Property]) = {
    val (externalGraphicUrl, wellKnownMarkName, color) = fill(props("stroke"))
    val dashArray = props.get("stroke-dasharray") map lengthArray getOrElse null
    val dashOffset = props.get("stroke-dashoffset") map length getOrElse null
    val linecap = props.get("stroke-linecap") map expression getOrElse null
    val linejoin = props.get("stroke-linejoin") map expression getOrElse null
    val miterLimit = props.get("stroke-miterlimit") getOrElse null
    val opacity = props.get("stroke-opacity") map scale getOrElse null
    val width = props.get("stroke-width") map length
    val strokeRepeat = props.get("stroke-repeat") map keyword getOrElse "repeat"
    val rotation = props.get("stroke-rotation") map angle getOrElse filters.literal(0)

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
        props.get("stroke-mime").map(keyword)
      )

      if (mark != null || externalGraphic != null) {
        styles.createGraphic(externalGraphic, mark, null, null, null, rotation)
      } else null
    }

    val graphicStroke = if (strokeRepeat == "repeat") graphic else null
    val graphicFill = if (strokeRepeat == "stipple") graphic else null

    styles.createStroke(
      color,
      width.getOrElse(filters.literal(1)),
      opacity,
      linejoin,
      linecap,
      dashArray,
      dashOffset,
      graphicFill,
      graphicStroke
    )
  }

  /**
   * Convert a set of properties to a set of Symbolizer objects attached to the
   * given Rule.
   */
  def symbolize(rules: List[Rule]) = {
    val properties = rules flatMap { _.properties }

    def orderedMarkRules(symbolizerType: String, order: Int) =
      rules.flatMap(_.context(symbolizerType, order))

    val lineSyms: List[(Double, LineSymbolizer)] = 
      (expand(properties, "stroke").toStream zip
       (Stream.from(1) map { orderedMarkRules("stroke", _) })
      ).map { case (props, markProps) =>
        val (_, _, stroke) = fill(props("stroke"))
        val dashArray = props.get("stroke-dasharray") map lengthArray getOrElse null
        val dashOffset = props.get("stroke-dashoffset") map length getOrElse null
        val linecap = props.get("stroke-linecap") map expression getOrElse null
        val linejoin = props.get("stroke-linejoin") map expression getOrElse null
        val miterLimit = props.get("stroke-miterlimit") getOrElse null
        val opacity = props.get("stroke-opacity") map scale getOrElse null
        val width = props.get("stroke-width") map length
        val strokeRepeat = props.get("stroke-repeat") map keyword getOrElse "repeat"
        val rotation = props.get("stroke-rotation") map angle getOrElse filters.literal(0)
        val geom = 
          props.get("stroke-geometry") orElse props.get("geometry") map expression getOrElse null
        val zIndex: Double = 
          props.get("stroke-z-index") orElse props.get("z-index") map {
            x => keyword("0", x).toDouble
          } getOrElse(0d)

        val graphic = buildGraphic("stroke", props, markProps)

        val graphicStroke = if (strokeRepeat == "repeat") graphic else null
        val graphicFill = if (strokeRepeat == "stipple") graphic else null

        val sym = 
          styles.createLineSymbolizer(
            styles.createStroke(
              stroke,
              width.getOrElse(filters.literal(1)),
              opacity,
              linejoin,
              linecap,
              dashArray,
              dashOffset,
              graphicFill,
              graphicStroke
            ),
            null
          )
        sym.setGeometry(geom)
        (zIndex, sym)
      }.toList

    val polySyms: List[(Double, PolygonSymbolizer)] = 
      (expand(properties, "fill").toStream zip
       (Stream.from(1) map { orderedMarkRules("fill", _) })
      ).map { case (props, markProps) =>
        val fillParams = fill(props("fill"))
        val size = props.get("fill-size") map length
        val rotation = props.get("fill-rotation") map angle
        val opacity = props.get("fill-opacity") map scale getOrElse null
        val geom =
          props.get("fill-geometry") orElse props.get("geometry") map expression getOrElse null
        val zIndex: Double = 
          props.get("fill-z-index") orElse props.get("z-index") map {
            x => keyword("0", x).toDouble
          } getOrElse(0d)

        val graphic = buildGraphic("fill", props, markProps) 

        val sym = styles.createPolygonSymbolizer(
            null,
            styles.createFill(fillParams._3, null, opacity, graphic),
            null
          )
        sym.setGeometry(geom)
        (zIndex, sym)
      }.toList

    val pointSyms: List[(Double, PointSymbolizer)] = 
      (expand(properties, "mark").toStream zip
       (Stream.from(1) map { orderedMarkRules("mark", _) })
      ).map { case (props, markProps) => 
        val geom = (props.get("mark-geometry") orElse props.get("geometry"))
          .map(expression).getOrElse(null)
        val zIndex: Double = 
          props.get("mark-z-index") orElse props.get("z-index") map {
            x => keyword(x).toDouble
          } getOrElse(0d)

        val graphic = buildGraphic("mark", props, markProps)

        val sym = styles.createPointSymbolizer(graphic, null)
        sym.setGeometry(geom)
        (zIndex, sym)
      }.toList

    val textSyms: List[(Double, TextSymbolizer)] =
      expand(properties, "label") map { props => 
        val fillParams = props.get("font-fill").map(fill)
        val fontFamily = props.get("font-family")
        val fontOpacity = props.get("font-opacity").map(scale)
        val anchorPoint = props.get("label-anchor").map(anchor)
        val offset = props.get("label-offset").map(displacement)
        val rotation = props.get("label-rotation").map(angle)
        val geom = (props.get("label-geometry") orElse props.get("geometry"))
          .map(expression).getOrElse(null)
        val zIndex: Double = 
          props.get("label-z-index") orElse props.get("z-index") map {
            x => keyword("0", x).toDouble
          } getOrElse(0d)

        val font = fontFamily.getOrElse(Nil).map { familyName => {
          val fontStyle =
            props.get("font-style").map(expression).getOrElse(filters.literal("normal"))
          val fontWeight =
            props.get("font-weight").map(expression).getOrElse(filters.literal("normal"))
          val fontSize =
            props.get("font-size").map(length).getOrElse(filters.literal("10"))
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
            buildExternalGraphic(fillParams._1, props.get("fill-mime").map(keyword))
          if (mark != null || externalGraphic != null) {
            styles.createGraphic(
              externalGraphic,
              mark,
              null,
              null,
              null,
              null
            )
          } else null
        }).getOrElse(null)

        val haloRadius = props.get("halo-radius").map(length)

        val halo = if (haloRadius.isDefined) {
          val haloColor = props.get("halo-color")
            .map(x => color(x.head)).getOrElse(null)
          val haloOpacity = props.get("halo-opacity").map(scale).getOrElse(null)
          styles.createHalo(
            styles.createFill(haloColor, haloOpacity),
            haloRadius.get
          )
        } else null

        val placement = offset match {
          case Some(List(d)) => styles.createLinePlacement(d)
          case Some(x :: y :: Nil) =>
            styles.createPointPlacement(
              anchorPoint.getOrElse(styles.getDefaultPointPlacement().getAnchorPoint()),
              styles.createDisplacement(x, y),
              rotation.getOrElse(styles.getDefaultPointPlacement().getRotation())
            )
          case _ => null
        }

        val sym = styles.createTextSymbolizer(
          styles.createFill(fillParams.map(_._3).getOrElse(null), null, fontOpacity.getOrElse(null), fontFill),
          font,
          halo,
          expression(props("label")),
          placement,
          null  //the geometry, but only as a string. the setter accepts an expression
        )
        sym.setGeometry(geom)

        for (priority <- props.get("-gt-label-priority") map expression) {
          sym.setPriority(priority)
        }

        for (
          (cssName, sldName) <- gtVendorOpts;
          value <- props.get(cssName)
        ) {
          sym.getOptions().put(sldName, keyword(value))
        }

        (zIndex, sym)
      }

    (polySyms ++ lineSyms ++ pointSyms ++ textSyms)
  }

  /**
   * Convert a list of tokens (as generated by CssParser.parse) into a GeoTools
   * Style object.  This works by creating individual rules for every possible
   * combination of rules from the input stylesheet, so it takes exponential
   * time (O(2^N) for N= number of input rules).  Be warned.
   *
   * @see org.geotools.styling.SLDTransformer if you want to serialize the
   *   resultant Style object to an XML file
   */
  def css2sld(styleSheet: List[Rule]): gt.Style = {
    val sld = styles.createStyle

    val rules = styleSheet

    import SelectorOps._

    def extractTypeName(rule: Rule): Option[String] =
      flatten(AndSelector(rule.selectors)).collect { 
        case TypenameSelector(typename) => typename 
      } headOption

    def extractScaleRange(rule: Rule): (Option[Double], Option[Double]) = {
      val lowerBounds = 
        flatten(AndSelector(rule.selectors)).collect { 
          case PseudoSelector("scale", ">", d) => d.toDouble
        }

      val upperBounds = 
        flatten(AndSelector(rule.selectors)).collect { 
          case PseudoSelector("scale", "<", d) => d.toDouble
        }

      val bottom = for (b <- Some(lowerBounds) if b.nonEmpty) yield b.max
      val top    = for (b <- Some(upperBounds) if b.nonEmpty) yield b.min

      (bottom, top)
    }

    def isForTypename(typename: Option[String])(rule: Rule): Boolean =
      typename map { t => 
        simplify(allOf(TypenameSelector(t) +: rule.selectors)) != Empty
      } getOrElse true

    def stripTypenames(rule: Rule): Rule =
      rule.copy(selectors = rule.selectors map {
        case TypenameSelector(_) => AcceptSelector
        case selector => selector
      })

    val typenames = (rules map extractTypeName) distinct

    val styleRules = 
      for (name <- typenames) yield (name, rules filter isForTypename(name) map stripTypenames)

    for ((typename, overlays) <- styleRules) {
      val zGroups: List[List[(Double, (List[Rule], List[Rule]), List[gt.Symbolizer])]] = 
        for (rule <- cascading2exclusive(overlays))
          yield groupByZ(symbolize(rule._1)) map {
            case (z, syms) => (z, rule, syms)
          }

      for ((_, group) <- flattenByZ(zGroups)) {
        val fts = styles.createFeatureTypeStyle
        typename.foreach { t => fts.featureTypeNames.add(new NameImpl(t)) }
        for (((in, out), syms) <- group if !syms.isEmpty) {
          val rule = compose(in, out)
          val sldRule = styles.createRule()
          val (minscale, maxscale) = extractScaleRange(rule)

          for (min <- minscale) 
            sldRule.setMinScaleDenominator(min)
          for (max <- maxscale) 
            sldRule.setMaxScaleDenominator(max)
          for (title <- rule.description.title)
            sldRule.getDescription().setTitle(title)
          for (abstrakt <- rule.description.abstrakt)
            sldRule.getDescription().setAbstract(abstrakt)

          sldRule.setFilter(rule.getFilter)
          for (sym <- syms) {
            sldRule.symbolizers.add(sym)
          }
          fts.rules.add(sldRule)
        }
        sld.featureTypeStyles.add(fts)
      }
    }

    return sld 
  }

  private def flattenByZ[R](zGroups: List[List[(Double, R, List[Symbolizer])]])
  : List[(Double, List[(R, List[Symbolizer])])] 
  = {
    def ordering(a: (Double, _, _), b: (Double, _, _)): Boolean = a._1 < b._1

    def foldUp[A, B](xs: List[(_, A, List[B])]): List[(A, List[B])] = {
      (xs foldLeft (Map[A, List[B]]() withDefaultValue Nil)) { (map, item) =>
        val (_, a, b) = item
        map.updated(a, map(a) ++ b)
      } toList
    }

    def group[A, B](xs: List[(Double, A, List[B])]): List[(Double, List[(A, List[B])])] = {
      if (xs isEmpty) Nil
      else {
        val z = xs.head._1
        val (firstGroup, rest) = xs span { _._1 == z }
        (z, foldUp(firstGroup)) :: group(rest)
      }
    }

    val x: List[(Double, R, List[Symbolizer])] =
      zGroups.flatten[(Double, R, List[Symbolizer])].sortWith(ordering)

    group(x)

    //group(zGroups.flatten[(Double, R, List[Symbolizer])].sort(ordering))
  }


  private def groupByZ(syms: List[(Double, Symbolizer)]): List[(Double, List[Symbolizer])] = {
    def ordering(a: (Double, _), b: (Double, _)): Boolean = a._1 < b._1
    def group[A](xs: List[(Double, A)]): List[(Double, List[A])] = {
      if (xs isEmpty) Nil
      else {
        val z = xs.head._1
        val (firstGroup, rest) = xs span { _._1 == z }
        (z, firstGroup map {_._2}) :: group(rest)
      }
    }


    // we make a special case for labels; they will be rendered last anyway, so
    // we can fold them into one layer
    val (labels, symbols) = syms partition { _.isInstanceOf[TextSymbolizer] }
    group(stableSort(symbols, ordering _).toList) ++ List((0d, labels map {_._2}))
  }

  /**
   * Transpose a nested list, so that the elements of the first list become the
   * heads of the output lists, the elements of the second list become the
   * second elements in the output, etc.  This method differs from
   * List.transpose in the Scala standard library in that it does not require
   * all elements of the input list to be the same length.  This means that the
   * operation is not reversible for all possible inputs.
   * 
   * For example:
   * <pre>
   *   scala&gt; val row1 = 'a1 :: 'b1 :: Nil
   *   scala&gt; val row2 = 'a2 :: 'b2 :: 'c2 :: Nil
   *   scala&gt; print(transpose(row1 :: row2 :: Nil))
   *   List(List('a1, 'a2), List('b1, 'b2), List('c2))
   *   scala&gt; print(transpose(transpose(row1 :: row2 :: Nil)))
   *   List(List('a1, 'b1, 'c2), List('a2, 'b2))
   * </pre>
   */
  private def transpose[A](xss: List[List[A]]): List[List[A]] = {
    val filtered = xss filter (!_.isEmpty)
    if (filtered isEmpty) Nil
    else (filtered map (_.head)) :: transpose(filtered map (_.tail))
  }

  /**
   * Given a list, generate all possible groupings of the contents of that list
   * into two sublists.  The sublists preserve the ordering of the original
   * list.
   *
   * This implementation is specialized for dealing with rules.  In particular,
   * it prunes the search space when unsatisfiable rule combinations are
   * discovered.
   */
   def combinations(xs: Seq[Rule])
   (prune: Pair[Seq[Rule], Seq[Rule]] => Boolean)
   : Seq[(Seq[Rule], Seq[Rule])] = {
     import Seq.empty

     xs match {
       case Seq() => Seq((empty, empty))
       case Seq(x, xs @ _*) => 
         combinations(xs)(prune) flatMap { 
           case (in, out) => Seq((x +: in, out), (in, x +: out))
         } filter prune
     }
  }

  def cascading2exclusive(xs: List[Rule])
  : List[(List[Rule], List[Rule])] = {
    def satisfiable(x: (Seq[Rule], Seq[Rule])): Boolean =
      compose(x._1, x._2).isSatisfiable

    def ordering(a: Rule, b: Rule): Boolean =
      Specificity(a.selectors) < Specificity(b.selectors)

    combinations(xs)(satisfiable).toList map {
      case (x, y) => (stableSort(x, ordering _).toList.reverse, y.toList)
    }
  }

  /**
   * Take a list of rules to include and a list of rules being excluded and
   * generate a rule that combines their selectors and properties.  The 'out'
   * list has its selectors negated and its properties omitted.
   */
  private def compose(in: Seq[Rule], out: Seq[Rule])
  : Rule = {
    def not(xs: Seq[Selector]): Selector =
      simplify(OrSelector(xs.toList map SelectorOps.not))

    def specificity(xs: Rule, ys: Rule) =
      Specificity(xs.selectors) > Specificity(ys.selectors)

    val selectors = in.flatMap(_.selectors) ++ out.map(x => not(x.selectors))
    val sortedRules = in.sortWith(specificity)
    val description = sortedRules.map(_.description)
      .foldLeft(Description(None, None)) (Description.combine)

    Rule(
      description,
      simplify(selectors.toList), 
      sortedRules.foldLeft(Map[Option[Context], Seq[Property]]())(_ ++ _.contexts)
    )
  }
}
