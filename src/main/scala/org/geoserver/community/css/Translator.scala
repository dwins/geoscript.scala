package org.geoserver.community.css

import org.geoserver.community.css.filter.FilterOps

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

  def buildMark(markName: String, width: OGCExpression, rotation: OGCExpression) = {
    if (markName != null) {
      Array(styles.createMark(
        filters.literal(markName),
        styles.getDefaultStroke,
        styles.getDefaultFill,
        width,
        rotation)
    ) } else null
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
  def color(p: Property) = p.values.first.first match {
    case Literal(body) => filters.literal(colors.getOrElse(body, body))
    case _ => null
  }

  def angle(xs: List[Value]) = xs.first match {
    case l: Literal =>
      filters.literal(l.body.replaceFirst("deg$", ""))
    case Expression(cql) =>
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def length(xs: List[Value]): OGCExpression = xs.first match {
    case Literal(body) =>
      filters.literal(body.replaceFirst("px$", ""))
    case Expression(cql) =>
      org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def expression(xs: List[Value]): OGCExpression = xs.first match {
    case Literal(body) => filters.literal(body)
    case Expression(cql) => org.geotools.filter.text.ecql.ECQL.toExpression(cql)
    case _ => null
  }

  def keyword(default: String, xs: List[Value]): String = xs.first match {
    case Literal(body) => body
    case _ => default
  }

  def keyword(xs: List[Value]): String = keyword(null:String, xs)

  def scale(s: String): Float = {
    if (s.endsWith("%")) s.replaceFirst("%$","").toFloat / 100.0f
    else s.toFloat
  }

  def scale(xs: List[Value]): OGCExpression = xs.first match {
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

  /**
   * Convert a set of properties to a set of Symbolizer objects attached to the
   * given Rule.
   */
  def symbolize(properties: List[Property]) = {
    val lineSyms: List[(Double, LineSymbolizer)] = 
      expand(properties, "stroke") map { props => 
        val strokeParams = fill(props("stroke"))
        val stroke = strokeParams._3
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

        val graphic = {
          val mark = buildMark(
            strokeParams._2,
            width.getOrElse(filters.literal(16)),
            filters.literal(0)
          )

          val externalGraphic =
            buildExternalGraphic(
            strokeParams._1,
            props.get("stroke-mime").map(keyword)
          )

          if (mark != null || externalGraphic != null) {
            styles.createGraphic(externalGraphic, mark, null, null, null, rotation)
          } else null
        }

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
      }

    val polySyms: List[(Double, PolygonSymbolizer)] = 
      expand(properties, "fill") map { props =>
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

        val graphic = {
          val mark = buildMark(
            fillParams._2,
            size.getOrElse(filters.literal(16)),
            rotation.getOrElse(filters.literal(0))
          )

          val externalGraphic =
            buildExternalGraphic(fillParams._1, props.get("fill-mime").map(keyword))

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

        val sym = styles.createPolygonSymbolizer(
            null,
            styles.createFill(fillParams._3, null, opacity, graphic),
            null
          )
        sym.setGeometry(geom)
        (zIndex, sym)
      }

    val pointSyms: List[(Double, PointSymbolizer)] = 
      expand(properties, "mark") map { props => 
        val fillParams = fill(props("mark"))
        val opacity = props.get("mark-opacity").map(scale).getOrElse(null)
        val size =
          props.get("mark-size").map(length).getOrElse(filters.literal(16))
        val rotation =
          props.get("mark-rotation").map(angle).getOrElse(filters.literal(0))
        val geom = (props.get("mark-geometry") orElse props.get("geometry"))
          .map(expression).getOrElse(null)
        val zIndex: Double = 
          props.get("mark-z-index") orElse props.get("z-index") map {
            x => keyword(x).toDouble
          } getOrElse(0d)

        val graphic = {
          val mimetype = props.get("mark-mime").map(keyword)
          val mark = buildMark(fillParams._2, size, rotation)
          val externalGraphic = buildExternalGraphic(fillParams._1, mimetype)

          styles.createGraphic(
            externalGraphic,
            mark,
            null,
            opacity,
            size,
            rotation
          )
        }

        val sym = styles.createPointSymbolizer(graphic, null)
        sym.setGeometry(geom)
        (zIndex, sym)
      }

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
            filters.literal(16),
            filters.literal(0)
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
            .map(x => color(x.first)).getOrElse(null)
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

  case class SimpleRule(
    description: Description,
    selectors: List[Selector],
    properties: List[Property]
  ) {
    lazy val isSatisfiable = 
      this.selectors exists { 
        case Exclude(_) => false
        case _ => true
      }

    def getFilter = 
      AndSelector(selectors filter { _.filterOpt.isDefined }).filterOpt.get
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

    val rules = 
      styleSheet flatMap {
        case Rule(desc, selectors, properties) =>
          selectors map { SimpleRule(desc, _, properties) }
      }

    def extractTypeName(rule: SimpleRule): Option[String] =
      rule.selectors find {
        _.isInstanceOf[TypenameSelector] 
      } map { _.asInstanceOf[TypenameSelector].typename }

    def extractScaleRange(rule: SimpleRule): (Option[Double], Option[Double]) = {
      val scales = rule.selectors flatMap {
          case sel @ PseudoSelector("scale", _, _) => Some(sel)
          case _ => None
        }

      val min = 
        scales.filter(_.operator == ">") map { _.value.toDouble } match {
          case Nil => None
          case mins => Some(mins.reduceLeft(Math.max))
        }

      val max = 
        scales.filter(_.operator == "<") map { _.value.toDouble } match {
          case Nil => None
          case maxes => Some(maxes.reduceLeft(Math.min))
        }

      (min, max)
    }

    def isForTypename(typename: Option[String])(rule: SimpleRule): Boolean =
      rule.selectors.forall {
        case TypenameSelector(existing) => typename == Some(existing)
        case _ => true
      }

    def stripTypenames(rule: SimpleRule): SimpleRule = 
      SimpleRule(
        rule.description,
        rule.selectors map { 
          case TypenameSelector(_) => AcceptSelector
          case selector => selector
        },
        rule.properties
      )

    val typenames = (rules map extractTypeName) removeDuplicates

    val styleRules = 
      for (name <- typenames) yield (name, rules filter isForTypename(name) map stripTypenames)

    for ((typename, overlays) <- styleRules) {
      val zGroups: List[List[(Double, SimpleRule, List[gt.Symbolizer])]] = 
        for (rule <- cascading2exclusive(overlays))
          yield groupByZ(symbolize(rule.properties)) map {
            case (z, syms) => (z, rule, syms)
          }

      for ((_, group) <- flattenByZ(zGroups)) {
        val fts = styles.createFeatureTypeStyle
        typename.foreach(fts.setFeatureTypeName(_))
        for ((rule, syms) <- group if !syms.isEmpty) {
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

  private def flattenByZ(zGroups: List[List[(Double, SimpleRule, List[Symbolizer])]])
  : List[(Double, List[(SimpleRule, List[Symbolizer])])] 
  = {
    def ordering(a: (Double, _, _), b: (Double, _, _)): Boolean = a._1 < b._1

    def foldUp[A, B](xs: List[(_, A, List[B])]): List[(A, List[B])] = {
      (xs foldLeft (Map[A, List[B]]() withDefaultValue Nil)) { (map, item) =>
        val (_, a, b) = item
        map(a) = map(a) ++ b
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

    val x: List[(Double, SimpleRule, List[Symbolizer])] =
      zGroups.flatten[(Double, SimpleRule, List[Symbolizer])].sort(ordering)

    group(x)

    //group(zGroups.flatten[(Double, SimpleRule, List[Symbolizer])].sort(ordering))
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

    import scala.util.Sorting.stableSort

    // we make a special case for labels; they will be rendered last anyway, so
    // we can fold them into one layer
    val (labels, symbols) = syms partition { _.isInstanceOf[TextSymbolizer] }
    group(stableSort(symbols, ordering _).toList) ++ List((0, labels map {_._2}))
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
  def permute(xs: List[SimpleRule])
  : List[(List[SimpleRule], List[SimpleRule])] = {
    // does this pair of "in" and "out" rules combine to form a set of selectors
    // that can possibly match anything?
    def satisfiable(x: (List[SimpleRule], List[SimpleRule])): Boolean =
      compose(x._1, x._2).isSatisfiable

    if (xs.isEmpty) 
      List((Nil, Nil))
    else
      permute(xs.tail) flatMap { case (in, out) =>
        (xs.head :: in, out) :: (in, xs.head :: out) :: Nil
      } filter satisfiable
  }

  def cascading2exclusive(xs: List[SimpleRule]): List[SimpleRule] =
    permute(xs) map scala.Function.tupled(compose) 

  /**
   * Take a list of rules to include and a list of rules being excluded and
   * generate a rule that combines their selectors and properties.  The 'out'
   * list has its selectors negated and its properties omitted.
   */
  private def compose(in: List[SimpleRule], out: List[SimpleRule]) = {
    def not(xs: List[Selector]): Selector =
      simplify(OrSelector(xs map SelectorOps.not))

    def specificity(xs: SimpleRule, ys: SimpleRule) = {
      Specificity(xs.selectors) > Specificity(ys.selectors)
    }

    val selectors =
      in.flatMap((x: SimpleRule) => x.selectors) ++
      out.map((x: SimpleRule) => not(x.selectors))

    val sortedRules = in.sort(specificity)

    val description = sortedRules.map(_.description)
      .foldLeft(Description(None, None)) (Description.combine)

    SimpleRule( 
      description,
      simplify(selectors), 
      sortedRules.flatMap(_.properties)
    )
  }
}
