package org.geoserver.community.css

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
object Translator extends CssOps with SelectorOps {
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
    val lineSyms: List[LineSymbolizer] = 
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
        sym
      }

    val polySyms: List[PolygonSymbolizer] = 
      expand(properties, "fill") map { props =>
        val fillParams = fill(props("fill"))
        val size = props.get("fill-size") map length
        val rotation = props.get("fill-rotation") map angle
        val opacity = props.get("fill-opacity") map scale getOrElse null
        val geom =
          props.get("fill-geometry") orElse props.get("geometry") map expression getOrElse null

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
        sym
      }

    val pointSyms: List[PointSymbolizer] = 
      expand(properties, "mark") map { props => 
        val fillParams = fill(props("mark"))
        val opacity = props.get("mark-opacity").map(scale).getOrElse(null)
        val size =
          props.get("mark-size").map(length).getOrElse(filters.literal(16))
        val rotation =
          props.get("mark-rotation").map(angle).getOrElse(filters.literal(0))
        val geom = (props.get("mark-geometry") orElse props.get("geometry"))
          .map(expression).getOrElse(null)

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
        sym
      }

    val textSyms: List[TextSymbolizer] =
      expand(properties, "label") map { props => 
        val fillParams = props.get("font-fill").map(fill)
        val fontFamily = props.get("font-family")
        val fontOpacity = props.get("font-opacity").map(scale)
        val anchorPoint = props.get("label-anchor").map(anchor)
        val offset = props.get("label-offset").map(displacement)
        val rotation = props.get("label-rotation").map(angle)
        val geom = (props.get("label-geometry") orElse props.get("geometry"))
          .map(expression).getOrElse(null)

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

        sym
      }

    (polySyms :: lineSyms :: pointSyms :: textSyms :: Nil)
  }

  case class SimpleRule(
    comment: String,
    selectors: List[Selector],
    properties: List[Property]
  )

  /**
   * Convert a list of tokens (as generated by CssParser.parse) into a GeoTools
   * Style object.  This works by creating individual rules for every possible
   * combination of rules from the input stylesheet, so it takes exponential
   * time (O(2^N) for N= number of input rules).  Be warned.
   *
   * @see org.geotools.styling.SLDTransformer if you want to serialize the
   *   resultant Style object to an XML file
   */
  def css2sld(styleSheet: List[Product]): gt.Style = {
    val style = styles.createStyle

    val rules = styleSheet flatMap {
        case Rule(comment, selectors, properties) => 
          selectors map { SimpleRule(comment, _, properties) }
        case _ => Nil
    }

    val (typed, untyped) =
      rules.partition(_.selectors.exists(_.isInstanceOf[TypenameSelector]))

    val styleRules: List[(String, List[List[gt.Rule]])] = 
      ((null: String, Nil: List[SimpleRule]) :: splitByType(typed)) map { 
        case (typename, rules) => (typename, featureTypeStyle(untyped ++ rules))
    } 

    styleRules.foreach(block => block._2.foreach (rules => {
      val typename = block._1
      val fts = styles.createFeatureTypeStyle
      if (typename != null) fts.setFeatureTypeName(typename)
      rules.foreach(fts.rules.add(_))
      style.featureTypeStyles.add(fts)
    }))

    return style
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

  private def featureTypeStyle(xs: List[SimpleRule])
  : List[List[gt.Rule]] = {
    def scaleBounds(xs: List[Selector]): List[Double] = {
      val bounds = xs.flatMap { _ match {
        case PseudoSelector("scale", _, scale) => Some(scale.toDouble)
        case _ => None
      }}

      (0d :: Math.POS_INF_DOUBLE :: bounds) . sort (_<_) . removeDuplicates
    }

    def sldRule(min: Double, max: Double, selector: DataSelector, title: String, `abstract`: String)
    (sym: Symbolizer): gt.Rule = {
      val styleRule = styles.createRule
      styleRule.setName(selector.toString)
      if (!title.isEmpty)
        styleRule.setTitle(title)
      if (!`abstract`.isEmpty)
        styleRule.setAbstract(`abstract`)
      styleRule.setFilter(selector.asFilter)
      if (min != 0)
        styleRule.setMinScaleDenominator(min)
      if (max != Math.POS_INF_DOUBLE) 
        styleRule.setMaxScaleDenominator(max)
      styleRule.symbolizers.add(sym)
      styleRule
    }

    def groupedRules(min: Double, max: Double, selector: DataSelector, title: String, `abstract`: String)
    (syms: List[Symbolizer]): List[gt.Rule] = {
      syms map sldRule(min, max, selector, title, `abstract`)
    }

    def inRange(min: Double, max: Double)(r: SimpleRule) = r.selectors forall {
      case PseudoSelector("scale", ">", d) => max > d.toDouble
      case PseudoSelector("scale", "<", d) => min < d.toDouble
      case _ => true
    }

    def omitScales(min: Double, max: Double)(r: SimpleRule) = {
      val selectors = r.selectors map {
        case PseudoSelector("scale", _, _) => AcceptSelector
        case x => x
      }

      SimpleRule(r.comment, selectors, r.properties)
    }

    val scales = scaleBounds(xs.flatMap(_.selectors))
    val ranges = scales zip (scales drop 1)
    val scalesets = ranges map { 
      case (minScale, maxScale) => {
        val rules = xs.filter(inRange(minScale, maxScale))
            .map(omitScales(minScale, maxScale))

        val rulesets = permute(rules).map { 
          case (in, out) => {
            val (selectors, properties) = compose(in, out)

            val sortedRules = in.sort(
                (x, y) => Specificity(x.selectors) > Specificity(y.selectors)
              )

            def getCommentEntry(s: String) = {
              sortedRules.flatMap(
                _.comment.lines.map(_.replaceFirst("""^\s*\*\s*""", "").trim)
                 .filter(_ startsWith ("@" + s))
                 .map(_.drop(1 + s.length).trim)
                 .toList.take(1)
              ).mkString(" with ")
            }

            val `abstract` = getCommentEntry("abstract")
            val title = getCommentEntry("title")

            val selector = AndSelector(selectors flatMap {
              case d: DataSelector => Some(d)
              case _ => None
            })

            selector match {
              case Exclude(_) => Nil
              case _ => symbolize(properties).map(
                groupedRules(minScale, maxScale, selector, title, `abstract`)
              )
            }
          }
        }

        transpose(rulesets).flatMap(transpose[gt.Rule])
      }
    }

    transpose(scalesets)
      .map(transpose(_).flatten((x: List[gt.Rule]) => x))
  }

  private def splitByType(xs: List[SimpleRule])
  : List[(String, List[SimpleRule])] = {
    def typeof(x: SimpleRule): String = {
      x.selectors.find(_.isInstanceOf[TypenameSelector]) match {
        case Some(TypenameSelector(typename)) => typename
        case None => ""
      }
    }

    if (xs isEmpty) Nil
    else {
      val typename = typeof(xs.head)
      val (currentType, others) = xs.partition(typename == typeof(_))
      (typename, currentType) :: splitByType(others)
    }
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
      !(compose(x._1, x._2)._1 contains Exclude)

    if (xs.isEmpty) (Nil, Nil) :: Nil
    else
      permute(xs.tail) flatMap { case (in, out) =>
        (xs.head :: in, out) :: (in, xs.head :: out) :: Nil
      } filter satisfiable
  }

  /**
   * Take a list of rules to include and a list of rules being excluded and
   * generate a rule that combines their selectors and properties.  The 'out'
   * list has its selectors negated and its properties omitted.
   */
  private def compose(in: List[SimpleRule], out: List[SimpleRule]) = {
    def not(xs: List[Selector]): Selector = {
      OrSelector(xs flatMap {
        case d: DataSelector => Some(negate(d).asInstanceOf[DataSelector])
        case _ => None
      })
    }

    def specificity(xs: SimpleRule, ys: SimpleRule) = {
      Specificity(xs.selectors) > Specificity(ys.selectors)
    }

    val selectors =
      in.flatMap((x: SimpleRule) => x.selectors) ++
      out.map((x: SimpleRule) => not(x.selectors))

    (simplify(selectors), in.sort(specificity).flatMap(_.properties))
  }
}
