package org.geoserver.community.css

import org.opengis.filter.{
  BinaryComparisonOperator,
  BinaryLogicOperator,
  Not,
  PropertyIsBetween,
  PropertyIsLike,
  PropertyIsNull
}

import org.opengis.filter.expression.{
  BinaryExpression,
  PropertyName
}

/**
 * The CssOps trait provides some functionality for dealing with tasks related
 * to CSS interpretation and manipulation. For example, facilities are provided
 * for interpreting CSS property values and for dealing with CSS inheritance.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
trait CssOps {
  val colors = Map(
    "aliceblue" -> "#f0f8ff",
    "antiquewhite" -> "#faebd7",
    "aqua" -> "#00ffff",
    "aquamarine" -> "#7fffd4",
    "azure" -> "#f0ffff",
    "beige" -> "#f5f5dc",
    "bisque" -> "#ffe4c4",
    "black" -> "#000000",
    "blanchedalmond" -> "#ffebcd",
    "blue" -> "#0000ff",
    "blueviolet" -> "#8a2be2",
    "brown" -> "#a52a2a",
    "burlywood" -> "#deb887",
    "cadetblue" -> "#5f9ea0",
    "chartreuse" -> "#7fff00",
    "chocolate" -> "#d2691e",
    "coral" -> "#ff7f50",
    "cornflowerblue" -> "#6495ed",
    "cornsilk" -> "#fff8dc",
    "crimson" -> "#dc143c",
    "cyan" -> "#00ffff",
    "darkblue" -> "#00008b",
    "darkcyan" -> "#008b8b",
    "darkgoldenrod" -> "#b8860b",
    "darkgray" -> "#a9a9a9",
    "darkgreen" -> "#006400",
    "darkgrey" -> "#a9a9a9",
    "darkkhaki" -> "#bdb76b",
    "darkmagenta" -> "#8b008b",
    "darkolivegreen" -> "#556b2f",
    "darkorange" -> "#ff8c00",
    "darkorchid" -> "#9932cc",
    "darkred" -> "#8b0000",
    "darksalmon" -> "#e9967a",
    "darkseagreen" -> "#8fbc8f",
    "darkslateblue" -> "#483d8b",
    "darkslategray" -> "#2f4f4f",
    "darkslategrey" -> "#2f4f4f",
    "darkturquoise" -> "#00ced1",
    "darkviolet" -> "#9400d3",
    "deeppink" -> "#ff1493",
    "deepskyblue" -> "#00bfff",
    "dimgray" -> "#696969",
    "dimgrey" -> "#696969",
    "dodgerblue" -> "#1e90ff",
    "firebrick" -> "#b22222",
    "floralwhite" -> "#fffaf0",
    "forestgreen" -> "#228b22",
    "fuchsia" -> "#ff00ff",
    "gainsboro" -> "#dcdcdc",
    "ghostwhite" -> "#f8f8ff",
    "gold" -> "#ffd700",
    "goldenrod" -> "#daa520",
    "gray" -> "#808080",
    "grey" -> "#808080",
    "green" -> "#008000",
    "greenyellow" -> "#adff2f",
    "honeydew" -> "#f0fff0",
    "hotpink" -> "#ff69b4",
    "indianred" -> "#cd5c5c",
    "indigo" -> "#4b0082",
    "ivory" -> "#fffff0",
    "khaki" -> "#f0e68c",
    "lavender" -> "#e6e6fa",
    "lavenderblush" -> "#fff0f5",
    "lawngreen" -> "#7cfc00",
    "lemonchiffon" -> "#fffacd",
    "lightblue" -> "#add8e6",
    "lightcoral" -> "#f08080",
    "lightcyan" -> "#e0ffff",
    "lightgoldenrodyellow" -> "#fafad2",
    "lightgray" -> "#d3d3d3",
    "lightgreen" -> "#90ee90",
    "lightgrey" -> "#d3d3d3",
    "lightpink" -> "#ffb6c1",
    "lightsalmon" -> "#ffa07a",
    "lightseagreen" -> "#20b2aa",
    "lightskyblue" -> "#87cefa",
    "lightslategray" -> "#778899",
    "lightslategrey" -> "#778899",
    "lightsteelblue" -> "#b0c4de",
    "lightyellow" -> "#ffffe0",
    "lime" -> "#00ff00",
    "limegreen" -> "#32cd32",
    "linen" -> "#faf0e6",
    "magenta" -> "#ff00ff",
    "maroon" -> "#800000",
    "mediumaquamarine" -> "#66cdaa",
    "mediumblue" -> "#0000cd",
    "mediumorchid" -> "#ba55d3",
    "mediumpurple" -> "#9370db",
    "mediumseagreen" -> "#3cb371",
    "mediumslateblue" -> "#7b68ee",
    "mediumspringgreen" -> "#00fa9a",
    "mediumturquoise" -> "#48d1cc",
    "mediumvioletred" -> "#c71585",
    "midnightblue" -> "#191970",
    "mintcream" -> "#f5fffa",
    "mistyrose" -> "#ffe4e1",
    "moccasin" -> "#ffe4b5",
    "navajowhite" -> "#ffdead",
    "navy" -> "#000080",
    "oldlace" -> "#fdf5e6",
    "olive" -> "#808000",
    "olivedrab" -> "#6b8e23",
    "orange" -> "#ffa500",
    "orangered" -> "#ff4500",
    "orchid" -> "#da70d6",
    "palegoldenrod" -> "#eee8aa",
    "palegreen" -> "#98fb98",
    "paleturquoise" -> "#afeeee",
    "palevioletred" -> "#db7093",
    "papayawhip" -> "#ffefd5",
    "peachpuff" -> "#ffdab9",
    "peru" -> "#cd853f",
    "pink" -> "#ffc0cb",
    "plum" -> "#dda0dd",
    "powderblue" -> "#b0e0e6",
    "purple" -> "#800080",
    "red" -> "#ff0000",
    "rosybrown" -> "#bc8f8f",
    "royalblue" -> "#4169e1",
    "saddlebrown" -> "#8b4513",
    "salmon" -> "#fa8072",
    "sandybrown" -> "#f4a460",
    "seagreen" -> "#2e8b57",
    "seashell" -> "#fff5ee",
    "sienna" -> "#a0522d",
    "silver" -> "#c0c0c0",
    "skyblue" -> "#87ceeb",
    "slateblue" -> "#6a5acd",
    "slategray" -> "#708090",
    "slategrey" -> "#708090",
    "snow" -> "#fffafa",
    "springgreen" -> "#00ff7f",
    "steelblue" -> "#4682b4",
    "tan" -> "#d2b48c",
    "teal" -> "#008080",
    "thistle" -> "#d8bfd8",
    "tomato" -> "#ff6347",
    "turquoise" -> "#40e0d0",
    "violet" -> "#ee82ee",
    "wheat" -> "#f5deb3",
    "white" -> "#ffffff",
    "whitesmoke" -> "#f5f5f5",
    "yellow" -> "#ffff00",
    "yellowgreen" -> "#9acd32"
  )

  /**
   * The Specificity class represents a CSS specificity ranking.  The 
   * specification states that specificity is determined by four scores (and 
   * ties are broken by ordering in the style document.)  These scores are: 
   * a: 1 if the style is inlined in a document attribute, 0 otherwise
   * b: the number of ID attributes in the selector
   * c: the number of other attributes and pseudo-classes in the selector
   * d: the number of element names and pseudo-elements in the selector
   *
   * In gt-css, there is no way to inline a style in a dataset, so "a" above is
   * omitted and everything gets bumped up a step, so to speak.
   * 
   * @see http://www.w3.org/TR/CSS21/cascade.html#specificity
   */
  case class Specificity(a: Int, b: Int, c: Int) extends Ordered[Specificity] {
    def compare(that: Specificity) = 
      if (this.a != that.a) this.a compare that.a
      else if (this.b != that.b) this.b compare that.b
      else this.c compare that.c

    def +(that: Specificity) = {
      Specificity(this.a + that.a, this.b + that.b, this.c + that.c)
    }
  }

  object Specificity {
    private def extract(f: org.opengis.filter.Filter): List[String] = {
      f match {
        case b: BinaryComparisonOperator => 
          extract(b.getExpression1) ++ extract(b.getExpression2)
        case b: BinaryLogicOperator => {
          val it = b.getChildren.iterator
          var attributes = Nil: List[String]
          while (it.hasNext) {
            attributes = attributes ++ extract(it.next)
          }
          attributes
        }
        case not: Not => extract(not.getFilter)
        case b: PropertyIsBetween => 
          extract(b.getExpression) ++
          extract(b.getLowerBoundary) ++
          extract(b.getUpperBoundary)
        case l: PropertyIsLike => extract(l.getExpression)
        case n: PropertyIsNull => extract(n.getExpression)
        case _ => Nil
      }
    }

    private def extract(expr: org.opengis.filter.expression.Expression)
    : List[String] = {
      expr match {
        case b: BinaryExpression =>
          extract(b.getExpression1) ++ extract(b.getExpression2)
        case f: org.opengis.filter.expression.Function => {
          val it = f.getParameters.iterator
          var attributes = Nil: List[String]
          while (it.hasNext) {
            attributes = attributes ++ extract(it.next)
          }
          attributes
        } 
        case p: PropertyName => p.getPropertyName :: Nil
        case _ => Nil
      }
    }

    private def countAttributes(f: org.opengis.filter.Filter) = {
      extract(f).removeDuplicates.length
    }

    def apply(x: Selector): Specificity = x match {
      case _: TypenameSelector => Specificity(0, 0, 1)
      case _: PseudoSelector => Specificity(0, 1, 0)
      case _: IdSelector => Specificity(1, 0, 0)
      case expr: ExpressionSelector => 
        Specificity(0, countAttributes(expr.asFilter), 0)
      case _ => Specificity(0, 0, 0)
    }

    def apply(xs: List[Selector]): Specificity = {
      xs.map(apply).foldLeft(Specificity(0, 0, 0))(
        (x: Specificity, y: Specificity) => x + y
      )
    }
  }

  object URL {
    def unapply(value: Value): Option[String] = value match {
      case Function("url", List(Literal(url))) => Some(url)
      case _ => None
    }
  }

  object Color {
    val ShortHex = """#?([a-fA-F0-9]{3})""".r
    val LongHex = """#?([a-fA-F0-9]{6})""".r

    def unapply(value: Value): Option[String] = value match {
      case Function("rgb", List(Literal(r), Literal(g), Literal(b))) => 
        val channels = List(r, g, b)
        val hex = "#%2x%2x%2x"
        def validInt(x: String) =
          try { x.toInt; true } catch { case _ => false }

        def validDouble(x: String) = 
          try { x.toDouble; true } catch { case _ => false }

        def dbl(x: String) = Math.round(x.toFloat * 255f)

        if (channels.forall(validInt)) {
          Some(hex.format(r.toInt, g.toInt, b.toInt))
        } else if (channels.forall(validDouble)) {
          Some(hex.format(dbl(r), dbl(g), dbl(b)))
        } else {
          None
        }
      case Literal(LongHex(hex)) => 
        Some("#" + hex)
      case Literal(ShortHex(hex)) => 
        Some(hex.map(x => "" + x + x).mkString("#", "", ""))
      case Literal(name) =>
        colors.get(name)
      case _ => 
        None
    }
  }

  object Symbol {
    def unapply(value: Value): Option[String] = value match {
      case Function("symbol", Literal(symbol) :: Nil) => Some(symbol)
      case _ => None
    }
  }
}
