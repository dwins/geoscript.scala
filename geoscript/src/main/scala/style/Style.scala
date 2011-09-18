package org.geoscript
package style.combinators

import filter._
import scala.collection.JavaConversions._

sealed abstract trait Style extends style.Style {
  def where(filter: Filter): Style
  def aboveScale(s: Double): Style
  def belowScale(s: Double): Style
  def and(that: Style): Style = {
    val seq: Style => Seq[Style] = {
      case CompositeStyle(ss) => ss
      case s => Seq(s)
    }
    CompositeStyle(seq(this) ++ seq(that))
  }
  def underlying: org.geotools.styling.Style
}

sealed abstract trait Paint {
  def asStroke(
    width: Expression,
    opacity: Expression,
    linejoin: Expression,
    linecap: Expression,
    dasharray: Seq[Float],
    dashoffset: Expression,
    mode: Stroke.Mode
  ): org.geotools.styling.Stroke

  def asFill(
    opacity: Expression
  ): org.geotools.styling.Fill
}

object Style {
  val Factory =
    org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)
}

object Stroke {
  sealed abstract trait Mode
  object Tile extends Mode
  object Follow extends Mode
}

abstract class SimpleStyle extends Style {
  private val styles =
    org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)
  private val filters =
    org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)

  def filter: Option[Filter]

  def minScale: Option[Double]

  def maxScale: Option[Double]

  def symbolizers: Seq[org.geotools.styling.Symbolizer]

  def zIndex: Double
  
  override def where(p: Filter): Style =
    new DerivedStyle(this) {
      override def filter = 
        delegate.filter.map(filters.and(p, _): Filter).orElse(Some(p))
    }

  override def aboveScale(s: Double): Style =
    new DerivedStyle(this) {
      override def minScale =
        delegate.minScale.map(math.max(_, s)).orElse(Some(s))
    }

  override def belowScale(s: Double): Style =
    new DerivedStyle(this) {
      override def maxScale =
        delegate.maxScale.map(math.min(_, s)).orElse(Some(s))
    }

  def underlying = {
    val rule = styles.createRule()
    for (f <- filter) rule.setFilter(f.underlying)
    for (s <- minScale) rule.setMinScaleDenominator(s)
    for (s <- maxScale) rule.setMaxScaleDenominator(s)
    rule.symbolizers.addAll(symbolizers)

    val ftstyle = styles.createFeatureTypeStyle()
    ftstyle.rules.add(rule)

    val style = styles.createStyle()
    style.featureTypeStyles.add(ftstyle)
    style
  }
}

class DerivedStyle(val delegate: SimpleStyle) extends SimpleStyle {
  def filter = delegate.filter
  def minScale = delegate.minScale
  def maxScale = delegate.maxScale
  def symbolizers = delegate.symbolizers
  def zIndex = delegate.zIndex
}

case class CompositeStyle(styles: Seq[Style]) extends Style {
  def flatten: Seq[SimpleStyle] =
    styles flatMap {
      case (style: SimpleStyle) => Seq(style)
      case (comp: CompositeStyle) => comp.flatten
    }

  override def aboveScale(s: Double): Style =
    CompositeStyle(styles map (_ aboveScale s))

  override def belowScale(s: Double): Style =
    CompositeStyle(styles map (_ belowScale s))

  override def where(p: Filter): Style =
    CompositeStyle(styles map (_ where p))

  override def underlying = {
    val style = Style.Factory.createStyle()
    val ftStyle = Style.Factory.createFeatureTypeStyle()

    for ((z, styles) <- this.flatten.groupBy(_.zIndex).toSeq.sortBy(_._1)) {
      val ftStyle = Style.Factory.createFeatureTypeStyle()
      for (s <- styles; fts <- s.underlying.featureTypeStyles)
        ftStyle.rules.addAll(fts.rules)
      style.featureTypeStyles.add(ftStyle)
    }

    style.featureTypeStyles.add(ftStyle)
    style
  }
}

object Paint {
  import geocss.CssOps.colors

  implicit def stringToPaint(colorName: String): Paint =
    if (colors contains colorName)
      Color(colors(colorName))
    else if (colorName matches "#[a-fA-F0-9]{6}") // TODO: regex for hex color codes
      Color(colorName)
    else
      Color("#000000")
}

case class Color(rgb: String) extends Paint {
  private val factory =
    org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)
  private val filter =
    org.geotools.factory.CommonFactoryFinder.getFilterFactory2(null)

  def asStroke(
    width: Expression,
    opacity: Expression,
    linejoin: Expression,
    linecap: Expression,
    dasharray: Seq[Float],
    dashoffset: Expression,
    mode: Stroke.Mode
  ): org.geotools.styling.Stroke = {
    factory.createStroke(
      filter.literal(rgb),
      if (width == null) null else width,
      if (opacity == null) null else opacity,
      if (linejoin == null) null else linejoin,
      if (linecap == null) null else linecap,
      if (dasharray == null) null else dasharray.toArray,
      if (dashoffset == null) null else dashoffset,
      null,
      null
    )
  }

  def asFill(
    opacity: Expression
  ): org.geotools.styling.Fill = {
    factory.fill(
      null,
      filter.literal(rgb),
      Option(opacity)
        .map(_.underlying)
        .getOrElse(filter.literal(1))
    )
  }
}

case class Fill(
  fill: Paint = null, 
  opacity: Expression = null,
  zIndex: Double = 0
) extends SimpleStyle {
  private val factory =
    org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)

  override val maxScale = None
  override val minScale = None
  override val filter = None
  override val symbolizers =
    Seq(factory.createPolygonSymbolizer(null, fill.asFill(opacity), null))
}

case class Stroke(
  stroke: Paint = null,
  width: Expression = null,
  opacity: Expression = null,
  linecap: Expression = null,
  linejoin: Expression = null,
  dashoffset: Expression = null,
  dasharray: Seq[Float] = null,
  zIndex: Double = 0,
  mode: Stroke.Mode = Stroke.Follow
) extends SimpleStyle {
  private val factory =
    org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)

  override val maxScale = None
  override val minScale = None
  override val filter = None
  override val symbolizers =
    Seq(factory.createLineSymbolizer(
      stroke.asStroke(
        width, opacity, linejoin, linecap, dasharray, dashoffset, mode
      ),
      null
    ))
}

case class Font(
  name: String,
  size: Int = 10,
  style: String = "normal",
  weight: String = "bold"
)

case class Label(
  text: Expression,
  geometry: Expression = null,
  font: Font = Font("Arial"),
  fontFill: Fill = Fill("#000000"),
  halo: Fill = null,
  rotation: Double = 0,
  anchor: (Double, Double) = (0, 0.5),
  displacement: (Double, Double) = (0, 0),
  followLine: Boolean = false,
  maxAngleDelta: Double = 22.5,
  maxDisplacement: Double = 50,
  autoWrap: Int = 0,
  repeat: Double = 0
) extends SimpleStyle {
  private val factory =
    org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)

  val maxScale = None
  val minScale = None
  val filter = None
  val symbolizers = {
    val sym = factory.createTextSymbolizer()
    sym.setLabel(text)
    sym.setGeometry(geometry)
    Seq(sym)
  }
  def zIndex = 0
}

case class Symbol(
  shape: Expression,
  fill: Fill = null,
  stroke: Stroke = null,
  size: Expression = 16,
  rotation: Expression = 0,
  opacity: Expression = 1,
  zIndex: Double = 0
) extends SimpleStyle with Paint {
  val filter = None
  val maxScale = None
  val minScale = None
  val symbolizers = 
    Seq(
      new org.geotools.styling.StyleBuilder(Style.Factory)
        .createPointSymbolizer(graphic)
    )


  def asStroke(
    width: Expression,
    opacity: Expression,
    linejoin: Expression,
    linecap: Expression,
    dasharray: Seq[Float],
    dashoffset: Expression,
    mode: Stroke.Mode
  ): org.geotools.styling.Stroke = {
    Style.Factory.createStroke(
      null,
      width,
      opacity,
      linejoin,
      linecap,
      dasharray.toArray,
      dashoffset,
      if (mode == Stroke.Tile) graphic else null,
      if (mode == Stroke.Follow) graphic else null
    )
  }

  def asFill(
    opacity: Expression
  ): org.geotools.styling.Fill = {
    Style.Factory.fill(
      graphic,
      null,
      opacity
    )
  }

  def graphic = {
    val stroke =
      if (this.stroke != null)
        this.stroke.stroke.asStroke(
          this.stroke.width,
          this.stroke.opacity,
          this.stroke.linejoin,
          this.stroke.linecap,
          this.stroke.dasharray,
          this.stroke.dashoffset,
          this.stroke.mode
        )
      else
        null

    val fill = 
      if (this.fill != null)
        this.fill.fill.asFill(this.fill.opacity)
      else
        null
    Style.Factory.createGraphic(
      null,
      Array(
        Style.Factory.createMark(
          shape,
          stroke,
          fill,
          size,
          rotation
        )
      ),
      null,
      opacity,
      size,
      rotation
    )
  }
}

case class Graphic(
  url: String,
  opacity: Expression = 1,
  size: Expression = 16,
  rotation: Expression = 0,
  zIndex: Double = 0
) extends SimpleStyle with Paint {
  private val factory =
    org.geotools.factory.CommonFactoryFinder.getStyleFactory(null)

  val filter = None
  val maxScale = None
  val minScale = None
  val symbolizers = 
    Seq(
      new org.geotools.styling.StyleBuilder(factory)
        .createPointSymbolizer(graphic)
    )

  def asStroke(
    width: Expression,
    opacity: Expression,
    linejoin: Expression,
    linecap: Expression,
    dasharray: Seq[Float],
    dashoffset: Expression,
    mode: Stroke.Mode
  ): org.geotools.styling.Stroke = {
    factory.createStroke(
      null,
      width,
      opacity,
      linejoin,
      linecap,
      dasharray.toArray,
      dashoffset,
      if (mode == Stroke.Tile) graphic else null,
      if (mode == Stroke.Follow) graphic else null
    )
  }

  def asFill(
    opacity: Expression
  ): org.geotools.styling.Fill = {
    factory.fill(
      graphic,
      null,
      opacity
    )
  }

  def graphic = 
    factory.createGraphic(
      Array(factory.createExternalGraphic(url, "image/png")),
      null,
      null,
      opacity,
      size,
      rotation
    )
}
