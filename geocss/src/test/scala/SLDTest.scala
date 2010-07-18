package org.geoscript.geocss

import org.specs._

/**
 * Acceptance tests against the DOM of a generated SLD
 */
class SLDTest extends Specification {
  def css2sld2dom(filename: String) = {
    val stream = getClass.getResourceAsStream(filename)
    val styleSheet = CssParser.parse(stream).get
    val style = Translator.css2sld(styleSheet)
    val bos = new java.io.ByteArrayOutputStream
    val xform = new org.geotools.styling.SLDTransformer
    xform.setIndentation(2)
    xform.transform(style, bos)
    scala.xml.XML.loadString(bos.toString())
  }


  "States style" in {
    val states = css2sld2dom("/states.css")

    val css = states \\ "Rule" \ "LineSymbolizer" \ "Stroke" \ "CssParameter"
    val titles = (states \\ "Rule" \ "Title" map (_.text)).toSet
    titles must containAll(
      Seq("Persons < 2M", "2M < Persons < 4M", "4M < Persons")
    )
    val width =
      css.filter(_.attribute("name").get.text == ("stroke-width")).head
    (width \\ "Div") must notBeEmpty
  }

  "The minimal properties required to activate each type of symbolizer" in {
    val minimal = css2sld2dom("/minimal.css")

    (minimal \\ "Rule") must haveSize(1)
    (minimal \\ "Rule" \ "PolygonSymbolizer") must haveSize(1)
    (minimal \\ "Rule" \ "PolygonSymbolizer" \ "Fill" \
      "CssParameter").head.attribute("name").get.text must_== ("fill")
    (minimal \\ "Rule" \ "PolygonSymbolizer" \ "Fill" \
      "CssParameter").head.text must_== ("#ff0000")

    (minimal \\ "Rule" \ "LineSymbolizer") must haveSize(1)
    (minimal \\ "Rule" \ "LineSymbolizer" \ "Stroke" \
      "CssParameter").head.attribute("name").get.text must_== ("stroke")
    (minimal \\ "Rule" \ "LineSymbolizer" \ "Stroke" \
      "CssParameter").head.text must_== "#ff0000"

    (minimal \\ "Rule" \ "PointSymbolizer") must haveSize(1)
    (minimal \\ "Rule" \ "PointSymbolizer" \ "Graphic" \ "Mark") must 
      haveSize(1)
    (minimal \\ "Rule" \ "PointSymbolizer" \ "Graphic" \ "Mark" \
      "WellKnownName").head.text must_== "star"

    (minimal \\ "Rule" \ "TextSymbolizer") must haveSize(1)
    (minimal \\ "Rule" \ "TextSymbolizer" \ "Label"
      \ "Literal").head.text must_== ("Label")
    (minimal \\ "Rule" \ "TextSymbolizer" \ "Halo" \ "Radius")
      .head.text.trim must_== "2"
  }

  "All properties that can be used outside of marks" in {
    val comprehensive = css2sld2dom("/comprehensive.css")

    (comprehensive \\ "Rule") must haveSize(1)

    val polysyms = (comprehensive \\ "Rule" \ "PolygonSymbolizer")
    polysyms must haveSize(1)
    val polyparams = (polysyms \ "Fill" \ "CssParameter")
    polyparams(0).attribute("name").get.text must_== ("fill")
    polyparams(1).attribute("name").get.text must_== ("fill-opacity")
    (polyparams)(0).text must_== ("#FFFFFF")
    (polyparams)(1).text.toDouble must be closeTo(0.7 +/- 0.0001)
    val polyGraphic = polysyms \ "Fill" \ "GraphicFill" \ "Graphic"
    (polyGraphic \ "ExternalGraphic" \ "OnlineResource")
      .head.attribute("http://www.w3.org/1999/xlink", "href")
      .get.text must_== ("http://example.com/example.png")
    (polyGraphic \ "ExternalGraphic" \ "Format").text must_== ("image/png")
    (polyGraphic \ "Size").text.trim must_== ("32")
    (polyGraphic \ "Rotation").text.trim must_== ("12")

    val linesyms = (comprehensive \\ "Rule" \ "LineSymbolizer")
    linesyms must haveSize(1)
    val lineparams = (linesyms \ "Stroke" \ "CssParameter")
    val stroke = lineparams find (_.attribute("name").exists(_.text=="stroke"))
    val strokeLinecap = lineparams find (_.attribute("name").exists(_.text=="stroke-linecap"))
    val strokeLinejoin = lineparams find (_.attribute("name").exists(_.text=="stroke-linejoin"))
    val strokeOpacity = lineparams find (_.attribute("name").exists(_.text=="stroke-opacity"))
    val strokeWidth = lineparams find (_.attribute("name").exists(_.text=="stroke-width"))
    val strokeDashOffset = lineparams find (_.attribute("name").exists(_.text=="stroke-dashoffset"))
    val strokeDashArray = lineparams find (_.attribute("name").exists(_.text=="stroke-dasharray"))

    stroke.get.text.trim must_== ("#FFFFFF")
    strokeLinecap.get.text.trim must_== ("square")
    strokeLinejoin.get.text.trim must_== ("mitre")
    strokeOpacity.get.text.trim.toDouble must be closeTo(0.7 +/- 0.0001)
    strokeWidth.get.text.trim must_== ("2")
    strokeDashOffset.get.text.trim must_== ("2")
    strokeDashArray.get.text.trim must_== ("1.0 2.0 1.0 4.0")

    val lineGraphic =
      linesyms \ "Stroke" \ "GraphicStroke" \ "Graphic"
    (lineGraphic \ "ExternalGraphic" \ "OnlineResource").head
      .attribute("http://www.w3.org/1999/xlink", "href")
      .get.text must_== ("http://example.com/example.gif")
    (lineGraphic \ "ExternalGraphic" \ "Format").text.trim must_== ("image/gif")
    (lineGraphic \ "Rotation").text.trim must_== ("12")

    val pointsyms = comprehensive \\ "Rule" \ "PointSymbolizer"
    pointsyms must haveSize(1)
    val pointgraphic = pointsyms \\ "Graphic"
    (pointgraphic \ "Mark" \ "WellKnownName").text.trim must_== ("circle")
    (pointgraphic \ "Opacity")
      .text.trim.toDouble must be closeTo(0.7 +/- 0.0001)
    (pointgraphic \ "Size").text.trim must_== ("16")
    (pointgraphic \ "Rotation").text.trim must_== ("12")

    val textsyms = comprehensive \\ "Rule" \ "TextSymbolizer"
    textsyms.length must_== (1)
    (textsyms \ "Label" \ "PropertyName").text must_== ("PROPNAME")
    textsyms \ "LabelPlacement" must haveSize(1)
    textsyms \ "LabelPlacement" \ "PointPlacement" \ "AnchorPoint" must haveSize(1)
    (textsyms \ "LabelPlacement" \\ "AnchorPointX").text.trim must_== ("0.5")
    (textsyms \ "LabelPlacement" \\ "AnchorPointY").text.trim must_== ("0.0")
    textsyms \ "LabelPlacement" \ "PointPlacement" \ "Displacement" must haveSize(1)
    (textsyms \ "LabelPlacement" \\ "DisplacementX").text.trim must_== ("1")
    (textsyms \ "LabelPlacement" \\ "DisplacementY").text.trim must_== ("2")
    val fontparams = textsyms \ "Font" \ "CssParameter"
    fontparams(0).attribute("name").get.text must_== ("font-family")
    fontparams(1).attribute("name").get.text must_== ("font-size")
    fontparams(2).attribute("name").get.text must_== ("font-style")
    fontparams(3).attribute("name").get.text must_== ("font-weight")
    fontparams(0).text.trim must_== ("Times New Roman")
    fontparams(1).text.trim must_== ("17")
    fontparams(2).text.trim must_== ("oblique")
    fontparams(3).text.trim must_== ("bold")
    val fillparams = textsyms \ "Fill" \ "CssParameter"
    fillparams(0).attribute("name").get.text must_== ("fill")
    fillparams(1).attribute("name").get.text must_== ("fill-opacity")
    val halo = textsyms \ "Halo"
    (halo \ "Radius").text.trim must_== ("2")
    val haloparams = halo \ "Fill" \ "CssParameter"
    haloparams(0).attribute("name").get.text must_== ("fill")
    haloparams(1).attribute("name").get.text must_== ("fill-opacity")
    haloparams(0).text.trim must_== ("#FFFFFF")
    haloparams(1).text.trim.toDouble must be closeTo(0.7 +/- 0.001)
  }

  "GeoTools vendor options should be passed through" in {
    val vendorOptions = css2sld2dom("/gt-opts.css")

    def vendor(name: String): Option[String] = {
      (vendorOptions \\ "VendorOption") find {
        _.attribute("name") map (_.text == name) getOrElse(false)
      } map { 
        _.child.text 
      }
    }

    // all vendor options should be direct children of textsymbolizers now
    vendorOptions \\ "VendorOption" must_== (
      vendorOptions \\ "TextSymbolizer" \ "VendorOption"
    )

    vendor("allGroup") must beSome("false")
    vendor("maxAngleDelta") must beSome("22.5")
    vendor("followLine") must beSome("false")
    vendor("autoWrap") must beSome("0")
    vendor("repeat") must beSome("0")
    vendor("goodnessOfFit") must beSome("0.50")
    vendor("conflictResolution") must beSome("true")
    vendor("removeOverlaps") must beSome("false")
    vendor("allowOverrun") must beSome("true")
    vendor("minGroupDistance") must beSome("-1")
    vendor("spaceAround") must beSome("0")
    vendor("group") must beSome("false")
    vendor("maxDisplacement") must beSome("0")
    vendor("forceLeftToRight") must beSome("true")
    vendor("spaceAround") must beSome("0")

    (vendorOptions \\ "Priority" \ "PropertyName" text) must_== ("priority")
  }


  "Mixing selector properties doensn't produce empty rules" in {
    val planet = css2sld2dom("/planet_polygon.css")

    for (rule <- planet \\ "Rule") {
      val text = rule \\ "TextSymbolizer"
      val mark = rule \\ "PointSymbolizer"
      val stroke = rule \\ "LineSymbolizer"
      val fill = rule \\ "PolygonSymbolizer"
      (text ++ mark ++ stroke ++ fill) must notBeEmpty
    }
  }

  "It should be possible to 'stack' symbolizers" in {
    val stackedSymbolizers = css2sld2dom("/stacked-symbolizers.css")

    stackedSymbolizers \\ "Rule" must haveSize(1)
    val rule = (stackedSymbolizers \\ "Rule").head
    val colors = 
    (rule \ "LineSymbolizer" \ "Stroke" \ "CssParameter") filter (
      _.attribute("name").exists(_.text == "stroke")
    ) map (_.text.trim)
    colors(0) must_== ("#ff0000")
    colors(1) must_== ("#008000")
    colors(2) must_== ("#0000ff")
  }

  "More specific rules should override properties from less specific ones" in {
    val overrides = css2sld2dom("/overrides.css")

    val fills =
      overrides \\ "CssParameter" filter {n => (n \ "@name").text == "fill"}
    val strokes =
      overrides \\ "CssParameter" filter {n => (n \ "@name").text == "stroke"}
    val marks = overrides \\ "WellKnownName" 

    fills must haveSize(2)
    fills.map(_.text).distinct must haveSize(2)

    strokes must haveSize(2)
    strokes.map(_.text).distinct must haveSize(2)

    marks must haveSize(2)
    marks.map(_.text).distinct must haveSize(2)
  }

  "It should be possible to style well-known marks" in {
    val defaultPoint = css2sld2dom("/default_point.css")
    val simpleLine = css2sld2dom("/cookbook/line_simpleline.css")
    val graphicPoint = css2sld2dom("/cookbook/point_pointasgraphic.css")
    val capitals = css2sld2dom("/capitals.css")
    val camping = css2sld2dom("/camping.css")
    val hospital = css2sld2dom("/hospital.css")
    val overrides = css2sld2dom("/mark-overrides.css")
    val rotatedSquare = css2sld2dom("/cookbook/point_rotatedsquare.css")

    defaultPoint \\ "Rule" must haveSize(1)
    defaultPoint \\ "PolygonSymbolizer" must haveSize(0)
    defaultPoint \\ "Fill" \ "CssParameter" must haveSize(1)
    defaultPoint \\ "Stroke" must haveSize(0)

    simpleLine \\ "Graphic" must haveSize(0)

    graphicPoint \\ "Size" must haveSize(0)

    capitals \\ "LineSymbolizer" must haveSize(0)
    capitals \\ "Stroke" \ "CssParameter" must haveSize(1)
    capitals \\ "PolygonSymbolizer" must haveSize(0)
    capitals \\ "Fill" \ "CssParameter" must haveSize(1)

    camping \\ "PolygonSymbolizer" \\ "Mark" \ "Stroke" must haveSize(1)
    camping \\ "PolygonSymbolizer" \\ "Mark" \ "Fill" must haveSize(0)
    camping \\ "PointSymbolizer" \\ "Mark" \ "Fill" must haveSize(1)
    camping \\ "PointSymbolizer" \\ "Mark" \ "Stroke" must haveSize(0)

    hospital \\ "PolygonSymbolizer" must haveSize(0)
    hospital \\ "PointSymbolizer" \\ "Mark" must haveSize(2)
    hospital \\ "PointSymbolizer" \\ "Mark" \ "Fill" must haveSize(2)
    hospital \\ "PointSymbolizer" \\ "Mark" \ "Fill" \ "CssParameter" must haveSize(2)

    rotatedSquare \\ "Graphic" \ "Rotation" must haveSize(1)
    (rotatedSquare \\ "Graphic" \ "Rotation").text.trim must_== ("45")

    overrides \\ "WellKnownName" must haveSize(2)
    (overrides \\ "WellKnownName" map (_.text)).distinct must haveSize(2)

    val fills = overrides \\ "Mark" \ "Fill"
    fills must haveSize(2)
    (fills \ "CssParameter" 
      filter (n => (n \ "@name" text) == "fill") 
      map (_.text)
    ).distinct must haveSize(2)
  }
}
