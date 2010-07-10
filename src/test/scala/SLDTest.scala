package org.geoserver.community.css

import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.junit.Test

/**
 * Acceptance tests against the DOM of a generated SLD
 */
class SLDTest extends JUnitSuite with MustMatchersForJUnit {
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


  @Test def statesStyle {
    val states = css2sld2dom("/states.css")

    val css = states \\ "Rule" \ "LineSymbolizer" \ "Stroke" \ "CssParameter"
    val titles = Set(states \\ "Rule" \ "Title" map (_.text): _*)
    for (t <- Seq("Persons < 2M", "2M < Persons < 4M", "4M < Persons"))
      titles must (contain(t))
    val width =
      css.filter(_.attribute("name").get.text == ("stroke-width")).first
    (width \\ "Div").isEmpty must be (false)
  }

  @Test def minimalStyle {
    val minimal = css2sld2dom("/minimal.css")

    (minimal \\ "Rule").length must be (1)
    (minimal \\ "Rule" \ "PolygonSymbolizer").length must be (1)
    (minimal \\ "Rule" \ "PolygonSymbolizer" \ "Fill" \
      "CssParameter").first.attribute("name").get must be ("fill")
    (minimal \\ "Rule" \ "PolygonSymbolizer" \ "Fill" \
      "CssParameter").first.text must be ("#ff0000")

    (minimal \\ "Rule" \ "LineSymbolizer").length must be (1)
    (minimal \\ "Rule" \ "LineSymbolizer" \ "Stroke" \
      "CssParameter").first.attribute("name").get must be ("stroke")
    (minimal \\ "Rule" \ "LineSymbolizer" \ "Stroke" \
      "CssParameter").first.text must be ("#ff0000")

    (minimal \\ "Rule" \ "PointSymbolizer").length must be (1)
    (minimal \\ "Rule" \ "PointSymbolizer" \ "Graphic" \ "Mark")
      .length must be (1)
    (minimal \\ "Rule" \ "PointSymbolizer" \ "Graphic" \ "Mark" \
      "WellKnownName").first.text must be ("star")

    (minimal \\ "Rule" \ "TextSymbolizer").length must be (1)
    (minimal \\ "Rule" \ "TextSymbolizer" \ "Label"
      \ "Literal").first.text must be ("Label")
    (minimal \\ "Rule" \ "TextSymbolizer" \ "Halo" \ "Radius")
      .first.text.trim must be ("2")
  }

  @Test def comprehensiveStyle {
    val comprehensive = css2sld2dom("/comprehensive.css")

    (comprehensive \\ "Rule").length must be (1)

    val polysyms = (comprehensive \\ "Rule" \ "PolygonSymbolizer")
    polysyms.length must be (1)
    val polyparams = (polysyms \ "Fill" \ "CssParameter")
    polyparams(0).attribute("name").get must be ("fill")
    polyparams(1).attribute("name").get must be ("fill-opacity")
    (polyparams)(0).text must be ("#FFFFFF")
    (polyparams)(1).text.toDouble must be (0.7 plusOrMinus 0.0001)
    val polyGraphic = polysyms \ "Fill" \ "GraphicFill" \ "Graphic"
    (polyGraphic \ "ExternalGraphic" \ "OnlineResource")
      .first.attribute("http://www.w3.org/1999/xlink", "href")
      .get must be ("http://example.com/example.png")
    (polyGraphic \ "ExternalGraphic" \ "Format").text must be ("image/png")
    (polyGraphic \ "Size").text.trim must be ("32")
    (polyGraphic \ "Rotation").text.trim must be ("12")

    val linesyms = (comprehensive \\ "Rule" \ "LineSymbolizer")
    linesyms.length must be (1)
    val lineparams = (linesyms \ "Stroke" \ "CssParameter")
    val stroke = lineparams find (_.attribute("name").exists(_.text=="stroke"))
    val strokeLinecap = lineparams find (_.attribute("name").exists(_.text=="stroke-linecap"))
    val strokeLinejoin = lineparams find (_.attribute("name").exists(_.text=="stroke-linejoin"))
    val strokeOpacity = lineparams find (_.attribute("name").exists(_.text=="stroke-opacity"))
    val strokeWidth = lineparams find (_.attribute("name").exists(_.text=="stroke-width"))
    val strokeDashOffset = lineparams find (_.attribute("name").exists(_.text=="stroke-dashoffset"))
    val strokeDashArray = lineparams find (_.attribute("name").exists(_.text=="stroke-dasharray"))

    stroke.get.text.trim must be ("#FFFFFF")
    strokeLinecap.get.text.trim must be ("square")
    strokeLinejoin.get.text.trim must be ("mitre")
    strokeOpacity.get.text.trim.toDouble must be (0.7 plusOrMinus 0.0001)
    strokeWidth.get.text.trim must be ("2")
    strokeDashOffset.get.text.trim must be ("2")
    strokeDashArray.get.text.trim must be ("1.0 2.0 1.0 4.0")

    val lineGraphic =
      linesyms \ "Stroke" \ "GraphicStroke" \ "Graphic"
    (lineGraphic \ "ExternalGraphic" \ "OnlineResource").first
      .attribute("http://www.w3.org/1999/xlink", "href")
      .get must be ("http://example.com/example.gif")
    (lineGraphic \ "ExternalGraphic" \ "Format").text.trim must be ("image/gif")
    (lineGraphic \ "Rotation").text.trim must be ("12")

    val pointsyms = comprehensive \\ "Rule" \ "PointSymbolizer"
    pointsyms.length must be (1)
    val pointgraphic = pointsyms \\ "Graphic"
    (pointgraphic \ "Mark" \ "WellKnownName").text.trim must be ("circle")
    (pointgraphic \ "Opacity")
      .text.trim.toDouble must be (0.7 plusOrMinus 0.0001)
    (pointgraphic \ "Size").text.trim must be ("16")
    (pointgraphic \ "Rotation").text.trim must be ("12")

    val textsyms = comprehensive \\ "Rule" \ "TextSymbolizer"
    textsyms.length must be (1)
    (textsyms \ "Label" \ "PropertyName") must be ("PROPNAME")
    textsyms \ "LabelPlacement" must have (length(1))
    textsyms \ "LabelPlacement" \ "PointPlacement" \ "AnchorPoint" must have (length(1))
    (textsyms \ "LabelPlacement" \\ "AnchorPointX").text.trim must be ("0.5")
    (textsyms \ "LabelPlacement" \\ "AnchorPointY").text.trim must be ("0.0")
    textsyms \ "LabelPlacement" \ "PointPlacement" \ "Displacement" must have (length(1))
    (textsyms \ "LabelPlacement" \\ "DisplacementX").text.trim must be ("1")
    (textsyms \ "LabelPlacement" \\ "DisplacementY").text.trim must be ("2")
    val fontparams = textsyms \ "Font" \ "CssParameter"
    fontparams(0).attribute("name").get must be ("font-family")
    fontparams(1).attribute("name").get must be ("font-size")
    fontparams(2).attribute("name").get must be ("font-style")
    fontparams(3).attribute("name").get must be ("font-weight")
    fontparams(0).text.trim must be ("Times New Roman")
    fontparams(1).text.trim must be ("17")
    fontparams(2).text.trim must be ("oblique")
    fontparams(3).text.trim must be ("bold")
    val fillparams = textsyms \ "Fill" \ "CssParameter"
    fillparams(0).attribute("name").get must be ("fill")
    fillparams(1).attribute("name").get must be ("fill-opacity")
    val halo = textsyms \ "Halo"
    (halo \ "Radius").text.trim must be ("2")
    val haloparams = halo \ "Fill" \ "CssParameter"
    haloparams(0).attribute("name").get must be ("fill")
    haloparams(1).attribute("name").get must be ("fill-opacity")
    haloparams(0).text.trim must be ("#FFFFFF")
    haloparams(1).text.trim.toDouble must be (0.7 plusOrMinus 0.001)
  }

  @Test def vendorOpts {
    val vendorOptions = css2sld2dom("/gt-opts.css")

    def vendor(name: String): Option[String] = {
      (vendorOptions \\ "VendorOption") find {
        _.attribute("name") map (_.text == name) getOrElse(false)
      } map { 
        _.child.text 
      }
    }

    // all vendor options should be direct children of textsymbolizers now
    vendorOptions \\ "VendorOption" must be (
      vendorOptions \\ "TextSymbolizer" \ "VendorOption"
    )

    vendor("allGroup") must be (Some("false"))
    vendor("maxAngleDelta") must be (Some("22.5"))
    vendor("followLine") must be (Some("false"))
    vendor("autoWrap") must be (Some("0"))
    vendor("repeat") must be (Some("0"))
    vendor("goodnessOfFit") must be (Some("0.50"))
    vendor("conflictResolution") must be (Some("true"))
    vendor("removeOverlaps") must be (Some("false"))
    vendor("allowOverrun") must be (Some("true"))
    vendor("minGroupDistance") must be (Some("-1"))
    vendor("spaceAround") must be (Some("0"))
    vendor("group") must be (Some("false"))
    vendor("maxDisplacement") must be (Some("0"))
    vendor("forceLeftToRight") must be (Some("true"))
    vendor("spaceAround") must be (Some("0"))

    vendorOptions \\ "Priority" \ "PropertyName" must be ("priority")
  }


  @Test def planetPolygon {
    val planet = css2sld2dom("/planet_polygon.css")

    for (rule <- planet \\ "Rule") {
      val text = rule \\ "TextSymbolizer"
      val mark = rule \\ "PointSymbolizer"
      val stroke = rule \\ "LineSymbolizer"
      val fill = rule \\ "PolygonSymbolizer"
      (text ++ mark ++ stroke ++ fill) must not have length(0)
    }
  }

  @Test def symbolizerStacking {
    val stackedSymbolizers = css2sld2dom("/stacked-symbolizers.css")

    stackedSymbolizers \\ "Rule" must have (length(1))
    val rule = (stackedSymbolizers \\ "Rule").first
    val colors = 
    (rule \ "LineSymbolizer" \ "Stroke" \ "CssParameter") filter (
      _.attribute("name").exists(_.text == "stroke")
    ) map (_.text.trim)
    colors(0) must be ("#ff0000")
    colors(1) must be ("#008000")
    colors(2) must be ("#0000ff")
  }

  @Test def overrides {
    val overrides = css2sld2dom("/overrides.css")

    val fills =
      overrides \\ "CssParameter" filter {n => (n \ "@name").text == "fill"}
    val strokes =
      overrides \\ "CssParameter" filter {n => (n \ "@name").text == "stroke"}
    val marks = overrides \\ "WellKnownName" 

    fills must have (length(2))
    fills.map(_.text).toList.removeDuplicates must have (length(2))

    strokes must have (length(2))
    strokes.map(_.text).toList.removeDuplicates must have (length(2))

    marks must have (length(2))
    marks.map(_.text).toList.removeDuplicates must have (length(2))
  }

  @Test def styledMarks {
    val defaultPoint = css2sld2dom("/default_point.css")
    val simpleLine = css2sld2dom("/cookbook/line_simpleline.css")
    val graphicPoint = css2sld2dom("/cookbook/point_pointasgraphic.css")
    val capitals = css2sld2dom("/capitals.css")
    val camping = css2sld2dom("/camping.css")
    val hospital = css2sld2dom("/hospital.css")
    val overrides = css2sld2dom("/mark-overrides.css")
    val rotatedSquare = css2sld2dom("/cookbook/point_rotatedsquare.css")

    defaultPoint \\ "Rule" must have (length(1))
    defaultPoint \\ "PolygonSymbolizer" must have (length(0))
    defaultPoint \\ "Fill" \ "CssParameter" must have (length(1))
    defaultPoint \\ "Stroke" must have (length(0))

    simpleLine \\ "Graphic" must have (length(0))

    graphicPoint \\ "Size" must have (length(0))

    capitals \\ "LineSymbolizer" must have (length(0))
    capitals \\ "Stroke" \ "CssParameter" must have (length(1))
    capitals \\ "PolygonSymbolizer" must have (length(0))
    capitals \\ "Fill" \ "CssParameter" must have (length(1))

    camping \\ "PolygonSymbolizer" \\ "Mark" \ "Stroke" must have (length(1))
    camping \\ "PolygonSymbolizer" \\ "Mark" \ "Fill" must have (length(0))
    camping \\ "PointSymbolizer" \\ "Mark" \ "Fill" must have (length(1))
    camping \\ "PointSymbolizer" \\ "Mark" \ "Stroke" must have (length(0))

    hospital \\ "PolygonSymbolizer" must have length(0)
    hospital \\ "PointSymbolizer" \\ "Mark" must have length(2)
    hospital \\ "PointSymbolizer" \\ "Mark" \ "Fill" must have length(2)
    hospital \\ "PointSymbolizer" \\ "Mark" \ "Fill" \ "CssParameter" must have length(2)

    rotatedSquare \\ "Graphic" \ "Rotation" must have length(1)
    (rotatedSquare \\ "Graphic" \ "Rotation").text.trim must be ("45")

    overrides \\ "WellKnownName" must have length(2)
    (overrides \\ "WellKnownName" map (_.text) toList).removeDuplicates must have length(2)

    val fills = overrides \\ "Mark" \ "Fill"
    fills must have length(2)
    (fills \ "CssParameter" 
      filter (n => (n \ "@name" text) == "fill") 
      map (_.text)
    ).toList.removeDuplicates must have (length(2))
  }
}
