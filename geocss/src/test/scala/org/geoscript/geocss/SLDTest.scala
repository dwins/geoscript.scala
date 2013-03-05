package org.geoscript.geocss

import org.geoscript.geocss.testing._

import org.scalatest.FunSuite
import org.scalatest.matchers.{ BeMatcher, ShouldMatchers }

/**
 * Acceptance tests against the DOM of a generated SLD
 */
class SLDTest extends FunSuite with ShouldMatchers {
  def css2sld2dom(filename: String) = {
    val url = getClass.getResource(filename)
    val stream = getClass.getResourceAsStream(filename)
    require(stream != null, "Failed to load " + filename)
    val styleSheet = CssParser.parse(stream).get
    val style = new Translator(Some(url)).css2sld(styleSheet)
    val bos = new java.io.ByteArrayOutputStream
    val xform = new org.geotools.styling.SLDTransformer
    xform.setIndentation(2)
    xform.transform(style, bos)
    scala.xml.XML.loadString(bos.toString())
  }

  test("States style") {
    val states = css2sld2dom("/states.css")

    val css = states \\ "Rule" \ "LineSymbolizer" \ "Stroke" \ "CssParameter"
    val titles = (states \\ "Rule" \ "Title" map (_.text)).toSet
    titles should containAll(
      "Persons < 2M", "2M < Persons < 4M", "4M < Persons"
    )
    val width =
      css.filter(_.attribute("name").get.text == ("stroke-width")).head
    width.text.trim should equal("3")
  }

  test("The minimal properties required to activate each type of symbolizer") {
    val minimal = css2sld2dom("/minimal.css")

    (minimal \\ "Rule") should have size(1)
    (minimal \\ "Rule" \ "PolygonSymbolizer") should have size(1)
    (minimal \\ "Rule" \ "PolygonSymbolizer" \ "Fill" \
      "CssParameter").head.attribute("name").get.text should equal ("fill")
    (minimal \\ "Rule" \ "PolygonSymbolizer" \ "Fill" \
      "CssParameter").head.text should equal ("#ff0000")

    val lineSymbolizers = (minimal \\ "Rule" \ "LineSymbolizer")
    lineSymbolizers should have size(1)
    val cssParameters = (lineSymbolizers \ "Stroke" \ "CssParameter")
    cssParameters.head.attribute("name").get.text should equal ("stroke")
    cssParameters.head.text should equal("#ff0000")

    val pointSymbolizers = (minimal \\ "Rule" \ "PointSymbolizer")
    pointSymbolizers should have size(1)
    val marks = (pointSymbolizers \ "Graphic" \ "Mark")
    marks should have size(1)
    val name = (marks \ "WellKnownName").head.text
    name should equal("star")

    (minimal \\ "Rule" \ "TextSymbolizer") should have size(1)
    val textSym = (minimal \\ "Rule" \ "TextSymbolizer").head;
    (textSym \ "Label") should have size(1)
    (textSym \ "Label").text should equal ("Label")
    (textSym \ "Halo" \ "Radius").head.text.trim.toDouble should equal(2d)
  }

  test("All properties that can be used outside of marks") {
    val comprehensive = css2sld2dom("/comprehensive.css")

    (comprehensive \\ "Rule") should have size(1)

    val polysyms = (comprehensive \\ "Rule" \ "PolygonSymbolizer")
    polysyms should have size(1)
    val polyparams = (polysyms \ "Fill" \ "CssParameter")
    polyparams(0).attribute("name").get.text should equal ("fill")
    polyparams(1).attribute("name").get.text should equal ("fill-opacity")
    (polyparams)(0).text should equal ("#FFFFFF")
    (polyparams)(1).text.toDouble should be(closeTo(0.7, 0.0001))
    val polyGraphic = polysyms \ "Fill" \ "GraphicFill" \ "Graphic"
    (polyGraphic \ "ExternalGraphic" \ "OnlineResource")
      .head.attribute("http://www.w3.org/1999/xlink", "href")
      .get.text should equal ("http://example.com/example.png")
    (polyGraphic \ "ExternalGraphic" \ "Format").text should equal ("image/png")
    (polyGraphic \ "Size").text.trim should equal ("32")
    (polyGraphic \ "Rotation").text.trim.toDouble should equal(12d)

    val linesyms = (comprehensive \\ "Rule" \ "LineSymbolizer")
    linesyms should have size(1)
    val lineparams = (linesyms \ "Stroke" \ "CssParameter")
    val stroke = lineparams find (_.attribute("name").exists(_.text=="stroke"))
    val strokeLinecap = lineparams find (_.attribute("name").exists(_.text=="stroke-linecap"))
    val strokeLinejoin = lineparams find (_.attribute("name").exists(_.text=="stroke-linejoin"))
    val strokeOpacity = lineparams find (_.attribute("name").exists(_.text=="stroke-opacity"))
    val strokeWidth = lineparams find (_.attribute("name").exists(_.text=="stroke-width"))
    val strokeDashOffset = lineparams find (_.attribute("name").exists(_.text=="stroke-dashoffset"))
    val strokeDashArray = lineparams find (_.attribute("name").exists(_.text=="stroke-dasharray"))

    stroke.get.text.trim should equal ("#FFFFFF")
    strokeLinecap.get.text.trim should equal ("square")
    strokeLinejoin.get.text.trim should equal ("mitre")
    strokeOpacity.get.text.trim.toDouble should be(closeTo(0.7, 0.0001))
    strokeWidth.get.text.trim should equal ("2")
    strokeDashOffset.get.text.trim should equal ("2")
    strokeDashArray.get.text.trim should equal ("1.0 2.0 1.0 4.0")

    val lineGraphic =
      linesyms \ "Stroke" \ "GraphicStroke" \ "Graphic"
    (lineGraphic \ "ExternalGraphic" \ "OnlineResource").head
      .attribute("http://www.w3.org/1999/xlink", "href")
      .get.text should equal ("http://example.com/example.gif")
    (lineGraphic \ "ExternalGraphic" \ "Format").text.trim should equal ("image/gif")
    (lineGraphic \ "Rotation").text.trim.toDouble should equal(12d)

    val pointsyms = comprehensive \\ "Rule" \ "PointSymbolizer"
    pointsyms should have size(1)
    val pointgraphic = pointsyms \\ "Graphic"
    (pointgraphic \ "Mark" \ "WellKnownName").text.trim should equal ("circle")
    (pointgraphic \ "Opacity")
      .text.trim.toDouble should be(closeTo(0.7, 0.0001))
    (pointgraphic \ "Size").text.trim should equal("16")
    (pointgraphic \ "Rotation").text.trim.toDouble should equal(12d)

    val textsyms = comprehensive \\ "Rule" \ "TextSymbolizer"
    textsyms.length should equal (1)
    (textsyms \ "Label" \ "PropertyName").text should equal ("PROPNAME")
    textsyms \ "LabelPlacement" should have size(1)
    textsyms \ "LabelPlacement" \ "PointPlacement" \ "AnchorPoint" should have size(1)
    (textsyms \ "LabelPlacement" \\ "AnchorPointX").text.trim should equal ("0.5")
    (textsyms \ "LabelPlacement" \\ "AnchorPointY").text.trim should equal ("0.0")
    textsyms \ "LabelPlacement" \ "PointPlacement" \ "Displacement" should have size(1)
    (textsyms \ "LabelPlacement" \\ "DisplacementX").text.trim should equal ("1")
    (textsyms \ "LabelPlacement" \\ "DisplacementY").text.trim should equal ("2")
    val fontparams = textsyms \ "Font" \ "CssParameter"
    fontparams(0).attribute("name").get.text should equal ("font-family")
    fontparams(1).attribute("name").get.text should equal ("font-size")
    fontparams(2).attribute("name").get.text should equal ("font-style")
    fontparams(3).attribute("name").get.text should equal ("font-weight")
    fontparams(0).text.trim should equal ("Times New Roman")
    fontparams(1).text.trim should equal ("17")
    fontparams(2).text.trim should equal ("oblique")
    fontparams(3).text.trim should equal ("bold")
    val fillparams = textsyms \ "Fill" \ "CssParameter"
    fillparams(0).attribute("name").get.text should equal ("fill")
    fillparams(1).attribute("name").get.text should equal ("fill-opacity")
    val halo = textsyms \ "Halo"
    (halo \ "Radius").text.trim should equal ("2")
    val haloparams = halo \ "Fill" \ "CssParameter"
    haloparams(0).attribute("name").get.text should equal ("fill")
    haloparams(1).attribute("name").get.text should equal ("fill-opacity")
    haloparams(0).text.trim should equal ("#FFFFFF")
    haloparams(1).text.trim.toDouble should be(closeTo(0.7, 0.001))
  }

  test("GeoTools vendor options should be passed through") {
    val vendorOptions = css2sld2dom("/gt-opts.css")

    def vendor(name: String): Option[String] = {
      (vendorOptions \\ "VendorOption") find {
        _.attribute("name") map (_.text == name) getOrElse(false)
      } map { 
        _.child.text 
      }
    }

    // all vendor options should be direct children of textsymbolizers now
    vendorOptions \\ "VendorOption" should equal (
      vendorOptions \\ "TextSymbolizer" \ "VendorOption"
    )

    vendor("allGroup") should be(Some("false"))
    vendor("maxAngleDelta") should be(Some("22.5"))
    vendor("followLine") should be(Some("false"))
    vendor("autoWrap") should be(Some("0"))
    vendor("repeat") should be(Some("0"))
    vendor("goodnessOfFit") should be(Some("0.50"))
    vendor("conflictResolution") should be(Some("true"))
    vendor("removeOverlaps") should be(Some("false"))
    vendor("allowOverrun") should be(Some("true"))
    vendor("minGroupDistance") should be(Some("-1"))
    vendor("spaceAround") should be(Some("0"))
    vendor("group") should be(Some("false"))
    vendor("maxDisplacement") should be(Some("0"))
    vendor("forceLeftToRight") should be(Some("true"))
    vendor("spaceAround") should be(Some("0"))

    (vendorOptions \\ "Priority" \ "PropertyName").text should equal ("priority")
  }


  test("Mixing selector properties doensn't produce empty rules") {
    val planet = css2sld2dom("/planet_polygon.css")

    for (rule <- planet \\ "Rule") {
      val text = rule \\ "TextSymbolizer"
      val mark = rule \\ "PointSymbolizer"
      val stroke = rule \\ "LineSymbolizer"
      val fill = rule \\ "PolygonSymbolizer"
      (text ++ mark ++ stroke ++ fill) should not(have(size(0)))
    }
  }

  test("It should be possible to 'stack' symbolizers") {
    val stackedSymbolizers = css2sld2dom("/stacked-symbolizers.css")

    stackedSymbolizers \\ "Rule" should have size(1)
    val rule = (stackedSymbolizers \\ "Rule").head
    val colors = 
    (rule \ "LineSymbolizer" \ "Stroke" \ "CssParameter") filter (
      _.attribute("name").exists(_.text == "stroke")
    ) map (_.text.trim)
    colors(0) should equal ("#ff0000")
    colors(1) should equal ("#008000")
    colors(2) should equal ("#0000ff")
  }

  test("More specific rules should override properties from less specific ones") {
    val overrides = css2sld2dom("/overrides.css")

    val fills =
      overrides \\ "CssParameter" filter {n => (n \ "@name").text == "fill"}
    val strokes =
      overrides \\ "CssParameter" filter {n => (n \ "@name").text == "stroke"}
    val marks = overrides \\ "WellKnownName" 

    fills should have size(2)
    fills.map(_.text).distinct should have size(2)

    strokes should have size(2)
    strokes.map(_.text).distinct should have size(2)

    marks should have size(2)
    marks.map(_.text).distinct should have size(2)
  }

  test("default point style from geoserver"){
    val defaultPoint = css2sld2dom("/default_point.css")
    defaultPoint \\ "Rule" should have size(1)
    defaultPoint \\ "Filter" should have size(0)
    defaultPoint \\ "PolygonSymbolizer" should have size(0)
    defaultPoint \\ "Fill" \ "CssParameter" should have size(1)
    defaultPoint \\ "Stroke" should have size(0)
  }

  test("Graphic tag not generated when no symbol/image is use"){
    val simpleLine = css2sld2dom("/cookbook/line_simpleline.css")
    simpleLine \\ "Graphic" should have size(0)
  }

  test("Size tag not generated when left unspecified"){
    val graphicPoint = css2sld2dom("/cookbook/point_pointasgraphic.css")
    graphicPoint \\ "Size" should have size(0)
  }

  test("fill/stroke for marks should not generate line/polygon symbolizers in SLD"){
  val capitals = css2sld2dom("/capitals.css")
    capitals \\ "LineSymbolizer" should have size(0)
    capitals \\ "Stroke" \ "CssParameter" should have size(1)
    capitals \\ "PolygonSymbolizer" should have size(0)
    capitals \\ "Fill" \ "CssParameter" should have size(1)
  }

  test("marks and pattern fills should be themable independently"){
    val camping = css2sld2dom("/camping.css")
    camping \\ "PolygonSymbolizer" \\ "Mark" \ "Stroke" should have size(1)
    camping \\ "PolygonSymbolizer" \\ "Mark" \ "Fill" should have size(0)
    camping \\ "PointSymbolizer" \\ "Mark" \ "Fill" should have size(1)
    camping \\ "PointSymbolizer" \\ "Mark" \ "Stroke" should have size(0)
  }

  test("multiple marks for a single feature should be themable independently"){
    val hospital = css2sld2dom("/hospital.css")
    hospital \\ "PolygonSymbolizer" should have size(0)
    hospital \\ "PointSymbolizer" \\ "Mark" should have size(2)
    hospital \\ "PointSymbolizer" \\ "Mark" \ "Fill" should have size(2)
    hospital \\ "PointSymbolizer" \\ "Mark" \ "Fill" \ "CssParameter" should have size(2)
  }

  test("cascading inheritance for mark style properties"){
    val overrides = css2sld2dom("/mark-overrides.css")
    overrides \\ "WellKnownName" should have size(2)
    (overrides \\ "WellKnownName" map (_.text)).distinct should have size(2)

    val fills = overrides \\ "Mark" \ "Fill"
    fills should have size(2)
    (fills \ "CssParameter" 
      filter (n => (n \ "@name").text == "fill") 
      map (_.text)
    ).distinct should have size(2)
  }

  test("rotation property is encoded") {
    val rotatedSquare = css2sld2dom("/cookbook/point_rotatedsquare.css")
    rotatedSquare \\ "Graphic" \ "Rotation" should have size(1)
    (rotatedSquare \\ "Graphic" \ "Rotation").text.trim.toDouble should equal(45d)
  }

  test("Shield graphics") {
    val roads = css2sld2dom("/shield.css")
    roads \\ "FeatureTypeStyle" should have size(1)
    roads \\ "Rule" should have size(1)
    val symbolizer = (roads \\ "Rule" \ "TextSymbolizer").head
    symbolizer \\ "Graphic" should have size(1)
    (symbolizer \\ "Fill" \ "CssParameter").text should equal("#008000")
    (symbolizer \\ "Stroke" \ "CssParameter").text should equal("#32cd32")
    val vendorOpts = (symbolizer \\ "VendorOption") map (n => (n \ "@name").text)
    vendorOpts should contain("graphic-margin")
    vendorOpts should contain("graphic-resize")
  }

  test("Styling separate properties independently") {
    val roads = css2sld2dom("/roads.css")

    roads \\ "FeatureTypeStyle" should have size(3)

    roads \\ "Rule" should have size(9)
    for {
      r <- roads \\ "Rule"
      f <- r \ "Filter"
    } f should not have(size(0))

    for (roadType <- Seq("highway", "secondary", "local-roads")) {
      def forType(ns: scala.xml.NodeSeq): Boolean =
        ns \\ "Filter" \\ "Literal" forall(_.text == roadType)

      (roads \\ "FeatureTypeStyle")
        .filter(forType)
        .should(have size(1))

      (roads \\ "Rule")
        .filter(forType)
        .should(have size(3))
    }
  }

  test("Properties with unknown function names should be silently ignored") {
    val dom = css2sld2dom("/unknown-function.css")
    dom \\ "PointSymbolizer" should have size(0)
  }

  test("Labels with multiple expressions should be implicitly concatenated") {
    val labels = css2sld2dom("/complex-label.css")
    labels \\ "Function" should (not(have(size(0))))
    for (f <- labels \\ "Function")
      (f \ "@name").text should equal("strConcat")
  }

  test("Raster symbolizers can be encoded") {
    val raster = css2sld2dom("/raster.simple.css")
    raster \\ "RasterSymbolizer" should (not(have(size(0))))
  }

  ignore("Raster symbolizers can have ContrastEnhancement") {
    val raster = css2sld2dom("/raster.colorenhancement.css")
    (raster \\ "RasterSymbolizer" \\ "ColorEnhancement") 
      .should(not(have(size(0))))
  }

  test("Raster symbolizers can have geometry") {
    val raster = css2sld2dom("/raster.geometry.css")
    (raster \\ "RasterSymbolizer" \ "Geometry")
      .should(not(have(size(0))))
  }

  test("Raster symbolizers can have opacity") {
    val raster = css2sld2dom("/raster.opacity.css")
    (raster \\ "RasterSymbolizer" \ "Opacity")
      .should(not(have(size(0))))
  }

  test("Raster symbolizers can have a colormap") {
    val raster = css2sld2dom("/raster.colormap.css")
    (raster \\ "RasterSymbolizer" \ "ColorMap")
      .should(not(have(size(0))))
  }

  test("Everything should convert without throwing Exceptions") {
    val testData = Seq(
      "/badstyle.css", "/camping.css", "/capitals.css", "/complex-scales.css",
      "/comprehensive.css", "/default_point.css", "/exclusive.css", "/filters.css",
      "/gt-opts.css", "/hospital.css", "/mark-overrides.css", "/marks.css",
      "/minimal.css", "/motorvag.css", "/overrides.css", "/percentage.css",
      "/planet_polygon.css", "/railroad.css", "/roads.css", "/scales.css",
      "/stacked-symbolizers.css", "/states.css", "/test-basic.css", "/test.css",
      "/typenames.css", "/complex-label.css", "/raster.colorenhancement.css",
      "/raster.colormap.css", "/raster.colormaptype.css", "/raster.gamma.css",
      "/raster.geometry.css", "/raster.opacity.css", "/raster.simple.css")

    // no assertion, just checking that no exceptions are thrown
    for (file <- testData) css2sld2dom(file)
  }
}
