package org.geoscript.geocss

import org.specs._

/**
 * Acceptance tests against the DOM of a generated SLD
 */
class SLDTest extends Specification with util.DataTables {
  def css2sld2dom(filename: String) = {
    val url = getClass.getResource(filename)
    val stream = getClass.getResourceAsStream(filename)
    require(stream != null, "Failed to load " + url)
    val styleSheet = CssParser.parse(stream).get
    val style = new Translator(Some(url)).css2sld(styleSheet)
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
    width.text.trim must_== "3" 
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
    val textSym = (minimal \\ "Rule" \ "TextSymbolizer").head;
    (textSym \ "Label") must haveSize(1)
    (textSym \ "Label").text must_== ("Label")
    (textSym \ "Halo" \ "Radius").head.text.trim.toDouble must_== 2d
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
    (polyGraphic \ "Rotation").text.trim.toDouble must_== 12d

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
    (lineGraphic \ "Rotation").text.trim.toDouble must_== 12d

    val pointsyms = comprehensive \\ "Rule" \ "PointSymbolizer"
    pointsyms must haveSize(1)
    val pointgraphic = pointsyms \\ "Graphic"
    (pointgraphic \ "Mark" \ "WellKnownName").text.trim must_== ("circle")
    (pointgraphic \ "Opacity")
      .text.trim.toDouble must be closeTo(0.7 +/- 0.0001)
    (pointgraphic \ "Size").text.trim must_== ("16")
    (pointgraphic \ "Rotation").text.trim.toDouble must_== 12d

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
    "default point style from geoserver" >> {
      val defaultPoint = css2sld2dom("/default_point.css")
      defaultPoint \\ "Rule" must haveSize(1)
      defaultPoint \\ "Filter" must beEmpty
      defaultPoint \\ "PolygonSymbolizer" must haveSize(0)
      defaultPoint \\ "Fill" \ "CssParameter" must haveSize(1)
      defaultPoint \\ "Stroke" must haveSize(0)
    }

    "Graphic tag not generated when no symbol/image is use" >> {
      val simpleLine = css2sld2dom("/cookbook/line_simpleline.css")
      simpleLine \\ "Graphic" must haveSize(0)
    }

    "Size tag not generated when left unspecified" >> {
      val graphicPoint = css2sld2dom("/cookbook/point_pointasgraphic.css")
      graphicPoint \\ "Size" must haveSize(0)
    }

    "fill/stroke for marks should not generate line/polygon symbolizers in SLD" >> {
    val capitals = css2sld2dom("/capitals.css")
      capitals \\ "LineSymbolizer" must haveSize(0)
      capitals \\ "Stroke" \ "CssParameter" must haveSize(1)
      capitals \\ "PolygonSymbolizer" must haveSize(0)
      capitals \\ "Fill" \ "CssParameter" must haveSize(1)
    }

    "marks and pattern fills should be themable independently" >> {
      val camping = css2sld2dom("/camping.css")
      camping \\ "PolygonSymbolizer" \\ "Mark" \ "Stroke" must haveSize(1)
      camping \\ "PolygonSymbolizer" \\ "Mark" \ "Fill" must haveSize(0)
      camping \\ "PointSymbolizer" \\ "Mark" \ "Fill" must haveSize(1)
      camping \\ "PointSymbolizer" \\ "Mark" \ "Stroke" must haveSize(0)
    }

    "multiple marks for a single feature should be themable independently" >> {
      val hospital = css2sld2dom("/hospital.css")
      hospital \\ "PolygonSymbolizer" must haveSize(0)
      hospital \\ "PointSymbolizer" \\ "Mark" must haveSize(2)
      hospital \\ "PointSymbolizer" \\ "Mark" \ "Fill" must haveSize(2)
      hospital \\ "PointSymbolizer" \\ "Mark" \ "Fill" \ "CssParameter" must haveSize(2)
    }

    "cascading inheritance for mark style properties" >> {
      val overrides = css2sld2dom("/mark-overrides.css")
      overrides \\ "WellKnownName" must haveSize(2)
      (overrides \\ "WellKnownName" map (_.text)).distinct must haveSize(2)

      val fills = overrides \\ "Mark" \ "Fill"
      fills must haveSize(2)
      (fills \ "CssParameter" 
        filter (n => (n \ "@name" text) == "fill") 
        map (_.text)
      ).distinct must haveSize(2)
    }
  
    "rotation property is encoded" >> {
      val rotatedSquare = css2sld2dom("/cookbook/point_rotatedsquare.css")
      rotatedSquare \\ "Graphic" \ "Rotation" must haveSize(1)
      (rotatedSquare \\ "Graphic" \ "Rotation").text.trim.toDouble must_== 45d
    }
  }

  "Shield graphics" in {
    val roads = css2sld2dom("/shield.css")
    roads \\ "FeatureTypeStyle" must haveSize(1)
    roads \\ "Rule" must haveSize(1)
    val symbolizer = (roads \\ "Rule" \ "TextSymbolizer").head
    symbolizer \\ "Graphic" must haveSize(1)
    (symbolizer \\ "Fill" \ "CssParameter").text must_== "#008000"
    (symbolizer \\ "Stroke" \ "CssParameter").text must_== "#32cd32"
    val vendorOpts = (symbolizer \\ "VendorOption") map (_ \ "@name" text)
    vendorOpts must contain("graphic-margin")
    vendorOpts must contain("graphic-resize")
  }

  "Styling separate properties independently" in {
    val roads = css2sld2dom("/roads.css")

    roads \\ "FeatureTypeStyle" must haveSize(3)

    roads \\ "Rule" must haveSize(9)
    roads \\ "Rule" map (_ \ "Filter") must notBeEmpty.toIterable

    for (roadType <- Seq("highway", "secondary", "local-roads")) {
      def forType(ns: scala.xml.NodeSeq): Boolean =
        ns \\ "Filter" \\ "Literal" forall(_.text == roadType)

      (roads \\ "FeatureTypeStyle")
        .filter(forType)
        .aka("ftStyles with filters for: " + roadType)
        .must(haveSize(1))

      (roads \\ "Rule")
        .filter(forType)
        .aka("rules with filters for: " + roadType)
        .must(haveSize(3))
    }
  }

  "Labels with multiple expressions should be implicitly concatenated" in {
    val labels = css2sld2dom("/complex-label.css")

    object callStrConcat extends matcher.Matcher[scala.xml.Node] {
      def apply(n: => scala.xml.Node) = 
        Triple(
          (n \ "@name" text) == "strConcat",
          "node %s calls the strConcat function".format(n),
          "node %s does not call the strConcat function".format(n)
        )
    }

    labels \\ "Function" must (notBeEmpty and callStrConcat.toIterable)
  }

  "Everything should convert without throwing Exceptions" in {
    val testData = Seq(
      "/badstyle.css", "/camping.css", "/capitals.css", "/complex-scales.css",
      "/comprehensive.css", "/default_point.css", "/exclusive.css", "/filters.css",
      "/gt-opts.css", "/hospital.css", "/mark-overrides.css", "/marks.css",
      "/minimal.css", "/motorvag.css", "/overrides.css", "/percentage.css",
      "/planet_polygon.css", "/railroad.css", "/roads.css", "/scales.css",
      "/stacked-symbolizers.css", "/states.css", "/test-basic.css", "/test.css",
      "/typenames.css", "/complex-label.css")

    for (file <- testData)
      { css2sld2dom(file) } must not(throwAn[Exception])
  }
}
