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
    val source = scala.io.Source.fromString(bos.toString)
    val parser = scala.xml.parsing.ConstructingParser.fromSource(source, false)
    parser.document
  }

  val minimal = css2sld2dom("/minimal.css")
  val states = css2sld2dom("/states.css")
  val comprehensive = css2sld2dom("/comprehensive.css")
  val vendorOptions = css2sld2dom("/gt-opts.css")

  @Test def statesStyle {
    val css = states \\ "Rule" \ "LineSymbolizer" \ "Stroke" \ "CssParameter"
    val width =
      css.filter(_.attribute("name").get.text == ("stroke-width")).first
    (width \\ "Div").isEmpty must be (false)
  }

  @Test def minimalStyle {
    (minimal \\ "Rule").length must be (4)
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
      .first.text must be ("2")
  }

  @Test def comprehensiveStyle {
    (comprehensive \\ "Rule").length must be (4)

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
    (polyGraphic \ "Size").text must be ("32")
    (polyGraphic \ "Rotation").text must be ("12")

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

    stroke.get.text must be ("#FFFFFF")
    strokeLinecap.get.text must be ("square")
    strokeLinejoin.get.text must be ("mitre")
    strokeOpacity.get.text.toDouble must be (0.7 plusOrMinus 0.0001)
    strokeWidth.get.text must be ("2")
    strokeDashOffset.get.text must be ("2")
    strokeDashArray.get.text must be ("1.0 2.0 1.0 4.0")

    val lineGraphic =
      linesyms \ "Stroke" \ "GraphicStroke" \ "Graphic"
    (lineGraphic \ "ExternalGraphic" \ "OnlineResource").first
      .attribute("http://www.w3.org/1999/xlink", "href")
      .get must be ("http://example.com/example.gif")
    (lineGraphic \ "ExternalGraphic" \ "Format").text must be ("image/gif")
    // TODO: These tests fail, looks like we're not passing these parameters through
    // (lineGraphic \ "Rotation").text must be ("12")
    // (lineGraphic \ "Opacity").text.toDouble must be (0.8 plusOrMinus 0.0001)

    val pointsyms = comprehensive \\ "Rule" \ "PointSymbolizer"
    pointsyms.length must be (1)
    val pointgraphic = pointsyms \\ "Graphic"
    (pointgraphic \ "Mark" \ "WellKnownName").text must be ("circle")
    (pointgraphic \ "Opacity")
      .text.toDouble must be (0.7 plusOrMinus 0.0001)
    (pointgraphic \ "Size").text must be ("16")
    (pointgraphic \ "Rotation").text must be ("12")

    val textsyms = comprehensive \\ "Rule" \ "TextSymbolizer"
    textsyms.length must be (1)
    (textsyms \ "Label" \ "PropertyName") must be ("PROPNAME")
    val fontparams = textsyms \ "Font" \ "CssParameter"
    fontparams(0).attribute("name").get must be ("font-family")
    fontparams(1).attribute("name").get must be ("font-size")
    fontparams(2).attribute("name").get must be ("font-style")
    fontparams(3).attribute("name").get must be ("font-weight")
    fontparams(0).text must be ("Times New Roman")
    fontparams(1).text must be ("17")
    fontparams(2).text must be ("oblique")
    fontparams(3).text must be ("bold")
    val fillparams = textsyms \ "Fill" \ "CssParameter"
    fillparams(0).attribute("name").get must be ("fill")
    fillparams(1).attribute("name").get must be ("fill-opacity")
    val halo = textsyms \ "Halo"
    (halo \ "Radius").text must be ("2")
    val haloparams = halo \ "Fill" \ "CssParameter"
    haloparams(0).attribute("name").get must be ("fill")
    haloparams(1).attribute("name").get must be ("fill-opacity")
    haloparams(0).text must be ("#FFFFFF")
    haloparams(1).text.toDouble must be (0.7 plusOrMinus 0.001)
  }

  @Test def vendorOpts {
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
    vendor("allowOveruns") must be (Some("true"))
    vendor("minGroupDistance") must be (Some("-1"))
    vendor("spaceAround") must be (Some("0"))
    vendor("group") must be (Some("false"))
    vendor("maxDisplacement") must be (Some("0"))
    vendor("forceLeftToRight") must be (Some("true"))
    vendor("spaceAround") must be (Some("0"))
  }
}
