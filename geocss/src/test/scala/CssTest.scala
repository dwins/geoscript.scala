package org.geoscript.geocss

import scala.util.parsing.input.StreamReader
import org.specs._

/**
 * General-purpose, one-size-fits-all tests for MSS parsing operations
 */
class CssTest extends Specification with util.DataTables {
  import CssOps._
  val Translator = new Translator

  "The test data should parse successfully" in {
    "resource path"      | "rule count" |> 
    "/test-basic.css"    ! 2            |
    "/states.css"        ! 4            |
    "/railroad.css"      ! 2            |
    "/minimal.css"       ! 1            |
    "/comprehensive.css" ! 1            |
    "/scales.css"        ! 3            |
    "/marks.css"         ! 2            |
    "/gt-opts.css"       ! 1            |
    "/default_point.css" ! 2            |
    "/hospital.css"      ! 3            | { (file, count) => 
      val stream = getClass.getResourceAsStream(file)
      stream must not be null
      val rules = CssParser.parse(stream)
      rules.successful must beTrue
      rules.get must haveSize(count)
    }
  }

  "producing an SLD should not throw any exceptions" in {
    val in = getClass().getResourceAsStream("/scales.css")
    val styleSheet = CssParser.parse(in).get
    val style = Translator.css2sld(styleSheet)
    val xform = new org.geotools.styling.SLDTransformer
    xform.setIndentation(2)
    val bytes = new java.io.ByteArrayOutputStream
    xform.transform(style, bytes)
    bytes.toByteArray.isEmpty must beFalse
  }

  "Specificity comparisons should work as in the CSS standard" in { 
    val any = AcceptSelector
    val id  = IdSelector("states.9")
    val cql = ExpressionSelector("STATE_NAME LIKE '%ia'")

    Specificity(id) must be_> (Specificity(cql))
    Specificity(cql) must be_> (Specificity(any))
    Specificity(id) must be_> (Specificity(any))
  }

  "Rule expansions should repeat properties as needed" in {
    val xs = List(
      Property("stroke", List(Literal("red")) :: List(Literal("green")) :: Nil),
      Property("opacity", List(Literal("0.70")) :: Nil),
      Property("width", 
        List(Literal("10")) :: List(Literal("8")) :: List(Literal("6")) :: Nil
      )
    )

    val expanded = expand(xs, "stroke")
    expanded must haveSize(2)
    expanded(0) must havePairs(
      "stroke"  -> List(Literal("red")),
      "opacity" -> List(Literal("0.70")),
      "width"   -> List(Literal("10"))
    )

    expanded(1) must havePairs(
      "stroke"  -> List(Literal("green")),
      "opacity" -> List(Literal("0.70")),
      "width"   -> List(Literal("8"))
    )

    expand(xs, "opacity") must haveSize(1)
    expand(xs, "width") must haveSize(3)
    expand(xs, "fill") must haveSize(0)
  }
}
