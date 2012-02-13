package org.geoscript.geocss

import scala.util.parsing.input.CharSequenceReader
import org.specs2._

/**
 * Tests for specific low-level productions in the CSS grammar
 */
class GrammarTest extends Specification 
with matcher.ParserMatchers
with matcher.DataTables
{
  val parsers = CssParser
  def is = 
    "parsing property names" ^ 
      {
        category(
          "Known bad",
          List("123", "-123"),
          CssParser.propname must failOn(_: String)
        )
      } ^ {
        category(
          "Known good",
          List("abc", "abc-123", "-gt-magic"),
          CssParser.propname must succeedOn(_: String)
        )
      } ^ end ^
    "parsing numbers" ^
      {
        category(
          "Known good",
          List("123", "1.23", ".123", "123.", "-123", "-.123"),
          CssParser.number must succeedOn(_: String)
        )
      } ^ {
        category(
          "Known bad", 
          List(".-123", "1.2.3", "1..23", "1-23", "one, dash, twenty-three"),
          CssParser.number must failOn(_: String)
        )
     } ^ end ^
   "parsing percentages" ^ 
     category(
       "Known good",
       List("12%", "1.2%", "-12%", "-.12%"),
       CssParser.percentage must succeedOn(_: String)
     ) ^
     category(
       "Known bad",
       List(".-12%", "12", "%"),
       CssParser.percentage must failOn(_: String)
     ) ^ end ^
   "parsing urls" ^ 
     category(
       "Known good",
       List("url(http://example.com/foo.png)"),
       CssParser.url must succeedOn(_: String)
     ) ^ end ^
   "parsing CSS functions" ^
     category(
       "Known good",
       List(
         "foo()",
         "url('http://example.com/icon.png')",
         """foo("http://example.com/icon.png")""",
         "rgb(50, 150, 250)",
         "rgb(50,150,250)"
       ),
       CssParser.function must succeedOn(_: String)
     )

  def category[A, B](
    name: String,
    examples: Seq[A],
    test: A => matcher.MatchResult[B]
  ) =
    examples.foldLeft(name: specification.Fragments) {
      (fragments, v) => fragments ^ v.toString ! test(v)
    } ^ bt
}
