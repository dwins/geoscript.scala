package org.geoscript.geocss

import scala.util.parsing.input.CharSequenceReader

import org.scalatest.FunSuite
import org.scalatest.matchers.{ Matcher, MatchResult, ShouldMatchers }

/**
 * Tests for specific low-level productions in the CSS grammar
 */
class GrammarTest extends FunSuite with ShouldMatchers {
  import CssParser._

  def failOn(text: String): Matcher[CssParser.Parser[_]] = not(succeedOn(text))

  def succeedOn(text: String): Matcher[CssParser.Parser[_]] =
    new Matcher[CssParser.Parser[_]] {
      def apply(p: CssParser.Parser[_]): MatchResult =
        new MatchResult(
          CssParser.parseAll(p, text).successful,
          "Parser %s did not accept %s" format(p, text),
          "Parser %s accepted %s" format(p, text)
        )
    }

  test("property names") {
    propname should failOn("123")
    propname should failOn("-123")
    propname should succeedOn("abc")
    propname should succeedOn("abc-123")
    propname should succeedOn("-gt-magic")
  }

  test("numbers") {
    number should succeedOn("123")
    number should succeedOn("1.23")
    number should succeedOn(".123")
    number should succeedOn("123.")
    number should succeedOn("-123")
    number should succeedOn("-.123")

    number should failOn(".-123")
    number should failOn("1.2.3")
    number should failOn("1..23")
    number should failOn("1-23")
    number should failOn("one, dash, twenty-three")
  }

  test("percentages") {
    percentage should succeedOn("12%")
    percentage should succeedOn("1.2%")
    percentage should succeedOn("-12%")
    percentage should succeedOn("-.12%")

    percentage should failOn(".-12%")
    percentage should failOn("12")
    percentage should failOn("%")
  }

  test("urls") {
    url should succeedOn ("url(http://example.com/foo.png)")

    url should failOn("foo()")
    url should failOn("url('http://example.com/icon.png')")
    url should failOn("""foo("http://example.com/icon.png")""")
    url should failOn("rgb(50) 150, 250)")
    url should failOn("rgb(50,150,250)")
  }

  test("functions") {
    function should succeedOn("foo()")
    function should succeedOn("url('http://example.com/icon.png')")
    function should succeedOn("""foo("http://example.com/icon.png")""")
    function should succeedOn("rgb(50, 150, 250)")
    function should succeedOn("rgb(50,150,250)")
  }
}
