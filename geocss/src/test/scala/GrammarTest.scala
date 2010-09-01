package org.geoscript.geocss

import scala.util.parsing.input.CharSequenceReader
import org.specs._

/**
 * Tests for specific low-level productions in the CSS grammar
 */
class GrammarTest extends Specification {
  "parsing property names" in {
    def parse(s: String) = 
      CssParser.parseAll(CssParser.propname, new CharSequenceReader(s))

    parse("123").successful must beFalse
    parse("-123").successful must beFalse
    parse("abc").successful must beTrue
    parse("abc-123").successful must beTrue
    parse("-gt-magic").successful must beTrue
  }

  "parsing numbers" in {
    def parse(s: String) = 
      CssParser.parseAll(CssParser.number, new CharSequenceReader(s))

    parse("123").successful must beTrue
    parse("1.23").successful must beTrue
    parse(".123").successful must beTrue
    parse("123.").successful must beTrue
    parse("-123").successful must beTrue
    parse("-.123").successful must beTrue
    parse(".-123").successful must beFalse
    parse("1.2.3").successful must beFalse
    parse("1..23").successful must beFalse
    parse("1-23").successful must beFalse
    parse("one dash twenty-three").successful must beFalse
  }

  "parsing percentages" in {
    def parse(s: String) = 
      CssParser.parseAll(CssParser.percentage, new CharSequenceReader(s))

    parse("12%").successful must beTrue
    parse("1.2%").successful must beTrue
    parse("-12%").successful must beTrue
    parse("-.12%").successful must beTrue
    parse(".-12%").successful must beFalse
    parse("12").successful must beFalse
    parse("%").successful must beFalse
  }

  "parsing urls" in {
    def parse(s: String) = 
      CssParser.parseAll(CssParser.url, new CharSequenceReader(s))

    parse("url(http://example.com/foo.png)").successful must beTrue
  }

  "parsing CSS functions" in {
    def parse(s: String) =
      CssParser.parseAll(CssParser.function, new CharSequenceReader(s))
    parse("foo()").successful must beTrue
    parse("url('http://example.com/icon.png')").successful must beTrue
    parse("foo(\"http://example.com/icon.png\")").successful must beTrue
    parse("rgb(50, 150, 250)").successful must beTrue
    parse("rgb(50,150,250)").successful must beTrue
  }
}
