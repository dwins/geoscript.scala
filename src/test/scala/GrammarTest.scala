package org.geoserver.community.css

import scala.util.parsing.input.CharSequenceReader
import org.scalatest.matchers.{ BeMatcher, MatchResult }
import org.scalatest.junit.{JUnitSuite, MustMatchersForJUnit}
import org.junit.Test

/**
 * Tests for specific low-level productions in the CSS grammar
 */
class GrammarTest extends JUnitSuite with MustMatchersForJUnit {
  val successful = new BeMatcher[CssParser.ParseResult[_]] { 
    def apply(result: CssParser.ParseResult[_]) = 
      result match {
        case success: CssParser.Success[_] => 
          MatchResult(true, "", "Parsed successfully")
        case nosuccess: CssParser.NoSuccess => 
          MatchResult(false, "\n" + nosuccess.toString, "")
      }
  }
  @Test def propertyNames {
    def parse(s: String) = 
      CssParser.parseAll(CssParser.propname, new CharSequenceReader(s))

    parse("123").successful must be (false)
    parse("-123").successful must be (false)
    parse("abc").successful must be (true)
    parse("abc-123").successful must be (true)
    parse("-gt-magic").successful must be (true)
  }

  @Test def numbers {
    def parse(s: String) = 
      CssParser.parseAll(CssParser.number, new CharSequenceReader(s))

    parse("123").successful must be (true)
    parse("1.23").successful must be (true)
    parse(".123").successful must be (true)
    parse("123.").successful must be (true)
    parse("-123").successful must be (true)
    parse("-.123").successful must be (true)
    parse(".-123").successful must be (false)
    parse("1.2.3").successful must be (false)
    parse("1..23").successful must be (false)
    parse("1-23").successful must be (false)
    parse("one dash twenty-three").successful must be (false)
  }

  @Test def percentages {
    def parse(s: String) = 
      CssParser.parseAll(CssParser.percentage, new CharSequenceReader(s))

    parse("12%").successful must be (true)
    parse("1.2%").successful must be (true)
    parse("-12%").successful must be (true)
    parse("-.12%").successful must be (true)
    parse(".-12%").successful must be (false)
    parse("12").successful must be (false)
    parse("%").successful must be (false)
  }

  @Test def url {
    def parse(s: String) = 
      CssParser.parseAll(CssParser.url, new CharSequenceReader(s))

    parse("url(http://example.com/foo.png)") must be (successful)
  }

  @Test def functions {
    def parse(s: String) =
      CssParser.parseAll(CssParser.function, new CharSequenceReader(s))
    parse("foo()") must be (successful)
    parse("url('http://example.com/icon.png')") must be (successful)
    parse("foo(\"http://example.com/icon.png\")") must be (successful)
    parse("rgb(50, 150, 250)") must be (successful)
    parse("rgb(50,150,250)") must be (successful)
  }
}
