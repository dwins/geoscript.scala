package org.geoserver.community.css

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Reader, StreamReader}

import org.geotools.filter.text.ecql.ECQL

/**
 * A parser for CSS syntax, extended a bit with CQL expressions and filters.
 * parse() convenience methods are defined for java.io.InputStream and
 * java.io.Reader sources.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
object CssParser extends RegexParsers {
  override val whiteSpace = """(\s|/\*([^/]|[^*]/)*\*/)+"""r

  private val expressionPartial =
    new PartialFunction[String,Expression] {
      def apply(s: String): Expression = Expression(s)
      def isDefinedAt(exp: String): Boolean = 
        try {
          ECQL.toExpression(exp); true
        } catch {
          case _ => false
        }
    }

  private val expressionSelectorPartial =
    new PartialFunction[String,ExpressionSelector] {
      def apply(s: String): ExpressionSelector = ExpressionSelector(s)
      def isDefinedAt(exp: String): Boolean = 
        try {
          ECQL.toFilter(exp); true
        } catch {
          case _ => false
        }
    }

  private object SingleComment extends Parser[String] {
    val whiteSpace = """\s*"""r
    val comment = """/\*((?:[^/]|[^*]/)*)\*/"""r

    override def apply(in: Reader[Char]): ParseResult[String] = {
      val source = in.source
      val start = findStart(source, in.offset)
      val space = source.subSequence(start, source.length)
      comment.findPrefixMatchOf(space) match {
        case Some(cmt) => Success(cmt.group(1), in.drop(start - in.offset + cmt.end))
        case None => Failure("nothing found", in)
      }
    }

    private def findStart(source: CharSequence, offset: Int): Int = {
      val space = source.subSequence(offset, source.length)
      whiteSpace.findPrefixMatchOf(space) match {
        case Some(m) => offset + m.end
        case None => offset
      }
    }
  }

  private val ParsedComment = rep1(SingleComment) map { x => Description(x.last) }

  private val identifier = """[a-zA-Z]([a-zA-Z0-9]|[-_][a-zA-Z0-9])*"""r
  private val fid = """[a-zA-Z]([a-zA-Z0-9]|[-._][a-zA-Z0-9])*"""r

  val number: Parser[String] = """-?[0-9]*([0-9]\.|\.[0-9]|[0-9])[0-9]*"""r
  private val measure = """([0-9]*\.[0-9]+|[0-9]+)[a-zA-z]+"""r
  val percentage: Parser[String] = """-?([0-9]*\.[0-9]+|[0-9]+)%"""r
  private val string = (
    (("\"" + """([^"]|\\"])*""" + "\"")r) | 
    (("'"  + """([^']|\\'])*""" + "'" )r)
  ) map {s => s.substring(1, s.length - 1)}

  private val color = """#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})"""r
  private val literal = percentage|measure|number|string|color

  private val expression = "[" ~> ("""[^]]*"""r) <~ "]"
  private val expressionSelector =
    ("[" ~> ("""[^]@]*"""r) <~ "]") ^? expressionSelectorPartial

  val pseudoSelector =
    ("[@" ~> (identifier ~ ("[><=]"r) ~ number) <~ "]") map {
      case id ~ op ~ number => PseudoSelector(id, op, number)
    }

  val pseudoClass = (":" ~> identifier) ^^ PseudoClass

  val url = "url(" ~> """[\.!#$%&*-~:/\p{Alnum}]+""".r <~ ")"
  val function = (identifier <~ "(") ~ (repsep(value, ",") <~ ")")

  private def stripBraces(expr: String) = {
    ("""^\[|\]$"""r).replaceAllIn(expr, "")
  }

  // property names
  // * start with a hyphen or an alphabetic character
  // * if they start with a hyphen, it is followed by an alphabetic character
  // * then follow 0 or more hyphens, underscores, or alphanumeric characters
  val propname: Parser[String] = """-?[a-zA-Z][_a-zA-Z0-9-]*"""r

  val value: Parser[Value] =
    (url ^^ { x => Function("url", List(Literal(x))) }) | 
    (function ^^ { case name ~ args => Function(name, args ) }) |
    ((identifier|literal) ^^ Literal) |
    (expression ^? expressionPartial)

  private val singleDefinition = (value*)

  private val multipleDefinition = rep1sep(singleDefinition, ",")

  private val property =
    ((propname <~ ":") ~ multipleDefinition) map {
      case id ~ defs => Property(id, defs)
    }

  private val propertyList = "{" ~> rep1sep(property, ";") <~ (";"?) ~ "}"

  private val idSelector = ("#" ~> fid) map IdSelector

  private val catchAllSelector = ("*": Parser[String]) map {_ => AcceptSelector}

  private val typeNameSelector = identifier map TypenameSelector

  private val simpleSelector = (
    catchAllSelector | idSelector | typeNameSelector | pseudoSelector | 
    pseudoClass | expressionSelector
  )*

  private val selector = rep1sep(simpleSelector, ",")

  private val rule = 
    ((ParsedComment?) ~ selector ~ propertyList) map {
      case Some(comment) ~ selector ~ props => Rule(comment, selector, props)
      case None          ~ selector ~ props => Rule(Description.Empty, selector, props)
    }

  val styleSheet = rule*

  def parse(input: String): ParseResult[List[Rule]] =
    parseAll(styleSheet, input)

  def parse(input: java.io.InputStream): ParseResult[List[Rule]] =
    parse(new java.io.InputStreamReader(input))

  def parse(input: java.io.Reader): ParseResult[List[Rule]] =
    parseAll(styleSheet, input)
}
