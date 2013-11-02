package org.geoscript.geocss

import CssOps.Specificity

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
  type Warn[T] = (Seq[Warning], T)
  type RuleConstraint = Either[Selector, Context]

  override val whiteSpace: scala.util.matching.Regex = """(?s)(?:\s|/\*.*?\*/)+""".r

  private val expressionPartial =
    new PartialFunction[String,Expression] {
      def apply(s: String): Expression = Expression(s)
      def isDefinedAt(exp: String): Boolean = 
        try {
          ECQL.toExpression(exp); true
        } catch {
          case (_: org.geotools.filter.text.cql2.CQLException) => false
        }
    }

  private val expressionSelectorPartial: PartialFunction[String, Selector] =
    new PartialFunction[String, Selector] {
      def apply(exp: String): Selector = 
        Selector.asSelector(ECQL.toFilter(exp)) 

      def isDefinedAt(exp: String): Boolean = 
        try {
          ECQL.toFilter(exp)
          true
        } catch {
          case (_: org.geotools.filter.text.cql2.CQLException) => false
        }
    }

  private val SingleComment: Parser[String] = {
    val whitespacepadding = rep(elem("Whitespace", _.isWhitespace))
    val startComment = elem('/') ~ elem('*')
    val endComment = elem('*') ~ elem('/')
    val commentChar = (elem('*') <~ not(elem('/'))) | elem("Comment content", _ != '*')
    for {
      _ <- whitespacepadding
      _ <- startComment
      body <- rep(commentChar)
      _ <- endComment
    } yield body.mkString
  }

  private val ParsedComment: Parser[Description] = rep1(SingleComment) map { x => Description(x.last) }

  private val identifier: Parser[String] = """[a-zA-Z]([a-zA-Z0-9]|[-_][a-zA-Z0-9])*""".r
  private val fid: Parser[String] = """[a-zA-Z]([a-zA-Z0-9]|[-._][a-zA-Z0-9])*""".r

  val number: Parser[String] = """-?[0-9]*([0-9]\.|\.[0-9]|[0-9])[0-9]*""".r
  private val measure: Parser[String] = """([0-9]*\.[0-9]+|[0-9]+)[a-zA-z]+""".r
  val percentage: Parser[String] = """-?([0-9]*\.[0-9]+|[0-9]+)%""".r
  private val string: Parser[String] = (
    (("\"" + """([^"]|\\"])*""" + "\"").r) | 
    (("'"  + """([^']|\\'])*""" + "'" ).r)
  ) map {s => s.substring(1, s.length - 1)}

  private val color: Parser[String] = """#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})""".r
  private val literal: Parser[String] = percentage|measure|number|string|color

  val spaces: Parser[_] = rep(elem("Whitespace", _.isWhitespace))

  private val embeddedCQL: Parser[String] = { 
    val doubleQuotedString = 
      for { 
        _    <- elem('"')
        body <- rep(elem("String literal", _ != '"'))
        -    <- elem('"')
      } yield body.mkString("\"", "", "\"")

    val singleQuotedString =
      for {
        _    <- elem('\'')
        body <- rep(elem("String literal", _ != '\''))
        _    <- elem('\'')
      } yield body.mkString("'", "", "'")

    for {
      _ <- spaces ~ elem('[')
      body <- rep(doubleQuotedString |
                  singleQuotedString |
                  elem("CQL Expression", _ != ']'))
      _ <- elem(']')
    } yield body.mkString("")
  }
    
  private val expressionSelector: Parser[Selector] =
    embeddedCQL ^? expressionSelectorPartial

  val pseudoSelector: Parser[PseudoSelector] =
    for {
      _ <- literal("[@")
      id <- identifier
      op <- "[><=]".r
      num <- number
      _ <- literal("]")
    } yield PseudoSelector(id, op, num.toDouble)

  val pseudoClass: Parser[PseudoClass] =
    (":" ~> identifier) ^^ PseudoClass

  val parameterizedPseudoClass: Parser[ParameterizedPseudoClass] =
    for {
      _ <- literal(":")
      id <- identifier
      _ <- literal("(")
      num <- number
      _ <- literal(")")
    } yield ParameterizedPseudoClass(id, num)

  val url: Parser[String] =
    "url(" ~> """[\.!#$%&*-~:/\p{Alnum}]+""".r <~ ")"
  val function: Parser[String ~ Seq[Value]] =
    (identifier <~ "(") ~ (repsep(value, ",") <~ ")")

  private def stripBraces(expr: String): String =
    """^\[|\]$""".r.replaceAllIn(expr, "")

  // property names
  // * start with a hyphen or an alphabetic character
  // * if they start with a hyphen, it is followed by an alphabetic character
  // * then follow 0 or more hyphens, underscores, or alphanumeric characters
  val propname: Parser[String] = """-?[a-zA-Z][_a-zA-Z0-9\-]*""".r

  val value: Parser[Value] =
    (url ^^ { x => Function("url", Seq(Literal(x))) }) | 
    (function ^^ { case name ~ args => Function(name, args) }) |
    ((identifier|literal) ^^ Literal) |
    (embeddedCQL ^? expressionPartial)

  private val singleDefinition: Parser[Seq[Value]] = rep(value)

  private val multipleDefinition: Parser[Seq[Seq[Value]]] = rep1sep(singleDefinition, ",")

  private val property: Parser[Property] =
    for {
      id   <- propname
      _    <- literal(":")
      defs <- multipleDefinition
    } yield Property(id, defs)

  private val propertyList: Parser[Seq[Property]] =
    for { 
      _ <- literal("{")
      props <- rep1sep(property, ";")
      _ <- opt(";") ~ "}"
    } yield props

  private val idSelector: Parser[Selector] = ("#" ~> fid) map Id

  private val catchAllSelector: Parser[Selector] = literal("*") ^^^ Accept

  private val typeNameSelector: Parser[Selector] = identifier map Typename

  private val basicSelector: Parser[Selector] =
    (catchAllSelector | idSelector | typeNameSelector | pseudoSelector |
     expressionSelector)

  private val pseudoElementSelector: Parser[Context] =
    (parameterizedPseudoClass | pseudoClass)

  private val simpleSelector: Parser[Seq[RuleConstraint]] = 
    rep1((basicSelector ^^ (Left(_))) | (pseudoElementSelector ^^ (Right(_))))

  private val selector: Parser[Seq[Seq[RuleConstraint]]] =
    rep1sep(simpleSelector, ",")

  private val rule: Parser[Seq[Rule]] =
    for { 
      comment <- opt(ParsedComment)
      sel <- selector
      props <- propertyList
    } yield {
      val desc = comment.getOrElse(Description.empty)

      val spec = (_: Seq[RuleConstraint])
         .collect { case Left(sel) => Specificity(sel) }
         .fold(Specificity.Zero) { _ + _ }

      for (s <- sel.groupBy(spec).values.to[Vector]) yield {
        def extractSelector(xs: Seq[RuleConstraint]): Selector =
          And(xs collect { case Left(s) => s })

        def extractContext(xs: Seq[RuleConstraint]): Option[Context] =
          xs.collectFirst { case Right(x) => x }

        val sels =     s map extractSelector
        val contexts = s map extractContext
        Rule(desc, List(Or(sels)), contexts map (Pair(_, props)))
      }
    }

  val styleSheet: Parser[Seq[Rule]] =
    rep(rule) map (_.flatten)

  def parse(input: String): ParseResult[Warn[Seq[Rule]]] =
    parseAll(styleSheet, input).map((Nil, _))

  def parse(input: java.io.InputStream): ParseResult[Warn[Seq[Rule]]] =
    parse(new java.io.InputStreamReader(input))

  def parse(input: java.io.Reader): ParseResult[Warn[Seq[Rule]]] =
    synchronized {
      parseAll(styleSheet, input).map((Nil, _))
    }
}
