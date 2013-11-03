package org.geoscript.geocss

import CssOps.Specificity

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Reader, StreamReader}

import org.geotools.filter.text.ecql.ECQL
import Writer.sugar

/**
 * A parser for CSS syntax, extended a bit with CQL expressions and filters.
 * parse() convenience methods are defined for java.io.InputStream and
 * java.io.Reader sources.
 *
 * @author David Winslow <cdwinslow@gmail.com>
 */
object CssParser extends RegexParsers {
  type Warn[T] = (List[Warning], T)
  type RuleConstraint = Either[Selector, Context]

  override val whiteSpace: scala.util.matching.Regex = """(?s)(?:\s|/\*.*?\*/)+""".r

  private def positional[T](parser: Parser[T]): Parser[(Position, T)] =
    Parser {
      input => parser(input).map((input.pos, _))
    }

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

  val spaces: Parser[Unit] = rep(elem("Whitespace", _.isWhitespace)) ^^^ {}

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
    
  private val expressionSelector: Parser[Warn[RuleConstraint]] =
    embeddedCQL ^? expressionSelectorPartial ^^ (x => (Nil, Left(x)))

  val pseudoSelector: Parser[Warn[RuleConstraint]] =
    for {
      _ <- literal("[@")
      (idPosition, id) <- positional(identifier)
      op <- "[><=]".r
      num <- number
      _ <- literal("]")
      warnings = if (id == "scale")
                   List(Warning.deprecated(idPosition, "scale", "scale-denominator"))
                 else
                   Nil
    } yield (warnings, Left(PseudoSelector(id, op, num.toDouble)))

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

  private val propertyList: Parser[Warn[Seq[Property]]] =
    for { 
      _ <- literal("{")
      props <- rep1sep(property, ";")
      _ <- opt(";") ~ "}"
    } yield (Nil, props)

  private val idSelector: Parser[Warn[RuleConstraint]] = ("#" ~> fid) ^^ (i => (Nil, Left(Id(i))))

  private val catchAllSelector: Parser[Warn[RuleConstraint]] = literal("*") ^^^ ((Nil, Left(Accept)): Warn[RuleConstraint])

  private val typeNameSelector: Parser[Warn[RuleConstraint]] = identifier ^^ (n => (Nil, Left(Typename(n))))

  private val basicSelector: Parser[Warn[RuleConstraint]] =
    (catchAllSelector | idSelector | typeNameSelector | pseudoSelector |
      expressionSelector)

  private val pseudoElementSelector: Parser[Warn[RuleConstraint]] =
    (parameterizedPseudoClass | pseudoClass) ^^ (x => (Nil, Right(x)))

  private val simpleSelector: Parser[Warn[Seq[RuleConstraint]]] = 
    rep1(basicSelector | pseudoElementSelector).map(x => Writer.sequence(x))

  private val selector: Parser[Warn[Seq[Seq[RuleConstraint]]]] =
    rep1sep(simpleSelector, ",").map((x: Seq[Warn[Seq[RuleConstraint]]]) => Writer.sequence(x))

  private val rule: Parser[Warn[Seq[Rule]]] =
    for { 
      comment <- opt(ParsedComment)
      selW <- selector
      propsW <- propertyList
    } yield {
      for {
        sel <- selW
        props <- propsW
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
    }

  val styleSheet: Parser[Warn[Seq[Rule]]] =
    rep(rule) map (x => Writer.sequence(x).map(_.flatten))

  def parse(input: String): ParseResult[Warn[Seq[Rule]]] =
    parseAll(styleSheet, input)

  def parse(input: java.io.InputStream): ParseResult[Warn[Seq[Rule]]] =
    parse(new java.io.InputStreamReader(input))

  def parse(input: java.io.Reader): ParseResult[Warn[Seq[Rule]]] =
    synchronized {
      parseAll(styleSheet, input)
    }
}
