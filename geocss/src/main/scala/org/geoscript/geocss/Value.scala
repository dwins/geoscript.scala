package org.geoscript.geocss

/**
 * A marker trait for values that can be taken by a styling property.
 */
sealed trait Value

/**
 * A Literal is a value that exhibits no dynamic behavior
 */
case class Literal(body: String) extends Value

/**
 * A Function is a built-in function of CSS such as URL or RGB which denotes a
 * special interpretation of a value.
 */
case class Function(name: String, parameters: Seq[Value]) extends Value

/**
 * An Expression is a CQL query embedded in a CSS styling property.
 */
case class Expression(body: String) extends Value
