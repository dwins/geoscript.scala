Introduction to Scala
======================

GeoScript.scala is implemented in Scala, a functional/object hybrid language
with strong facilities for Java interoperability.  In order to use it
effectively, you will need to understand some basics of Scala syntax.  This
guide explains some of the Scala constructs most frequently used in
GeoScript.scala code. 


Variables
---------

Variables in Scala must be declared before they are used.  This is accomplished
using the ``val`` or ``var`` statement::

    val a = "12"
    var b = List(1, 2, 3)

Variables declared with ``val`` can only be assigned once. Variables declared
with ``var`` may be overwritten.


Type Annotations
----------------

All values in Scala have a type associated with them.  In the examples above,
we allowed Scala to "infer" the types based on the values on the other side of
the expression.  However, sometimes Scala is unable to infer types and it is
necessary to provide them explicitly.  We can also specify types where Scala
would normally infer them in order to use a more generic type, or to add an
extra sanity check to code::

    val a: Int = "12" // Fails, since "12" is a String, not an Int
    var b // Fails since we are not providing an initial value from which to infer the type

Blocks
------

In Scala, we can set aside a sub-section of code as a "block" by wrapping it in
curly braces.  This is useful to limit scope.  Blocks automatically return the
result of their final statement::

    val total: Int = {
        val a = 1
        val b = 2
        val c = 3
        a + b + c
    }

    // total is now 6
    // a, b, and c are undeclared outside of the {} pair

Functions
---------

Function definitions in Scala use the ``def`` keyword::

    def makeBetter(s: String): String = {
      "better " + s
    }

    makeBetter("mousetrap") // returns "better mousetrap"

For single-statement function definitions we can also omit the curly-braces
(similar to how statements and blocks are interchangeable)::

    def oneLiner(s: String): Int = s.length

The Scala compiler doesn't infer the types of function arguments.  Function
return types *can* be inferred, but it is generally wise to declare them
anyway.  Scala will not compile code where functions that differ only in their
argument lists have implicit return types.

Objects and Classes
-------------------

Since Scala is an object-oriented language, it deals with objects, or
"instances" of "classes".  For example, the string literal "abc" is an instance
of the String class.  GeoScript provides several classes for you.  In general,
you won't need to define your own classes when using GeoScript.  Aside from
types that can be expressed as literals, you can create objects using the
``new`` keyword.  Many classes also (or instead) provide factories called
"companion objects" that can be used to create instances of those objects
without the ``new``::
    
    class Message(msg: String) {
      def greet() { println(msg) }
    }

    val message = new Message("hello")
    message.greet()

Operators
---------

In Scala, methods on objects can also be called in the style of an infix operator::

    val message = "123"
    message substring 1 // returns "23", the same as message.substring(1)

The reverse is also true::
    
    val message = "abc"
    val expandedMessage = "expanded ".+(message)


Function Objects
----------------

In Scala, functions are first-class objects, and can be passed around like any other value::

    def increase(x: Int) = x + 1

    List(1, 2, 3).map(increase) // returns List(2, 3, 4)

Additionally, there is lightweight syntax for creating anonymous functions::

    List(1, 2, 3).map({ x => x + 1 })

This can even be used with operator syntax to create methods that look like native language constructs::

    List(1, 2, 3) map { x => x + 1 }

Pattern Matching
----------------

Scala also provides a feature called pattern matching.  This is like the ``switch/case`` construct seen in many procedural and object-oriented languages in that it allows comparing a value against many conditions with a distinct response to each::

    val guess = 0

    guess match {
      case 0 => "Known value: zero"
      case _ => "I don't know how to handle that"
    }

    // produces "Known value: zero"

Here we've matched against the literal value ``0``, and the catch-all value ``_``.  We can also use patterns to express type requirements::

    val items = List(1, "a string", false)

    items map { item => 
      item match {
        case i: Int => "Integer!"
        case s: String => "String!"
        case b: Boolean => "Boolean!"
      }
    }

    // produces List("Integer!", "String!", "Boolean!")

An interesting aspect of this syntax is that it *names a variable*.  For example, in the line::

    case i: Int => "Integer!" 
    
a variable named `i` is defined within that case block that provides access to the Integer methods on the item.  (Since the List contains items of different types, the `item` variable is inferred to have the most specific type that can contain any of them.  In this case, that means a type that can contain an Int, a String, or a Boolean.  This type is `Any`, a Scala type that can hold any value. `Any` doesn't have many useful methods, however.)

A third form of pattern uses a Scala feature called "extractors."  Patterns using extractors also define variables that can be used inside of the cases they define.  For an example, let's look at the find() method on the Scala List class.  List.find() accepts a function that inspects list elements, and returns an Option.  An Option is a Scala standard class that represents a possible (but not guaranteed) result of an operation.  More specifically, List.find() gives a None back if the list doesn't contain any element that satisfies the search condition, or Some(item) otherwise::

    val opt = List(1, 2, 3) find { x => x % 2 == 0 }
    
    opt match {
      case Some(item) => println("List contained the even number:" + item)
      case None => println("List did not contain an even number."
    }

Learning More
-------------

These are the basics.  If you are still confused, you can check out some further Scala introductions at these web sites:

* http://scala-lang.org/node/25
* http://www.artima.com/scalazine/articles/steps.html
* http://www.simplyscala.com/ 
