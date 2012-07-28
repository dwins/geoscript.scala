package org.geoscript.support.logic

trait Satisfiability
case object Always extends Satisfiability
case object Sometimes extends Satisfiability
case object Never extends Satisfiability
