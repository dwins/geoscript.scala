package org.geoscript.support.graph

import org.scalacheck._, Arbitrary._, Prop._

object GraphCheck extends Properties("Maximal cliques") {
  val parity = (ps: Set[Int]) => 
    (a: Int, b: Int) => (ps - 0).exists(p => (a + b) % p == 0)

  property("are cliques") =
    forAll { (vs: Set[Int], ps: Set[Int]) =>
      val cliques = maximalCliques(vs, parity(ps))
      cliques.forall(isClique(_, parity(ps)))
    }

  property("are maximal") =
    forAll { (vs: Set[Int], ps: Set[Int]) =>
      val cliques = maximalCliques(vs, parity(ps))
      cliques.forall { c =>
        (vs -- c).forall { v => !isClique(c + v, parity(ps)) }
      }
    }

  property("contain every node") =
    forAll { (vs: Set[Int], ps: Set[Int]) =>
      val cliques = maximalCliques(vs, parity(ps))
      cliques.flatten == vs
    }

  property("are never subsets of each other") =
    forAll { (vs: Set[Int], ps: Set[Int]) =>
      val cliques = maximalCliques(vs, parity(ps))
      cliques.forall { a =>
        (cliques - a).forall { b =>
          !(a subsetOf b) && !(b subsetOf a)
      } }
    }
}
