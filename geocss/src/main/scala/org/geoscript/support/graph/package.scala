package org.geoscript.support

package object graph {
  def isClique[V](vs: Set[V], connected: (V, V) => Boolean): Boolean =
    vs.toSeq.combinations(2).forall { case Seq(a, b) => connected(a, b) }

  def maximalCliques[V]
    (vertices: Set[V], connected: (V, V) => Boolean)
    : Set[Set[V]]
  = {
    // leveraging the fact that Map[A,B] <: (A => B) here to pre-compute
    // neighborhoods
    val neighbors: V => Set[V] = vertices
      .map { v => (v, (vertices - v).filter(connected(_, v))) }
      .toMap

    def degree(v: V) = vertices.count(connected(_, v))

    def recurse
      (r: Set[V], p: Set[V], x: Set[V], accum: Set[Set[V]])
      : Set[Set[V]]
    = {
      if (p.isEmpty && x.isEmpty)
        accum + r
      else {
        val pivot = (p ++ x).head
        val pruned = (p -- neighbors(pivot)).toSeq
        split(p, pruned, x).foldLeft(accum) {
          case (accum, (v, p1, x1)) =>
            recurse(
              r + v,
              p1 & neighbors(v),
              x1 & neighbors(v),
              accum
            )
        }
      }
    }

    def split
      (vs: Set[V], pruned: Seq[V], x: Set[V])
      : Seq[(V, Set[V], Set[V])] 
    = {
      val splits = 
        ((pruned scanLeft (vs.toSet, x)) {
          case ((p, x), v) => (p - v, x + v)
        })

      (pruned zip splits).map { case (a, (b, c)) => (a, b, c) }
    }

    val sorted = vertices.toSeq.sortBy(degree)
    split(vertices, sorted, Set.empty).foldLeft(Set.empty[Set[V]]) {
      case (accum, (v, p, x)) =>
        recurse(Set(v), p & neighbors(v), x & neighbors(v), accum)
    }
  }

  // Combinations of vertices containing at most one member of each clique
  def enumerateCombinations[V](cliques: Set[Set[V]]): Set[Set[V]] = {
    def recurse
      (results: Set[Set[V]], accum: Set[V], cliques: Seq[Set[V]])
      : Set[Set[V]]
    = {
      if (cliques isEmpty)
        results + accum
      else {
        val clique = cliques.head
        val tail = cliques.tail

        (clique foldLeft recurse(results, accum, tail)) { (results, v) =>
          recurse(results, accum + v, tail.filterNot(_ contains v))
        }
      }
    }

    // largest cliques first for moar speedz!!
    val sorted = cliques.toSeq.sortBy(- _.size)

    recurse(Set.empty, Set.empty, sorted)
  }
}
