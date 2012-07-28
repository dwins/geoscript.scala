package org.geoscript.support.interval

sealed trait Interval[T] {
  def contains(v: T): Boolean 
}

object Interval {
  import Cap.{ join, satisfies }

  private val empty: Interval[Any] = 
    new Interval[Any] {
      def contains(v: Any) = false
      override val toString = "{∅}"
    }

  def Empty[T] = empty.asInstanceOf[Interval[T]]

  def Full[T : Ordering] = NonEmpty(None, None)

  object Degenerate {
    def apply[T : Ordering](v: T): Interval[T] =
      NonEmpty(Some(Closed(v)), Some(Closed(v)))
    def unapply[T](i: Interval[T]): Option[T] =
      Some(i) collect {
        case NonEmpty(Some(Closed(v)), Some(Closed(v0))) if v == v0 => v
      }
  }

  def left[T : Ordering](c: Cap[T]): Interval[T] = NonEmpty(Some(c), None)

  def right[T : Ordering](c: Cap[T]): Interval[T] = NonEmpty(None, Some(c))
  
  def degenerate[T : Ordering](v: T): Interval[T] = 
    NonEmpty(Some(Closed(v)), Some(Closed(v)))

  def finite[T : Ordering](min: Cap[T], max: Cap[T]): Interval[T] =
    intersection[T](
      left(min),
      right(max)
    )

  def intersection[T](a: Interval[T], b: Interval[T])(implicit evidence: Ordering[T])
  : Interval[T] =
    (a, b) match {
      case (`empty`, _) | (_, `empty`) => Empty
      case (a: NonEmpty[T], b: NonEmpty[T]) => 
        val min = 
          (for (x <- a.min; y <- b.min) yield join(evidence.gt _)(x, y)) orElse
          a.min orElse
          b.min
        val max =
          (for (x <- a.max; y <- b.max) yield join(evidence.lt _)(x, y)) orElse
          a.max orElse
          b.max
        val emptyOpt =
          for (mn <- min; mx <- max) yield
            (mn, mx) match {
              case (Closed(_), Closed(_)) => evidence.gt(mn.v, mx.v)
              case _ => evidence.gteq(mn.v, mx.v)
            }
        val empty = emptyOpt.getOrElse(false)
        if (empty)
          Empty
        else
          NonEmpty(min, max)
    }

  case class NonEmpty[T](min: Option[Cap[T]], max: Option[Cap[T]])(implicit evidence: Ordering[T]) extends Interval[T] {
    def contains(u: T) =
      min.forall(v => satisfies(evidence.lt _)(v, u)) &&
      max.forall(v => satisfies(evidence.gt _)(v, u))

    override def toString = {
      val left = 
        min match {
          case Some(Closed(v)) => "[" + v
          case Some(Open(v)) => "(" + v
          case None => "[-∞"
        }
      val right =
        max match {
          case Some(Closed(v)) => v + "]" case Some(Open(v)) => v + ")"
          case None => "+∞]"
        }

      left + ", " + right
    }
  }
}
