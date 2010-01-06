package dwins.geocrunch

trait GeoHash {
  private val characters = "0123456789bcdefghjkmnpqrstuvwxyz"

  def geohash(lat: Double, long: Double, size: Int): String = {
    def mingle[A](x: Stream[A], y: Stream[A]): Stream[A] = {
      Stream.cons(x.head, mingle(y, x.tail))
    }

    def hashBits(x:Double, min: Double, max:Double): Stream[Boolean] = {
      val mid = (min + max) / 2
      if (x >= mid) {
        Stream.cons(true, hashBits(x, mid, max))
      } else {
        Stream.cons(false, hashBits(x, min, mid))
      }
    }

    def hash_text(bits: List[Boolean]): String = {
      val pieces = bits splitAt 5;
      return character(pieces._1) + 
        (if (pieces._2 != Nil) hash_text(pieces._2) else "")
    }

    def character(bits: List[Boolean]):String = {
      val index = bits.foldLeft(0) { (x: Int, b: Boolean) =>
        2*x + (if (b) 1 else 0)
      }

      characters.substring(index, index + 1)
    }

    val bits = mingle(hashBits(long, -180, 180), hashBits(lat, -90, 90))
      .take(5 * size).force;

    hash_text(bits)
  }

  def dehash(hash: String): Pair[Double, Double] = {
    def demingle[A](list:Stream[A]): (Stream[A], Stream[A]) = {
      if (list isEmpty) {
        (Stream.empty, Stream.empty) 
      } else {
        val xs = demingle(list drop 1)
        (Stream.cons(list.first, xs._2), xs._1)
      }
    }
    
    def dehashBits(bits: Stream[Boolean], min: Double, max: Double): Double = {
      val mid = (min + max) / 2
      if (bits isEmpty) {
        max
      } else if (bits.head) {
        dehashBits(bits.tail, mid, max)
      } else {
        dehashBits(bits.tail, min, mid)
      }
    }

    val bits = hash.flatMap ((x: Char) => {
      val bitString = characters.indexOf(x).toBinaryString
      ("00000".substring(0, 5 - bitString.length) + bitString).map('1' ==)
    })

    val streams = demingle(bits.toStream)

    (dehashBits(streams._2, -90, 90), dehashBits(streams._1, -180, 180))
  }
} 
