package org.geoscript.serialize

trait Readable[T] {
  def read[U](t: T)(op: java.io.Reader => U): U
}

object Readable {
  implicit object readReader extends Readable[java.io.Reader] {
    def read[T](in: java.io.Reader)(f: java.io.Reader => T): T = f(in)
  }

  implicit object readFile extends Readable[java.io.File] {
    def read[T](file: java.io.File)(f: java.io.Reader => T): T = {
      val in = new java.io.BufferedReader(new java.io.FileReader(file))
      try
        f(in)
      finally
        in.close()
    }
  }

  implicit object readString extends Readable[String] {
    def read[T](s: String)(f: java.io.Reader => T): T = {
      val in = new java.io.StringReader(s)
      try
        f(in)
      finally
        in.close()
    }
  }
}
