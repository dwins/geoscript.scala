package org.geoscript.serialize

trait Decodable[T] {
  def decode[U](t: T)(op: java.io.InputStream => U): U
}

object Decodable {
  implicit object decodeDecoder extends Decodable[java.io.InputStream] {
    def decode[T](in: java.io.InputStream)(f: java.io.InputStream => T): T = f(in)
  }

  implicit object decodeFile extends Decodable[java.io.File] {
    def decode[T](file: java.io.File)(f: java.io.InputStream => T): T = {
      val in = new java.io.BufferedInputStream(new java.io.FileInputStream(file))
      try
        f(in)
      finally
        in.close()
    }
  }

  implicit object decodeString extends Decodable[Array[Byte]] {
    def decode[T](bs: Array[Byte])(f: java.io.InputStream => T): T = {
      val in = new java.io.ByteArrayInputStream(bs)
      try
        f(in)
      finally
        in.close()
    }
  }
}
