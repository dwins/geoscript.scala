package org.geoscript.serialize

trait Encodable[Spec, Out] {
  def encode(spec: Spec)(op: java.io.OutputStream => Unit): Out
}

object Encodable {
  implicit object encodeOutputStream extends Encodable[java.io.OutputStream, Unit] {
    def encode(spec: java.io.OutputStream)(op: java.io.OutputStream => Unit): Unit = op(spec)
  }

  implicit object encodeFile extends Encodable[java.io.File, java.io.File] {
    def encode(spec: java.io.File)(op: java.io.OutputStream => Unit): java.io.File = {
      val out = new java.io.FileOutputStream(spec)
      try {
        op(out)
        spec
      } finally {
        out.close()
      }
    }
  }

  implicit object encodeBytes extends Encodable[Unit, Array[Byte]] {
    def encode(spec: Unit)(op: java.io.OutputStream => Unit): Array[Byte] = {
      val buff = new java.io.ByteArrayOutputStream
      try {
        op(buff)
        buff.toByteArray
      } finally
        buff.close()
    }
  }
}
