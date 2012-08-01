package org.geoscript.serialize

trait Encodable[Spec, Out] {
  def encode(spec: Spec)(op: java.io.OutputStream => Unit): Out
}

object Encodable {
  implicit object encodeOutputStream extends Encodable[java.io.OutputStream, Unit] {
    def encode(spec: java.io.OutputStream)(op: java.io.OutputStream => Unit): Unit = op(spec)
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
