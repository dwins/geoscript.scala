package org.geoscript.serialize

import java.io.File

trait Reader[+T] {
  def read[U : Readable](source: U): T =
    implicitly[Readable[U]].read(source)(readFrom(_))
  def readFrom(source: java.io.Reader): T
}

trait Writer[-T] {
  def format(t: T): String = write(t, ())(Writable.writeString)

  def write[Spec, Out](t: T, spec: Spec)(implicit writable: Writable[Spec, Out]): Out =
    implicitly[Writable[Spec, Out]].write(spec)(writeTo(_, t))

  def writeTo(sink: java.io.Writer, t: T): Unit
}

trait Format[T] extends Reader[T] with Writer[T]

trait Readable[T] {
  def read[U](t: T)(op: java.io.Reader => U): U
}

trait Writable[Spec, Out] {
  def write(spec: Spec)(op: java.io.Writer => Unit): Out
}

object Writable {
  implicit object writeWriter extends Writable[java.io.Writer, Unit] {
    def write(spec: java.io.Writer)(op: java.io.Writer => Unit): Unit = op(spec)
  }

  implicit object writeFile extends Writable[File, Unit] {
    def write(spec: File)(op: java.io.Writer => Unit): Unit = {
      val writer = new java.io.FileWriter(spec)
      try 
        op(writer)
      finally
        writer.close()
    }
  }

  implicit object writeString extends Writable[Unit, String] {
    def write(spec: Unit)(op: java.io.Writer => Unit): String = {
      val writer = new java.io.StringWriter
      try {
        op(writer)
        writer.toString
      } finally {
        writer.close()
      }
    }
  }
}

object Readable {
  implicit object readReader extends Readable[java.io.Reader] {
    def read[T](in: java.io.Reader)(f: java.io.Reader => T): T = f(in)
  }

  implicit object readFile extends Readable[File] {
    def read[T](file: File)(f: java.io.Reader => T): T = {
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

trait Decoder[+T] {
  def decode[U : Decodable](source: U): T =
    implicitly[Decodable[U]].decode(source)(decodeFrom(_))
  def decodeFrom(source: java.io.InputStream): T
}

trait Encoder[-T] {
  def buffer(t: T): Array[Byte] = encode(t, ())(Encodable.encodeBytes)

  def format(t: T): String = new String(buffer(t))

  def encode[Spec, Out](t: T, spec: Spec)(implicit encodable: Encodable[Spec, Out]): Out =
    implicitly[Encodable[Spec, Out]].encode(spec)(encodeTo(_, t))

  def encodeTo(sink: java.io.OutputStream, t: T): Unit
}

trait Codec[T] extends Encoder[T] with Decoder[T]

trait Decodable[T] {
  def decode[U](t: T)(op: java.io.InputStream => U): U
}

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

object Decodable {
  implicit object decodeDecoder extends Decodable[java.io.InputStream] {
    def decode[T](in: java.io.InputStream)(f: java.io.InputStream => T): T = f(in)
  }

  implicit object decodeFile extends Decodable[File] {
    def decode[T](file: File)(f: java.io.InputStream => T): T = {
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
