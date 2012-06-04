package org.geoscript
package serialize

import java.io.{ File, InputStream, OutputStream }

trait Reader[+T] {
  def read(source: Source): T
}

trait Writer[-T] {
  def write[A](t: T, sink: Sink[A]): A
}

trait Format[T] extends Reader[T] with Writer[T]

trait Sink[T] {
  def apply(op: OutputStream => Unit): T
}

object Sink {
  implicit def stream(out: OutputStream): Sink[Unit] =
    new Sink[Unit] { 
      def apply(op: OutputStream => Unit) = op(out)
    }

  implicit def file(name: String): Sink[File] =
    file(new java.io.File(name))

  implicit def file(file: File): Sink[File] =
    new Sink[java.io.File] { 
      def apply(op: OutputStream => Unit) = {
        val output =
          new java.io.BufferedOutputStream(new java.io.FileOutputStream(file))
        stream(output)(op)
        output.close()

        file
      }
    }

  def string: Sink[String] =
    new Sink[String] { // TODO: Needs better text support
      def apply(op: OutputStream => Unit) = 
        buffer(op).view.map(_.toChar).mkString
    }

  def buffer: Sink[Array[Byte]] =
    new Sink[Array[Byte]] {
      def apply(op: OutputStream => Unit) = {
        val output = new java.io.ByteArrayOutputStream
        stream(output)(op)
        output.close()
        output.toByteArray
      }
    }
}

trait Source {
  def apply[T](op: InputStream => T): T
}

object Source {
  implicit def stream(in: InputStream): Source =
    new Source { 
      def apply[T](op: InputStream => T): T = op(in)
    }

  implicit def file(name: String): Source =
    file(new java.io.File(name))

  implicit def file(file: File): Source =
    new Source { 
      def apply[T](op: InputStream => T): T = {
        val input =
          new java.io.BufferedInputStream(new java.io.FileInputStream(file))
        val res = stream(input)(op)
        input.close()

        res
      }
    }

  def string(data: String): Source =
    new Source {
      def apply[T](op: InputStream => T): T = {
        val input = new java.io.ByteArrayInputStream(data.getBytes())
        val res = stream(input)(op)
        input.close()
        res
      }
    }

  def buffer(data: Array[Byte]): Source =
    new Source {
      def apply[T](op: InputStream => T): T = {
        val input = new java.io.ByteArrayInputStream(data)
        val res = stream(input)(op)
        input.close()

        res
      }
    }
}
