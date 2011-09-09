package org.geoscript
package io

import java.io.{ File, OutputStream }

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
