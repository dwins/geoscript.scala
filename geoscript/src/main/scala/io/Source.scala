package org.geoscript
package io

import java.io.{ File, InputStream }

trait Source {
  def apply[T](op: InputStream => T): T
}

object Source {
  def stream(in: InputStream): Source =
    new Source { 
      def apply[T](op: InputStream => T): T = op(in)
    }

  def file(name: String): Source =
    file(new java.io.File(name))

  def file(file: File): Source =
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
