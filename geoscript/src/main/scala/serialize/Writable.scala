package org.geoscript.serialize

trait Writable[Spec, Out] {
  def write(spec: Spec)(op: java.io.Writer => Unit): Out
}

object Writable {
  implicit object writeWriter extends Writable[java.io.Writer, Unit] {
    def write(spec: java.io.Writer)(op: java.io.Writer => Unit): Unit = op(spec)
  }

  implicit object writeFile extends Writable[java.io.File, Unit] {
    def write(spec: java.io.File)(op: java.io.Writer => Unit): Unit = {
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
