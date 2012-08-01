package org.geoscript.serialize

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
