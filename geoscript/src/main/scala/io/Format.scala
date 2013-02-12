package org.geoscript.io

trait Reader[T] {
  def read(source: Source): T
}

trait Writer[T] {
  def write[U](t: T, sink: Sink[U]): U
}

trait Format[T] extends Writer[T] with Reader[T] 
