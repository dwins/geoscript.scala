package org.geoscript.serialize


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
