package tutorial

object Layers extends App {
  import org.geoscript._, feature._, layer._
  val lyr = Layer(Schema("foo", "text".binds[String]))
  lyr += fromAttributes(("text", "hello"))

  for (feature <- lyr) 
    println(feature.get[String]("text"))
}
