package tutorial

object Layers extends App {
  import org.geoscript._, layer._
  val lyr = Layer("foo", Seq(feature.bind[String]("text")))
  lyr += feature.fromAttributes(("text", "hello"))

  for (feature <- lyr) 
    println(feature.get[String]("text"))
}
