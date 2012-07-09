package tutorial

object Layers extends App {
  import org.geoscript._, layer._
  val lyr = Layer("foo", Seq(feature.bind[String]("text")))
  lyr += feature.fromAttributes(("text", "hello"))
  lyr.withAll { features =>
    for (feature <- features) {
      println(feature.get[String]("text"))
    }
  }
}
