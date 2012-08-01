import org.geoscript.support.graph.maximalCliques

object GraphDemo extends App {
  val parity = (a: Int, b: Int) => (a + b) % 2 == 0
  maximalCliques((1 to 100).toSet, parity).foreach(println)
}
