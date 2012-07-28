package org.geoscript.support.logic

object LogicDemo extends App {
  import symbolic._

  val kb = Knowledge.Oblivion[Sentence].given('A)

  def test(p: Sentence) = println(kb.reduce(p))

  test('A)
  test('B)
  test(Not('A))
  test(And('A, 'B))
  test(Or('A, 'B))
}
