package org.geoscript.support.logic

object LogicDemo extends App {
  import symbolic._

  val kb = Knowledge.Oblivion[Sentence].given('A.atom)

  def test(p: Sentence) = println(kb.reduce(p))

  test('A.atom)
  test('B.atom)
  test(Not('A.atom))
  test(And('A.atom, 'B.atom))
  test(Or('A.atom, 'B.atom))
}
