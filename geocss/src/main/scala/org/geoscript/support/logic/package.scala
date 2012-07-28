package org.geoscript.support

package object logic {
  def given[P : Sentential](p: P): Knowledge[P] = Knowledge.Oblivion[P].given(p)
  def reduce[P : Sentential](p: P): P = Knowledge.Oblivion[P].reduce(p)
}
