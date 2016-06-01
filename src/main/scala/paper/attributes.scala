package oa2ag
package paper

/**
 * The attribute interfaces as used by the different sections of the paper.
 */
object attributes {
  trait HasValue { val value: Int }
  def HasValue(v: Int) = new HasValue { val value = v }

  trait HasPP { val pp: String }
  def HasPP(p: String) = new HasPP { val pp = p }

  trait HasRight { val right: Boolean }
  def HasRight(r: Boolean) = new HasRight { val right = r }

  trait HasPos { val pos: Int }
  def HasPos(p: Int) = new HasPos { val pos = p }

  trait HasCount { val count: Int }
  def HasCount(c: Int) = new HasCount { val count = c }

  trait HasIndent { val indent: String }
  def HasIndent(s: String) = new HasIndent { val indent = s}
}
