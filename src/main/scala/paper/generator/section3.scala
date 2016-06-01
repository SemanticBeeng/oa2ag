package oa2ag.paper
package generator

/**
 * Section 3 - Encoding Synthesized Attributes (Generated)
 * =======================================================
 * This file is analogous to the files `section3.scala`. It should be read as
 * translation from the manual encoding to the usage of the generated code. It
 * is recommended to compare the two files in order to gain intuition about the
 * code generator.
 *
 * The generated file is called `Expr.gen.scala` and is located in
 * `src/main/scala/paper/generator`. The description of the algebraic signature
 * serving as input to the generator can be found in
 * `project/CodeToGenerate.scala`.
 *
 * Where present, running the tests should yield the exact same results as
 * the manual encoding developed in section 3.
 *
 * For instance compare the output of evaluating:
 *
 *     paper.section3.subsection1.test
 *
 * and
 *
 *     paper.generator.section3.subsection1.test
 */
package section3

import attributes._
import examples._


/**
 * Section 3.1 - S1-Attributed Grammars
 * ------------------------------------
 */
object subsection1 {

  /**
   * Figure 4. Equations encoded as Algebra
   */
  val Expr_value = new Expr.Algebra[Int] {
    def lit = n => n
    def add = (e1, e2) => e1 + e2
  }

  def test {
    println( threeplusfive(Expr_value) ) // prints 8
  }
}

/**
 * Section 3.2 - S-Attributed Grammars
 * -----------------------------------
 */
object subsection2 {

  /**
   * Figure 5. Composing two algebras to compute two synthesized attributes at once.
   *
   * (a) Two attribute equation-sets
   */
  val Expr_value = new Expr.Algebra[HasValue] {
    def lit = n => HasValue(n)
    def add = (e1, e2) => HasValue(e1.value + e2.value)
  }

  val Expr_pp = new Expr.Algebra[HasPP] {
    def lit = n => HasPP(n.toString)
    def add = (e1, e2) => HasPP(e1.pp + "+" + e2.pp)
  }
}