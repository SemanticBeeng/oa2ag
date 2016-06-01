package oa2ag.paper

/**
 * Section 3 - Encoding Synthesized Attributes
 * ===========================================
 * This file contains all code examples from section 3 of our paper:
 *
 *   Rendel, Brachthäuser, Ostermann.
 *   From Object Algebras to Attribute Grammars.
 *   Submitted to OOPSLA 2014.
 *
 * For every subsection of section 3 in the paper, there is one
 * Scala object in this file. Each of these objects consists of:
 *
 *   1. The source code shown in the paper
 *   2. Auxiliary code omitted from the paper
 *   3. Examples showing how to use the specific variant
 *
 * You can execute the examples in this file from the Scala console.
 * For example, to execute the examples from Section 3.2, run:
 *
 *    scala> paper.section3.subsection2.test
 *
 * You can reach the Scala console by entering 'console' at the
 * sbt prompt.
 */
package section3

import attributes._


/**
 * Section 3.1 - S1-Attributed Grammars
 * ------------------------------------
 */
object subsection1 {

  /**
   * Figure 3a. Algebraic signature
   */
  trait ExprSig[E] {
    def Lit: Int => E
    def Add: (E, E) => E
  }

  trait ExprCompose[E1, E2] extends ExprSig[E1 with E2] {
    val alg1: ExprSig[E1]
    val alg2: ExprSig[E2]
  }

  /**
   * Figure 4. Equations encoded as Algebra
   */
  val Expr_value = new ExprSig[Int] {
    def Lit = n => n
    def Add = (e1, e2) => e1 + e2
  }

  /**
   * Example program: 3 + 5
   */
  def threeplusfive[E](alg: ExprSig[E]) =
    alg.Add(alg.Lit(3), alg.Lit(5))


  def test {
    println( threeplusfive(Expr_value) ) // prints 8
  }
}


/**
 * Section 3.2 - S-Attributed Grammars
 * -----------------------------------
 */
object subsection2 {

  import subsection1.{ ExprSig, ExprCompose, threeplusfive }

  /**
   * Figure 5. Composing two algebras to compute two synthesized attributes at once.
   *
   * (a) Two attribute equation-sets
   */
  val Expr_value = new ExprSig[HasValue] {
    def Lit = n => HasValue(n)
    def Add = (e1, e2) => HasValue(e1.value + e2.value)
  }

  val Expr_pp = new ExprSig[HasPP] {
    def Lit = n => HasPP(n.toString)
    def Add = (e1, e2) => HasPP(e1.pp + "+" + e2.pp)
  }

  /**
   * (b) Composition
   */
  trait ComposeValuePP extends ExprCompose[HasValue, HasPP] {
    val alg1: ExprSig[HasValue]
    val alg2: ExprSig[HasPP]

    def Lit = n => new HasValue with HasPP {
      val value = alg1.Lit(n).value
      val pp    = alg2.Lit(n).pp
    }

    def Add = (e1, e2) => new HasValue with HasPP {
      val value = alg1.Add(e1, e2).value
      val pp    = alg2.Add(e1, e2).pp
    }
  }

  def test {
    val both = new ComposeValuePP { val alg1 = Expr_value; val alg2 = Expr_pp }
    println( threeplusfive(both).value ) // prints 8
    println( threeplusfive(both).pp )    // prints 3+5
  }
}
