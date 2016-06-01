package oa2ag
package paper
package generator

/**
 * Section 4 - Modularity of the Encoding (Generated)
 * ==================================================
 */
package section4

import attributes._
import examples._

/**
 * Section 4.1 - Accessing other attributes of children
 * ----------------------------------------------------
 */
object subsection1 {

  /**
   * The type alias from the paper
   *
   *     type ExprAlg[E] = PreExprAlg[E, E]
   *
   * can be found in `Expr.gen.scala` as
   *
   *     type Algebra[Expr] = Signature[Expr, Expr]
   *
   */

  val Expr_ppOpt = new Expr.Signature[HasPP with HasValue, HasPP] {
    def lit = n => HasPP(n.toString)
    def add = (e1, e2) =>
      if (e2.value == 0) e1 else
        HasPP(e1.pp + "+" + e2.pp)
  }

  val Expr_eval = new Expr.Signature[HasValue, HasValue] {
    def lit = n => HasValue(n)
    def add = (e1, e2) => HasValue(e1.value + e2.value)
  }

  /**
   * The composition (`ExprCompose`) for complete algebras is not generated and
   * thus does not differ from the manual implementation.
   */
}

/**
 * Section 4.2 - Accessing other attributes on the current node
 * ------------------------------------------------------------
 * Differences to the manual encoding presented in the paper:
 *
 *   The pre-function-algebra type is automatically generated, a binary operator
 *   for composition of pre-function-algebras (<+) is introduced. The trait
 *   necessary to prepare pre-function-algebras for folding is also generated
 *   and replaced by a simple function call to `Expr.apply`.
 */
object subsection2 {

   /**
   * Since pre-function-algebras model synthesized attributes they are called
   * `Expr.Syn` in the generated code.
   */
  //- type CtxExprAlg[-E, -Ctx, +Out] = PreExprAlg[E, Ctx => Out]

  val Expr_ppOpt2 = new Expr.Syn[HasPP, HasValue, HasPP] {
    def lit = n => ctx => HasPP(n.toString)
    def add = (e1, e2) => ctx =>
      if (ctx.value == 0) HasPP("0") else
        HasPP(e1.pp + "+" + e2.pp)
  }

  val Expr_eval = new Expr.Syn[HasValue, Any, HasValue] {
    def lit = n => ctx => HasValue(n)
    def add = (e1, e2) => ctx => HasValue(e1.value + e2.value)
  }

  /**
   * The composition trait (`ExprCompose`) is called
   * `Expr.Signature.DepProduct` since it allows the second algebra
   * to depend on the first one.
   *
   * To ease working with the object algebra encoding of attribute grammars a
   * binary operator `<+` for attribute composition is introduced.
   * The semantics of this operator are simply given by `section2.ExprCompose`
   * with `alg1 <+ alg2`.
   */
  val Expr_ppOpt2Eval = Expr_eval <+ Expr_ppOpt2

  /**
   * The intermediate step `Assemble (1)` is not generated. It however can be
   * simulated by passing an identity context transformer (`Expr.Inh.Default`)
   * to the final assembly method: `Expr.apply`
   */
  val assembled = Expr.apply(Expr.Inh.Default[Any], Expr_ppOpt2Eval)

  // For the usage nothing has to be changed:
  object test {
    println( zeropluszeroplusfive(assembled)(null).pp ) // prints 0+5
  }
}