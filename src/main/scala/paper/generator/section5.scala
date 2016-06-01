package oa2ag
package paper
package generator

/**
 * Section 5 - Encoding Inherited Attributes (Generated)
 * =====================================================
 * Differences to the manual encoding presented in the paper:
 *
 *   The version of context transformers (inherited attributes) in this
 *   subsection represent intermediate steps. As such they do not find
 *   immediate correspondence in the generated code. However, ignoring
 *   functionality introduced in later sections the simple version of
 *   context transformers can be encoded.
 */
package section5

import attributes._
import examples._

/**
 * Section 5.1 - I-Attributed Grammars
 * -----------------------------------
 */
object subsection1 {

  /**
   * `InhAlg1` as well as `CtxInhAlg1` are intermediate steps and are not
   * generated. Hence there is no difference to `section2.subsection4`.
   *
   * The same holds for the development of "Assemble (2)". Skipping ahead we can
   * use the generated trait for the final shape of inherited attributes, but
   * ignore the possibility to access children left to the current node.
   *
   * Thus we have
   *
   *     type CtxInhAlg[E] = Expr.Inh[Any, E, E]
   */

  val Expr_indent = new Expr.Inh[Any, HasIndent, HasIndent]{
    def add_lhs = ctx => HasIndent(ctx.indent)
    def add_rhs = _ => ctx => HasIndent(ctx.indent + " ")
  }

  val Expr_right = new Expr.Inh[Any, HasRight, HasRight] {
    def add_lhs = ctx => HasRight(false)
    def add_rhs = _ => ctx => HasRight(true)
  }

  val Expr_pp = new Expr.Syn[HasPP, HasRight, HasPP] {
    def lit = n => ctx => HasPP(n.toString)
    def add = (e1, e2) => ctx =>
      if (ctx.right)
        HasPP("(" + e1.pp + "+" + e2.pp + ")")
      else
        HasPP(e1.pp + "+" + e2.pp)
  }

  // This is the same as calling `Expr.apply(right, pp)`
  val assembled = Expr(Expr_right, Expr_pp)
  val init = HasRight(false)

  def test {
    println( threepluszeroplusfive(assembled)(init).pp ) // prints 3+5+0
    println( threeplusfourplusfive(assembled)(init).pp ) // prints 3+(4+5)
  }
}


/**
 * Section 5.2 - L-Attributed Grammars
 * -----------------------------------
 * Differences to the manual encoding presented in the paper:
 *
 *   The trait `PreInhAlg`, as well as the trait for final assembly are both
 *   generated. Composing the synthesized attributes and assembling them
 *   with the corresponding inherited attribute amounts to
 *
 *       Expr(pos, pp <+ count)
 *
 * The trait `PreInhAlg` is generated and called `InhExprSig` since it is the
 * derived signature for inherited attributes.
 */
object subsection2 {

   val Expr_pos = new Expr.Inh[HasCount, HasPos, HasPos] {
    def add_lhs = ctx => HasPos(ctx.pos + 1)
    def add_rhs = e1 => ctx => HasPos(ctx.pos + e1.count + 1)
  }

  val Expr_count = new Expr.Syn[HasCount, Any, HasCount] {
    def lit = n => ctx => HasCount(1)
    def add = (e1, e2) => ctx => HasCount(e1.count + e2.count + 1)
  }

  val Expr_pp = new Expr.Syn[HasPP, HasPos, HasPP] {
    def lit = n => ctx =>
      HasPP("(" + n + "){" + ctx.pos + "}")
    def add = (e1, e2) => ctx =>
      HasPP("(" + e1.pp + "+" + e2.pp + "){" + ctx.pos + "}")
  }

  // Using the binary operator and the assemble method the assembly can be
  // expressed very concisely.
  val assembled = Expr(Expr_pos, Expr_pp <+ Expr_count)

  def test {
    println( threeplusfourplusfivepluszero(assembled)(HasPos(0)).pp )
    // prints (((3){2}+(4){3}){1}+((5){5}+(0){6}){4}){0}
  }
}

/**
 * Section 5.3 - Three ways to construct sentences
 * -----------------------------------------------
 */
object subsection3 {

  import paper.section4.subsection1.mix
  import subsection2.{assembled, Expr_pos, Expr_count, Expr_pp }
  import Expr.Syntax._

  val threeplusfiveTree = Add(Lit(3), Lit(5))

  /**
   * The partial evaluation of `ExprAssemble` still has to be performed by hand
   */
  def threeplusfive2[Ctx, Out]
      (alg1: Expr.Inh[Ctx with Out, Ctx, Ctx],
       alg2: Expr.Syn[Ctx with Out, Ctx, Out])
        (implicit comp: Compose[Ctx, Out]): Ctx => Ctx with Out =
    ctx => {
      val in3 = alg1.add_lhs(ctx)
      val out3 = alg2.lit(3)(in3)
      val all3 = mix[Ctx, Out](in3, out3)

      val in5 = alg1.add_rhs(all3)(ctx)
      val out5 = alg2.lit(5)(in5)
      val all5 = mix[Ctx, Out](in5, out5)

      mix[Ctx, Out](ctx, alg2.add(all3, all5)(ctx))
    }

  def test {

    val init = HasPos(0)

    // 1. passing the algebra to church encoded sentences
    println("1: " + threeplusfive(assembled)(init).pp)

    // 2. folding over a tree using the algebra
    println("2: " + assembled(threeplusfiveTree)(init).pp )
    //                        ^^^^^^^^^^^^^^^^^
    // In the generated code the method `apply` on an algebra allows folding the
    // algebra over a given syntax tree.

    // 3. partially evaluating `assemble` with three plus five
    val threeplusfiveAlg = threeplusfive2(Expr_pos, Expr_pp <+ Expr_count)

    println("3: " + threeplusfiveAlg(init).pp)
  }
}