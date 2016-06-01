package oa2ag
package paper

/**
 * Section 5 - Encoding Inherited Attributes
 * =========================================
 */
package section5

import attributes._
import section4.subsection1.mix

/**
 * Section 5.1 - I-Attributed Grammars
 * -----------------------------------
 */
object subsection1 {

  /**
   * Figure 8. Encoding inherited attributes by transformation of the grammar
   *
   * (a) Algebraic signature
   */
  import section4.subsection2.CtxExprSig
  import section4.subsection1.{ ExprSig, PreExprSig }

  /**
   * (b) Composition of pre-algebras
   */
  import section4.subsection2.ExprCompose

  /**
   * (c) Assembly of pre-algebras and their context transformed counterpart
   */

  // This intermediate step is omitted in the paper. We adapt ExprAssemble to
  // also take cont ext transformers into account.
  trait SimpleExprAssemble[Ctx, Out] extends ExprSig[Ctx => Out] {

    val alg1: CtxExprSig[Out, Ctx, Out]
    val alg2: CtxInhSig[Ctx]

    // For `Lit` no context transformer is applied
    def Lit = n => ctx => alg1.Lit(n)(ctx)

    // please note the similarities to the previous version of ExprAssemble -
    // the difference however is that the context is transformed by alg1.X
    def Add = (e1, e2) => ctx => {
      val outL = (alg2.Add1 andThen e1)(ctx)
      val outR = (alg2.Add2 andThen e2)(ctx)
      alg1.Add(outL, outR)(ctx)
    }
  }

  // This is version of the ExprAssemble in figure 8 of the paper.
  //
  // `SimpleExprAssemble` allows accessing computed context information on the
  // current node. However, it does not enable us to access the context
  // information of the already processed child nodes. That's why we make the
  // following changes:
  trait ExprAssemble[Ctx, Out] extends ExprSig[Ctx => Ctx with Out] {
    //                                                ^^^^^^^^^^^^

    // The signature of alg1 specifies that context information is available
    // on child nodes.
    val alg1: CtxExprSig[Ctx with Out, Ctx, Out]
    //                   ^^^^^^^^^^^^
    val alg2: CtxInhSig[Ctx]

    implicit val comp: Compose[Ctx, Out]

    // We mix together the context and the synthesized result
    def Lit = n => ctx => mix[Ctx, Out](ctx, alg1.Lit(n)(ctx))

    def Add = (l, r) => ctx => {
      val outL = (alg2.Add1 andThen l)(ctx)
      val outR = (alg2.Add2 andThen r)(ctx)
      mix[Ctx, Out](ctx, alg1.Add(outL, outR)(ctx))
    }
  }

  /**
   * (d) Algebraic signature of context decorator
   */
  type CtxInhSig[Ctx] = InhSig[Ctx => Ctx]
  trait InhSig[+Out] {
    def Add1: Out
    def Add2: Out
  }

  // Examples

  val Expr_indent = new CtxInhSig[HasIndent] {
    def Add1 = ctx => HasIndent(ctx.indent)
    def Add2 = ctx => HasIndent(ctx.indent + " ")
  }

  val Expr_right = new CtxInhSig[HasRight] {
    def Add1 = ctx => HasRight(false)
    def Add2 = ctx => HasRight(true)
  }

  val Expr_pp = new CtxExprSig[HasPP, HasRight, HasPP] {
    def Lit = n => ctx => HasPP(n.toString)
    def Add = (e1, e2) => ctx =>
      if (ctx.right)
        HasPP("(" + e1.pp + "+" + e2.pp + ")")
      else
        HasPP(e1.pp + "+" + e2.pp)
  }

  val assembled = new ExprAssemble[HasRight, HasPP] {
    val comp = Compose.compose[HasRight, HasPP]
    val alg1 = Expr_pp
    val alg2 = Expr_right
  }
  val init = HasRight(false)

  import section4.subsection1.threepluszeroplusfive

  // Example program: 3 + (4 + 5)
  def threeplusfourplusfive[E](alg: ExprSig[E]) =
    alg.Add(alg.Lit(3), alg.Add(alg.Lit(4), alg.Lit(5)))

  def test {
    println( threepluszeroplusfive(assembled)(init).pp ) // prints 3+5+0
    println( threeplusfourplusfive(assembled)(init).pp ) // prints 3+(4+5)
  }
}


/**
 * Section 5.2 - L-Attributed Grammars
 * -----------------------------------
 */
object subsection2 {

  /**
   * Figure 10. Encoding of an L-attributed grammar.
   *
   * (a) Algebraic signature
   */
  import section4.subsection2.CtxExprSig
  import section4.subsection1.{ ExprSig, PreExprSig }

  /**
   * (b) Composition of pre-algebras
   */
  import section4.subsection2.ExprCompose

  /**
   * (c) Assembly of pre-algebras and their context transformed counterpart
   *
   * This is an adaption of subsection1.ExprAssemble. The only difference is
   * that here, the context transformers are themselves context-sensitive.
   */
  trait ExprAssemble[Ctx, Out] extends ExprSig[Ctx => Ctx with Out] {
    val alg1: CtxExprSig[Ctx with Out, Ctx, Out]
    val alg2: CtxInhSig[Ctx with Out, Ctx, Ctx]

    implicit val comp: Compose[Ctx, Out]

    def Lit = n => ctx => mix[Ctx, Out](ctx, alg1.Lit(n)(ctx))

    def Add = (e1, e2) => ctx => {
      val outL = (alg2.Add1 andThen e1)(ctx)
      val outR = (alg2.Add2(outL) andThen e2)(ctx)
      mix[Ctx, Out](ctx, alg1.Add(outL, outR)(ctx))
    }
  }

  /**
   * (d) Algebraic signature of context decorators
   */
  type CtxInhSig[-E, -Ctx, +Out] = PreInhSig[E, Ctx => Out]
  type InhSig[E] = PreInhSig[E, E]
  trait PreInhSig[-E, +Out] {
    def Add1: Out
    def Add2: E => Out
  }

  // Examples

  /**
   * Figure 11. Encoding of an L-attributed grammar.
   *
   * Pre-order labeling with position.
   *
   * (a) Grammar
   *
   *    e₀ → n         { Lit }
   *    e₀ → e₁ "+" e₂ { Add }
   *
   * (b) Equations
   *
   *    e₁.pos = e₀ + 1
   *    e₂.pos = e₀ + e₁.count + 1
   *
   * (c) Algebra
   */
  val Expr_pos = new CtxInhSig[HasCount, HasPos, HasPos] {
    def Add1 = ctx => HasPos(ctx.pos + 1)
    def Add2 = e1 => ctx => HasPos(ctx.pos + e1.count + 1)
  }

  val Expr_count = new CtxExprSig[HasCount, Any, HasCount] {
    def Lit = n => ctx => HasCount(1)
    def Add = (e1, e2) => ctx => HasCount(e1.count + e2.count + 1)
  }

  val Expr_pp = new CtxExprSig[HasPP, HasPos, HasPP] {
    def Lit = n => ctx =>
      HasPP("(" + n + "){" + ctx.pos + "}")
    def Add = (e1, e2) => ctx =>
      HasPP("(" + e1.pp + "+" + e2.pp + "){" + ctx.pos + "}")
  }

  val Expr_ppCount = new ExprCompose[
      HasPP, HasPos, HasPP,
      HasCount, HasPos, HasCount] {

    implicit val comp1 = Compose.compose[HasPos, HasPP]
    implicit val comp2 = Compose.compose[HasPP, HasCount]

    val alg1 = Expr_pp
    val alg2 = Expr_count
  }

  val assembled = new ExprAssemble[HasPos, HasCount with HasPP] {
    val alg1 = Expr_ppCount
    val alg2 = Expr_pos
    val comp = Compose.compose[HasPos, HasCount with HasPP]
  }

  def threeplusfourplusfivepluszero[E](alg: ExprSig[E]) =
    alg.Add(alg.Add(alg.Lit(3), alg.Lit(4)), alg.Add(alg.Lit(5), alg.Lit(0)))

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

  import section4.subsection1.{ ExprSig, PreExprSig }
  import section4.subsection2.{ CtxExprSig }
  import section5.subsection2.{ assembled, CtxInhSig, Expr_pos, Expr_ppCount }

  def threeplusfive[E](alg: ExprSig[E]) =
    alg.Add(alg.Lit(3), alg.Lit(5))

  trait Expr {
    def fold[E](alg: ExprSig[E]): E
  }
  case class Lit(n: Int) extends Expr {
    def fold[E](alg: ExprSig[E]) = alg.Lit(n)
  }
  case class Add(l: Expr, r: Expr) extends Expr {
    def fold[E](alg: ExprSig[E]) = alg.Add(l.fold(alg), r.fold(alg))
  }

  val threeplusfiveTree = Add(Lit(3), Lit(5))

  def threeplusfive2[Ctx, Out]
      (alg1: CtxInhSig[Ctx with Out, Ctx, Ctx],
       alg2: CtxExprSig[Ctx with Out, Ctx, Out])
        (implicit comp: Compose[Ctx, Out]): Ctx => Ctx with Out =
    ctx => {
      val in3 = alg1.Add1(ctx)
      val out3 = alg2.Lit(3)(in3)
      val all3 = mix[Ctx, Out](in3, out3)

      val in5 = alg1.Add2(all3)(ctx)
      val out5 = alg2.Lit(5)(in5)
      val all5 = mix[Ctx, Out](in5, out5)

      mix[Ctx, Out](ctx, alg2.Add(all3, all5)(ctx))
    }

  def test {

    val init = HasPos(0)

    // 1. passing the algebra to church encoded sentences
    println("1: " + threeplusfive(assembled)(init).pp)

    // 2. folding over a tree using the algebra
    println("2: " + threeplusfiveTree.fold(assembled)(init).pp )

    // 3. partially evaluating `assemble` with three plus five
    val threeplusfiveAlg = threeplusfive2(Expr_pos, Expr_ppCount)

    // we use an inherited attribute (pos) and a synthesized attribute (ppWithCount)
    println("3: " + threeplusfiveAlg(init).pp)
  }
}