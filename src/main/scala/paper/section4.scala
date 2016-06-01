package oa2ag
package paper

/**
 * Section 4 - Modularity of the Encoding
 * ======================================
 */
package section4

import attributes._

/**
 * Section 4.1 - Accessing other attributes of children
 * ----------------------------------------------------
 */
object subsection1 {

  /**
   * Figure 6. Modularizing attribute definitions.
   *
   * (a) Algebraic signature
   */
  type ExprSig[E] = PreExprSig[E, E]

  trait PreExprSig[-E, +Out] {
    def Lit: Int => Out
    def Add: (E, E) => Out
  }

  /**
   * (b) Composition of pre-algebras
   */
  trait ExprCompose[I1, O1, I2, O2]
      extends PreExprSig[I1 with I2, O1 with O2] {

    val alg1: PreExprSig[I1, O1]
    val alg2: PreExprSig[I2, O2]

    // This implementation detail is hidden in the paper
    implicit val comp: Compose[O1, O2]

    def Lit = n => {
      val out1 = alg1.Lit(n)
      val out2 = alg2.Lit(n)
      mix[O1, O2](out1, out2)
    }

    def Add = (e1, e2) => {
      val out1 = alg1.Add(e1, e2)
      val out2 = alg2.Add(e1, e2)
      mix[O1, O2](out1, out2)
    }
  }

  // Examples

  val Expr_ppOpt = new PreExprSig[HasPP with HasValue, HasPP] {
    def Lit = n => HasPP(n.toString)
    def Add = (e1, e2) =>
      if (e2.value == 0) e1 else
        HasPP(e1.pp + "+" + e2.pp)
  }

  // `mix` is implemented in terms of our macro for delegation based mixin
  // composition
  def mix[A, B](a: A, b: B)(implicit c: Compose[A, B]): A with B = c(a, b)

  // Another attribute to composed the optimized pretty printer with.
  // We redefine Expr_eval here (already defined in section3.scala) since
  // ExprSig has been redefined.
  val Expr_eval = new ExprSig[HasValue] {
    def Lit = n => HasValue(n)
    def Add = (e1, e2) => HasValue(e1.value + e2.value)
  }

  val ppOptimizedWithEval: ExprSig[HasPP with HasValue] = new ExprCompose[
      HasValue, HasValue,
      HasPP with HasValue, HasPP] {

    // We ask the compose macro to implement some form of delegation based
    // mixin composition for this particular instance `HasValue with HasPP`.
    // This technical detail is omitted in the paper and, as we will see in
    // section2-generated, can be hidden from the user interface.
    implicit val comp = Compose.compose[HasValue, HasPP]

    val alg1 = Expr_eval
    val alg2 = Expr_ppOpt
  }

  // Example program: (3 + 0) + 5
  def threepluszeroplusfive[E](alg: ExprSig[E]) =
    alg.Add(alg.Add(alg.Lit(3), alg.Lit(0)), alg.Lit(5))

  def test {
    println( threepluszeroplusfive(ppOptimizedWithEval).pp ) // prints 3+5
    println( threepluszeroplusfive(ppOptimizedWithEval).value ) // prints 8
  }
}


/**
 * Section 4.2 - Accessing other attributes on the current node
 * ------------------------------------------------------------
 */
object subsection2 {

  import subsection1.{ mix, PreExprSig, ExprSig }

  /**
   * Figure 7. Depending on information about the same node.
   *
   * (a) Algebraic signature.
   *
   * Taking the context into account
   */
  type CtxExprSig[-E, -Ctx, +Out] = PreExprSig[E, Ctx => Out] // = Expr.Syn[E, Ctx, Out]

  /**
   * (b) Composition of pre-algebras
   */
  trait ExprCompose[
      E1, C1, O1, E2, C2 >: C1 with O1, O2]
    extends CtxExprSig[E1 with E2, C1, O1 with O2] {

    val alg1: CtxExprSig[E1, C1, O1]
    val alg2: CtxExprSig[E2, C2, O2]

    // Again, this implementation detail is hidden in the paper
    implicit val comp1: Compose[C1, O1]
    implicit val comp2: Compose[O1, O2]

    def Lit = n => ctx => {
      val out1 = alg1.Lit(n)(ctx)
      val out2 = alg2.Lit(n)(mix[C1, O1](ctx, out1))
      mix[O1, O2](out1, out2)
    }

    def Add = (e1, e2) => ctx => {
      val out1 = alg1.Add(e1, e2)(ctx)
      val out2 = alg2.Add(e1, e2)(mix[C1, O1](ctx, out1))
      mix[O1, O2](out1, out2)
    }
  }

  /**
   * (c) Assembly of pre-algebras
   *
   * We cannot directly use CtxExprSig[E, Ctx, O] = PreExprSig[E,
   * Ctx => O] for folding. In order to prepare it for folding,
   * we have to add some logic that passes through the static
   * context `ctx`.
   *
   * Here, the context is just passed through unchanged. In
   * following subsections, we add context transformers to modify
   * the context and implement the equations for inherited
   * attributes.
   */
  trait ExprAssemble[Ctx, Out] extends ExprSig[Ctx => Out] {

    val alg1: CtxExprSig[Out, Ctx, Out]

    def Lit = n => ctx =>
      alg1.Lit(n)(ctx)

    def Add = (e1, e2) => ctx => {
      val outL = e1(ctx)
      val outR = e2(ctx)
      alg1.Add(outL, outR)(ctx)
    }
  }

  // Examples

  val Expr_ppOpt2 = new CtxExprSig[HasPP, HasValue, HasPP] {
    def Lit = n => ctx => HasPP(n.toString)
    def Add = (e1, e2) => ctx =>
      if (ctx.value == 0) HasPP("0") else
        HasPP(e1.pp + "+" + e2.pp)
  }

  // eval as a pre-function-algebra (just ignoring the context)
  val Expr_eval = new CtxExprSig[HasValue, Any, HasValue] {
    def Lit = n => ctx => HasValue(n)
    def Add = (e1, e2) => ctx => HasValue(e1.value + e2.value)
  }

  // compose the two
  val Expr_ppOpt2Eval = new ExprCompose[
      HasValue, Any, HasValue,
      HasPP, HasValue, HasPP] {

    implicit val comp1 = Compose.anyL[HasValue]
    implicit val comp2 = Compose.compose[HasValue, HasPP]

    val alg1 = Expr_eval
    val alg2 = Expr_ppOpt2
  }

  val assembled = new ExprAssemble[Any, HasPP with HasValue] {
    val alg1 = Expr_ppOpt2Eval
  }

  // Example program: (0 + 0) + 5
  def zeropluszeroplusfive[E](alg: ExprSig[E]) =
    alg.Add(alg.Add(alg.Lit(0), alg.Lit(0)), alg.Lit(5))

  def test {
    // We can pass `null` since we know that the finally assembled
    // `ppOptimized2WithEval` is not making use of its context of type `Any`
    println( zeropluszeroplusfive(assembled)(null).pp ) // prints 0+5
  }
}