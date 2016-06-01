package oa2ag.paper
package section2
package generator

// macro for allowing dynamic mixin composition
import oa2ag.Compose

/**
 * This file is analogous to `section2.scala` and should be read as translation
 * from the manual encoding to the usage of the generated code. It is
 * recommended to compare the two files in order to gain intuition about the
 * code generator.
 *
 * The generated file is called `Expr.gen.scala` and is located in
 * `src/main/scala/paper/section2`. The description of the algebraic signature
 * serving as input to the generator can be found in
 * `project/CodeToGenerate.scala`.
 *
 * Where present, running the tests should yield the exact same results as
 * the manual encoding in `section2.scala`
 *
 * For instance compare the output of evaluating:
 * 
 *     paper.section2.subsection4.test
 * 
 * and
 *
 *     paper.section2.generator.subsection4.test
 *
 * Inline comments starting with "-" indicate that writing this code is not
 * necessary anymore, since it is being generated.
 */

/**
 * Since we are now always using the same definition of algebras (namely the
 * generated one) all examples can be located in one place
 */
object examples {

  def threepluszeroplusfive[E](alg: Expr.Algebra[E]) = 
    alg.add(alg.add(alg.lit(3), alg.lit(0)), alg.lit(5))

  def threeplusfive[E](alg: Expr.Algebra[E]) = 
    alg.add(alg.lit(3), alg.lit(5))

  def zeropluszeroplusfive[E](alg: Expr.Algebra[E]) = 
    alg.add(alg.add(alg.lit(0), alg.lit(0)), alg.lit(5))

  def threeplusfourplusfive[E](alg: Expr.Algebra[E]) = 
    alg.add(alg.lit(3), alg.add(alg.lit(4), alg.lit(5)))

  def threeplusfourplusfivepluszero[E](alg: Expr.Algebra[E]) = 
    alg.add(alg.add(alg.lit(3), alg.lit(4)), alg.add(alg.lit(5), alg.lit(0)))

}

/**
 * Subsection 2.1 - S1-Attributed Grammars
 * ---------------------------------------
 * Difference to section2.subsection1:
 *   Trait `ExprAlg` is now being generated and called `Expr.Algebra`
 */
object subsection1 {

  import attributes._
  import examples._

  /**
   * Figure 1. Encoding an S1-attribute grammar
   */
  val value = new Expr.Algebra[Int] {
    def lit = n => n
    def add = (e1, e2) => e1 + e2
  }  
  
  object test {
    println( threeplusfive(value) ) // prints 8
  }
}

/**
 * Subsection 2.2 - S-Attributed Grammars
 * --------------------------------------
 * The differences to section2.subsection2 immediately follow from the those
 * of subsection1 above, so we omit them here.
 */


/**
 * Subsection 2.3 - Modularizing attributes
 * ----------------------------------------
 * Differences to section2.subsection3_1: 
 *   The pre-algebra trait `PreExprAlg` is being generated and called 
 *   `Signature`.
 */
object subsection3_1 {

  import attributes._

  val ppOptimized = new Expr.Signature[HasPP with HasValue, HasPP] {
    def lit = n => HasPP(n.toString)
    def add = (e1, e2) =>
      if (e2.value == 0) e1 else 
        HasPP(e1.pp + "+" + e2.pp)
  }

  /**
   * This equation can be found in `Expr.gen.scala` as
   *   
   *     type Algebra[Expr] = Signature[Expr, Expr]
   */
  //- type ExprAlg[E] = PreExprAlg[E, E]


  /**
   * The composition (`ExprCompose`) for this intermediate step is not generated
   * and thus does not differ from the manual implementation.
   */
}

/**
 * Differences to section2.subsection3_2: 
 *   The pre-function-algebra type is automatically generated, a binary operator
 *   for composition of pre-function-algebras is introduced. The trait necessary
 *   to prepare pre-function-algebras for folding is also generated and replaced
 *   by a simple function call to `Expr.apply`.
 */
object subsection3_2 {
  
  import attributes._
  import examples._

  /**
   * Since pre-function-algebras model synthesized attributes they are called
   * `Expr.Syn` in the generated code.
   */
  //- type CtxExprAlg[-E, -Ctx, +Out] = PreExprAlg[E, Ctx => Out]

  val ppOptimized2 = new Expr.Syn[HasPP, HasValue, HasPP] {
    def lit = n => ctx => HasPP(n.toString)
    def add = (e1, e2) => ctx => 
      if (ctx.value == 0) HasPP("0") else 
        HasPP(e1.pp + "+" + e2.pp)
  }

  val eval2 = new Expr.Syn[HasValue, Any, HasValue] {
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
  //- trait ExprCompose[
  //-     E1, C1, O1, E2, C2 >: C1 with O1, O2] 
  //-   extends CtxExprAlg[E1 with E2, C1, O1 with O2] {
  //-   val alg1: CtxExprAlg[E1, C1, O1]
  //-   val alg2: CtxExprAlg[E2, C2, O2]
  //-   ... 
  //- }

  val ppOptimized2WithEval = eval2 <+ ppOptimized2

  /**
   * The intermediate step `Assemble (1)` is not generated. It however can be
   * simulated by passing an identity context transformer (`Expr.Inh.Default`)
   * to the final assembly method: `Expr.apply`
   */
  val assembled = Expr.apply(Expr.Inh.Default[Any], ppOptimized2WithEval)

  //- trait ExprAssemble[Ctx, Out] extends ExprAlg[Ctx => Out] {
  //-   ...
  //- }

  // For the usage nothing has to be changed:
  object test {
    println( zeropluszeroplusfive(assembled)(null).pp ) // prints 0+5
  }
}

/**
 * Section 2.4 - Inherited Attributes
 * ----------------------------------
 * Differences to `section2.subsection4`:
 *   The version of context transformers (inherited attributes) in this
 *   subsection represent intermediate steps. As such they do not find
 *   immediate correspondence in the generated code. However, ignoring 
 *   functionality introduced in later sections the simple version of
 *   context transformers can be encoded.
 */
object subsection4 {

  import attributes._
  import examples._

  /**
   * `InhAlg1` as well as `CtxInhAlg1` are intermediate steps and are not
   * generated. Hence there is no difference to `section2.subsection4`.
   *
   * The same holds for the development of "Assemble (2)". Skipping ahead we can
   * use the generated trait for the final shape of inherited attributes, but 
   * ignore the possibility to access children left to the current node:
   */
  type CtxInhAlg1[E] = Expr.Inh[Any, E, E] 

  val right = new CtxInhAlg1[HasRight] {
    def add_lhs = ctx => HasRight(false)
    def add_rhs = _ => ctx => HasRight(true)
  }


  /**
   * Assemble (3)
   * ------------
   * The final shape of assembly (`ExprAssemble`) maps to the generated trait
   * `Expr.Signature.DepProduct` already mentioned in `subsection3_2`. There an
   * identity context transformer has been used to simulate the intermediate
   * step. `ExprAssemble` generalizes the assembly of synthesized attributes and
   * context transformers to yield an L-attributed attribute grammar.
   */
  //- trait ExprAssemble[Ctx, Out] extends ExprAlg[Ctx => Ctx with Out] {
  //-   val alg1: CtxInhAlg1[Ctx]
  //-   val alg2: CtxExprAlg[Ctx with Out, Ctx, Out]
  //-   ...
  //- }


  /**
   * Examples
   */
  val pp = new Expr.Syn[HasPP, HasRight, HasPP] {
    def lit = n => ctx => HasPP(n.toString)
    def add = (e1, e2) => ctx => 
      if (ctx.right)
        HasPP("(" + e1.pp + "+" + e2.pp + ")")
      else
        HasPP(e1.pp + "+" + e2.pp)
  }

  // This is the same as calling `Expr.apply(right, pp)`
  val assembled = Expr(right, pp)
  val init = HasRight(false)

  object test {
    println( threepluszeroplusfive(assembled)(init).pp ) // prints 3+5+0
    println( threeplusfourplusfive(assembled)(init).pp ) // prints 3+(4+5)
  }
}


/**
 * Section 2.5 - L-Attributed Grammars
 * -----------------------------------
 * Differences to `section2.subsection5`:
 *   The trait `PreInhAlg`, as well as the trait for final assembly are both
 *   generated. Composing the synthesized attributes and assembling them 
 *   with the corresponding inherited attribute amounts to 
 *
 *       Expr(pos, pp <+ count)
 */
object subsection5 {

  import attributes._
  import examples._

  /**
   * the trait `PreInhAlg` is generated and called `InhExprSig` since it is the
   * derived signature for inherited attributes.
   */
  //- trait PreInhAlg[-E, +Out] {
  //-   def add1: Out
  //-   def add2: E => Out
  //- }

  /**
   * As above for synthesized attributes, this equation can be found in the 
   * generated code as 
   *
   *     type Inh[-Expr, -Self, +Out] = InhExprSig[Expr, Self => Out]
   */
  //- type CtxInhAlg[-E, -Ctx, +Out] = PreInhAlg[E, Ctx => Out]

  /**
   * Pre-order labeling with position
   */
  val pos = new Expr.Inh[HasCount, HasPos, HasPos] {
    def add_lhs = ctx => HasPos(ctx.pos + 1)
    def add_rhs = e1 => ctx => HasPos(ctx.pos + e1.count + 1)
  }

  val count = new Expr.Syn[HasCount, Any, HasCount] {
    def lit = n => ctx => HasCount(1)
    def add = (e1, e2) => ctx => HasCount(e1.count + e2.count + 1)
  }

  val pp = new Expr.Syn[HasPP, HasPos, HasPP] {
    def lit = n => ctx => HasPP("(" + n + "){" + ctx.pos + "}")
    def add = (e1, e2) => ctx => HasPP("(" + e1.pp + "+" + e2.pp + "){" + ctx.pos + "}")
  }

  // Using the binary operator and the assemble method the assembly can be
  // expressed very concisely.
  val assembled = Expr(pos, pp <+ count)

  object test {
    println( threeplusfourplusfivepluszero(assembled)(HasPos(0)).pp )
    // prints (((3){2}+(4){3}){1}+((5){5}+(0){6}){4}){0}  
  }
}


/**
 * Section 2.6 - Three ways to construct sentences
 * -----------------------------------------------
 * Differences to `section2.subsection6`:
 *   The classes representing physical trees are automatically generated in
 *   `Expr.Syntax`. The same holds for the definition of `fold`.
 */
object subsection6 {

  import attributes._
  import examples._
  import subsection5.{ assembled, pos, count, pp }

  import Expr.Syntax._
  //- trait Expr { ... }
  //- case class Add(l: Expr, r: Expr) extends Expr { ... }
  //- case class Lit(n: Int) extends Expr { ... }

  val threeplusfiveTree = Add(Lit(3), Lit(5))

  /**
   * The partial evaluation of assemble still has to be performed by hand
   */
  def threeplusfive2[Ctx, Out]
      (alg1: Expr.Inh[Ctx with Out, Ctx, Ctx],
       alg2: Expr.Syn[Ctx with Out, Ctx, Out])
        (implicit comp: Compose[Ctx, Out]): Ctx => Ctx with Out =
    ctx => {
      val in3 = alg1.add_lhs(ctx)
      val out3 = alg2.lit(3)(in3)
      val all3 = comp(in3, out3)

      val in5 = alg1.add_rhs(all3)(ctx)
      val out5 = alg2.lit(5)(in5)
      val all5 = comp(in5, out5)

      comp(ctx, alg2.add(all3, all5)(ctx))
    }

  object test {

    val init = HasPos(0)

    // 1. passing the algebra to church encoded sentences
    println("1: " + threeplusfive(assembled)(init).pp)

    // 2. folding over a tree using the algebra
    
    println("2: " + assembled(threeplusfiveTree)(init).pp )

    // 3. partially evaluating `assemble` with three plus five
    val threeplusfiveAlg = threeplusfive2(pos, pp <+ count)

    println("3: " + threeplusfiveAlg(init).pp)
  }
}