package oa2ag
package lc

/**
 * This file is not generated, yet.
 * --------------------------------
 * We probably need to extend the input language of the generate to also
 * account for later extension of nonterminals (Like in this case)
 */
object LetExpr extends Nonterminal {

  trait Syntax extends Expr.Syntax
  object Syntax {
    case class Let(name: String, exp: Syntax, body: Syntax) extends Syntax
  }

  /**
   * Part 1. The Language Interface
   * ------------------------------
   */
  trait Signature[-Expr, +Out] extends Expr.Signature[Expr, Out] {
    def let: (String, Expr, Expr) => Out
  }
  object Signature {

    implicit class SigOps[Expr1, Self1, Out1](alg1: Signature[Expr1, Self1 => Out1]) {

      // Impose additional requirements on the argument type
      def apply[Req]: Signature[Expr1, Req with Self1 => Out1] = alg1

      def <+[Expr2, Self2 >: Self1 with Out1, Out2](
          alg2: Signature[Expr2, Self2 => Out2])(implicit 
          comp1: Compose[Out1, Out2],
          comp2: Compose[Self1, Out1]) = 
        new DepProduct[Expr1, Self1, Out1, Expr2, Self2, Out2] {
          val (left, right, compose1, compose2) = (alg1, alg2, comp1, comp2)
        }
    }

    trait DepProduct[Expr1, Self1, Out1, Expr2, Self2 >: Self1 with Out1, Out2]
        extends Signature[Expr1 with Expr2, Self1 => Out1 with Out2]
        with Expr.Signature.DepProduct[Expr1, Self1, Out1, Expr2, Self2, Out2] {
      val left: Signature[Expr1, Self1 => Out1]
      val right: Signature[Expr2, Self2 => Out2]
      def let = (x, e, b) => assemble(_, left.let(x, e, b), right.let(x, e, b))
    }
  }

  type Algebra[E] = Signature[E, E]

  /**
   * Part 2. The transformed Interface for Inh. Attributes
   * -----------------------------------------------------
   */
  trait InhSignature[-Expr, +Out] extends Expr.InhExprSig[Expr, Out] {
    def let1: String => Out
    def let2: (String, Expr) => Out
  }
  object InhSignature {

    implicit class SigOps[Expr1, Self1, Out1](alg1: InhSignature[Expr1, Self1 => Out1]) {

      // Impose additional requirements on the argument type
      def apply[Req]: InhSignature[Expr1, Req with Self1 => Out1] = alg1

      def <+[Expr2, Self2 >: Self1 with Out1, Out2](
          alg2: InhSignature[Expr2, Self2 => Out2])(implicit 
          comp1: Compose[Out1, Out2],
          comp2: Compose[Self1, Out1]) = 
        new DepProduct[Expr1, Self1, Out1, Expr2, Self2, Out2] {
          val (left, right, compose1, compose2) = (alg1, alg2, comp1, comp2)
        }
    }

    trait DepProduct[Expr1, Self1, Out1, Expr2, Self2 >: Self1 with Out1, Out2]
        extends InhSignature[Expr1 with Expr2, Self1 => Out1 with Out2] 
        with Expr.InhExprSig.DepProduct[Expr1, Self1, Out1, Expr2, Self2, Out2] {
      
      val left: InhSignature[Expr1, Self1 => Out1]
      val right: InhSignature[Expr2, Self2 => Out2]

      def let1 = name => assemble(_, left.let1(name), right.let1(name))
      def let2 = (name, e) => assemble(_, left.let2(name, e), right.let2(name, e))
    }
  }


  /**
   * Part 3. The composition mechanism
   * ---------------------------------
   */
  trait AttributeGrammar[IA, SA] extends Expr.AttributeGrammar[IA, SA] 
      with Algebra[IA => IA with SA] {

    val inhExpr: InhSignature[IA with SA, IA => IA]
    val syn: Signature[IA with SA, IA => SA]

    def let = (n, e, b) => attr => compose(attr, {
      val exp = (e compose inhExpr.let1(n))(attr)
      val body = (b compose inhExpr.let2(n, exp))(attr)
      syn.let(n, exp, body)(attr)
    })
  }

  trait Foldable[E] extends Expr.Foldable[E] { self: Algebra[E] =>
    def apply(t: Syntax): E = t match {
      case Syntax.Let(n, e, b) => let(n, this(e), this(b))
      case _ => super.apply(t)
    }
  }

  def apply[IA, SA](
      i: Inh[IA with SA, IA, IA], 
      s: Syn[IA with SA, IA, SA])(implicit 
      c: Compose[IA, SA]) = 
    new AttributeGrammar[IA, SA] with Foldable[IA => IA with SA] {
      val (inhExpr, syn, compose) = (i, s, c)
    }

  /**
   * Part 4. Attribution Definition Utility
   * --------------------------------------
   */
  type Inh[-Expr, -Self, +Out] = InhSignature[Expr, Self => Out]
  type Syn[-Expr, -Self, +Out] = Signature[Expr, Self => Out]

  object Inh {

    /**
     * Decorates the output of an inherited attribute.
     *
     *     Inh[Any, A, B] <+ Decorate(B => C) == Inh[Any, A, B with C]
     */
    trait Decorate[-C, -S, +T] extends Inh[C, S, T] 
        with Expr.Inh.Decorate[C, S, T] {
      def let1 = _ => transform
      def let2 = (_, _) => transform
    }
    def Decorate[S, T](f: S => T) = new Decorate[Any, S, T] { 
      val transform = f 
    }

    trait Identity[-C, -S <: T, +T] extends Decorate[C, S, T] { 
      val transform: S => T = identity 
    }
    def Identity[T] = new Identity[Any, T, T] {}

    def Dummy[C, S, T] = new Decorate[C, S, T] { 
      val transform = (self: S) => ??? 
    }

    def Require[Req] = new Identity[Any, Req, Any] {}
  }

  object Syn {

    /**
     * Decorates the output of a synthesized attribute.
     *
     *     Syn[Any, A, B] <+ Decorate(B => C) == Syn[Any, A, B with C]
     *
     * please note, that the implementation of Syn.decorate and Inh.decorate coincide!
     */
    trait Decorate[-C, -S, +T] extends Syn[C, S, T] 
        with Expr.Syn.Decorate[C, S, T] {
      def let = (_, _, _) => transform
    }
    def Decorate[S, T](f: S => T) = new Decorate[Any, S, T] { 
      val transform = f 
    }

    trait Identity[-C, -S <: T, +T] extends Decorate[C, S, T] { 
      val transform: S => T = identity 
    }
    def Identity[T] = new Identity[Any, T, T] {}

    def Dummy[C, S, T] = new Decorate[C, S, T] { 
      val transform = (self: S) => ??? 
    }

    def Require[Req] = new Identity[Any, Req, Any] {}
  }
}
