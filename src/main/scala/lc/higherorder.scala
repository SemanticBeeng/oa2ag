package oa2ag
package lc
package higherorder

object equations {
  
  import attributes._
  import plain.equations._


  /** Expressions */

  // Expr.swapped
  // ------------
  // Swaps the children of add nodes (simple higher order attribute)
  trait Expr_swapped[E] extends Expr.Syn[HasSwappedArgs[E], Any, HasSwappedArgs[E]] {
    def alg: Expr.Algebra[E]

    def const = v => self => alg.const(v)

    def add = (l, r) => self => alg.add(r.swappedArgs, l.swappedArgs)
    def id = n => self => alg.id(n)
    def lambda = (n, b) => self => alg.lambda(n, b.swappedArgs)
    def call = (f, a) => self => alg.call(f.swappedArgs, a.swappedArgs)
  }
  def Expr_swapped[E](a: Expr.Algebra[E]) = new Expr_swapped[E] { lazy val alg = a }


  /** Let-Expressions */

  // LetExpr.pp
  // ----------
  // Pretty printer for let expressions
  trait LetExpr_pp extends Expr_pp with LetExpr.Syn[HasPP, Any, HasPP] {
    def let = (n, e, b) => self => s"Let($n, ${e.pp}, ${b.pp})"
  }
  object LetExpr_pp extends LetExpr_pp


  // LetExpr.default
  // ---------------
  val LetExpr_default = LetExpr.Inh.Identity[Any]


  // LetExpr.desugar
  // ---------------
  trait LetExpr_desugar[E] extends LetExpr.Syn[CanDesugar[E], Any, CanDesugar[E]] {

    def alg: Expr.Algebra[E]

    def let = (n, e, b) => self => alg.call(alg.lambda(n, b.desugar), e.desugar)

    // boring cases - but not so nice, since this pretty much looks like a fold!
    def const = v => self => alg.const(v)
    def add = (l, r) => self => alg.add(r.desugar, l.desugar)
    def id = n => self => alg.id(n)
    def lambda = (n, b) => self => alg.lambda(n, b.desugar)
    def call = (f, a) => self => alg.call(f.desugar, a.desugar)
  }
  def LetExpr_desugar[E](a: Expr.Algebra[E]) = 
    new LetExpr_desugar[E] { lazy val alg = a; }

  // type to be used on `Self`. It's `IA with ...` to allow self being passed
  // as inherited attribute to the desugared result.
  // TODO Make `Desugared` a function type to be used as a view bound
  type Desugared[IA, SA] = IA with CanDesugar[IA => IA with SA]


  // LetExpr.forward
  // ---------------
  // Allows implicit forwarding of attributes to a desugaring.
  trait LetExpr_forward[IA, Self] 
      extends LetExpr.Syn.Decorate[Any, Desugared[IA, Self], Self] {
    implicit val transform = (self: Desugared[IA, Self]) => self.desugar(self)
  }
  def LetExpr_forward[IA, Self] = new LetExpr_forward[IA, Self] {}


  // LetExpr.fv
  // ----------
  // Manual implementation of forwarding, not necessary anymore.
  // Use `LetExpr_forward[IA, HasFV]` instead!
  trait LetExpr_fv[IA] extends LetExpr.Syn[Any, Desugared[IA, HasFV], HasFV] {

    // Allows avoiding the desugaring and calling self.fv directly
    implicit def forward(self: Desugared[IA, HasFV]): IA with HasFV =
      self.desugar(self)

    def let = (n, e, b) => identity
    def const = v => identity
    def add = (l, r) => identity
    def id = n => identity
    def lambda = (n, b) => identity
    def call = (f, a) => identity
  }
  def LetExpr_fv[IA] = new LetExpr_fv[IA] {}


  // LetExpr.fvpp
  // ------------
  // A pretty printer that also prints free variables
  val LetExpr_fvpp = LetExpr.Syn.Decorate[HasPP with HasFV, HasPP] { 
    self => s"${self.pp}: ${self.fv}"
  }

   // Now we want forwarding to avoid having to write this manually...
   // UPDATE: not needed anymore, see decorator version LetExpr_fvpp instead
  trait LetExpr_fvpp_old extends LetExpr.Syn[Any, HasPP with HasFV, HasPP] {

    // Self => HasPP
    val nicePP = (self: HasPP with HasFV) => 
      HasPP(s"${self.pp}: ${self.fv}")

    def let = (n, e, b) => nicePP
    def const = v => nicePP
    def add = (l, r) => nicePP
    def id = n => nicePP
    def lambda = (n, b) => nicePP
    def call = (f, a) => nicePP
  }
  object LetExpr_fvpp_old extends LetExpr_fvpp_old


  // LetExpr.pos
  // -----------
  trait LetExpr_pos extends Expr_pos 
      with LetExpr.Inh[HasPosition, HasPosition, HasPosition] {
    def let1 = n => parent => parent.pos + 1
    def let2 = (n, e) => parent => e.pos + 1
  }
  object LetExpr_pos extends LetExpr_pos


  // LetExpr.pospp
  // -------------
  val LetExpr_pospp = LetExpr.Syn.Decorate[HasPP with HasPosition, HasPP] { 
    self => s"${self.pp}: ${self.pos}"
  }
}

object test {
  import attributes._
  import plain.equations._
  import equations._
  import Expr.Syntax._

  val program = Add(Add(Const(22), Id("x")), Const(8))

  // Let's try swapping of the add node and using a different algebra afterwards.
  val algAfterDesugaring = Expr(Expr_default, Expr_pp)
  val desugarAlg = Expr(Expr_default, Expr_swapped(algAfterDesugaring))

  val map = HasVariables(Map("x" -> IntValue(12)))

  // that pretty much looks like multiple passes...
  println( desugarAlg(program)(map).swappedArgs(map).pp )

  // test case for annotation of the Δ (= HasValue)
  val x: Expr.Syn[HasFV with HasPP, HasValue, HasFV with HasPP] = 
    Expr.Syn.Require[HasValue] <+ Expr_fv <+ Expr_ppp


  /** LetExpr */

  // The base algebra to desugar to
  val expAlg = Expr(Expr_default, Expr_fv <+ Expr_pp)

  def letProgram[E](alg: LetExpr.Algebra[E]): E = {
    import alg._
    let("x", add(const(3), id("y")), add(id("x"), id("z")))
  }

  locally {

    val beforeDesugaring = letProgram(LetExpr(LetExpr_default, LetExpr_desugar(expAlg)))
    val desugared = beforeDesugaring(null).desugar
    println( desugared(map).pp ) //=> x + x

  }

  locally {

    val base = Expr(Expr_pos, Expr_fv <+ Expr_pp)

    // we have to explicitly repeat the IA for expAlg on LetExpr_fv (= Any)
    // when composing them
    val working = LetExpr_pp <+ LetExpr_desugar(expAlg) <+ LetExpr_fv[Any] <+ LetExpr_fvpp

    // using generic forwarding we can selectively choose to forward attributes.
    // Try the following alternatives:
    //
    // LetExpr_forward[Any, HasFV]             forwards only the free variables attribute
    // LetExpr_forward[Any, HasFV with HasPP]  forwards free variables and pretty printer
    val working2 = LetExpr_pp <+ LetExpr_desugar(expAlg) <+ 
      LetExpr_forward[Any, HasFV] <+ LetExpr_fvpp
    
    val alg = LetExpr(LetExpr_pos, working2) //module(base) <+ LetExpr_pospp)

    // The exported module interface contains all those `Compose` instances that 
    // contain `IA`.
    def module[IA, SA <: HasFV](base: Expr.Algebra[IA => IA with SA])(implicit
        c1: Compose[IA, HasPP],
        c2: Compose[IA, HasPP with CanDesugar[IA => IA with SA]],
        c3: Compose[IA, HasPP with CanDesugar[IA => IA with SA] with HasFV]) =

      // Both require and fv need the annotation HasPosition 
      // imposed by `base`
      LetExpr.Syn.Require[IA] <+ LetExpr_pp <+ LetExpr_desugar(base) <+ LetExpr_fv[IA] <+ LetExpr_fvpp

    val modular = LetExpr(LetExpr_pos, module(base))

    println( letProgram(alg)(0).pp )
  }

  locally {

    import Expr._
    import Syn._

    // "Predecorate"
    // Decorate[FV => PP]
    val predecorator = Decorate { (self: HasFV) => HasPP("${self.fv}")  }

    // Alg[Any, PP, HasPosition]
    val alg = Decorate { (self: HasPP) => HasPosition(self.pp.size) }

    val composed: Syn[Any, HasFV, HasPosition] = predecorator <+ alg

    // "Postdecorate"
    // Decorate[Pos => FV]
    val postdecorator = Decorate { (self: HasPosition) => HasFV(Set.empty) }

    val composed2: Syn[Any, HasPP, HasFV] = alg <+ postdecorator
  }


  /**
   * Is the child parameter necessary?
   * ---------------------------------
   * The reason to keep it: Dependency on synthesized attributes that are composed
   * to the rhs of the current attribute. For instance:
   *
   *    Syn[A, Any, B] <+ Syn[Any, B, A] = Syn[A, Any, A with B]
   */
  locally {
    import Expr._
    import Syn._

    val alg: Syn[HasPP, Any, HasPP with HasFV] = 
      Expr.Syn.Dummy[HasPP, Any, HasFV] <+ Expr.Syn.Dummy[Any, HasFV, HasPP]

    // can form a complete algebra with Inh[Any, Any, Any]
    Expr(Expr_default, alg)

    // Trying the same without using Child
    val algWithout = Expr.Syn.Dummy[Any, HasPP, HasFV] <+ Expr.Syn.Dummy[Any, HasFV, HasPP]
    // Expr(Expr_default, algWithout) //=> Error: Inherited attribute HasPP missing...
  }
}