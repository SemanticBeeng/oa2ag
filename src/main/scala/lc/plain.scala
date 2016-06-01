package oa2ag
package lc
package plain

object equations {
  import attributes._

  /** Expressions */

  // Expr.default
  // ------------
  // Inherited attribute to be used as default attribute
  val Expr_default = Expr.Inh.Default[Any]


  // Expr.pp
  // -------
  trait Expr_pp extends Expr.Syn[HasPP, Any, HasPP] {
    def const = v => self => s"Const($v)"
    def add = (l, r) => self => s"Add(${l.pp}, ${r.pp})"
    def id = name => self => s"Id($name)"
    def lambda = (n, b) => self => s"Lambda($n, ${b.pp})"
    def call = (f, a) => self => s"Call(${f.pp}, ${a.pp})"
  }
  object Expr_pp extends Expr_pp


  // Expr.fv
  // -------
  // computes the set of free variables
  trait Expr_fv extends Expr.Syn[HasFV, Any, HasFV] {
    def const = v => self => Set.empty[String]
    def add = (l, r) => self => l.fv ++ r.fv
    def id = name => self => Set(name)
    def lambda = (n, b) => self => b.fv - n
    def call = (f, a) => self => f.fv ++ a.fv
  }
  object Expr_fv extends Expr_fv


  // Expr.value
  // ----------
  // If we would allow equation specification per production variable lookup
  // and scoping rules could be grouped together
  trait Expr_value extends Expr.Syn[HasValue, HasVariables, HasValue] {
    def const = v => _ => IntValue(v)
    def add = (l, r) => _ => (l.value, r.value) match {
      case (IntValue(x), IntValue(y)) => IntValue(x + y)
      case (l, r) => sys error s"cannot add $l + $r" 
    }
    def id = name => self => self.variables.get(name) match {
      case Some(v) => v
      case None => sys error s"not defined: $name"
    }
    def lambda = (n, b) => self => ??? // TODO implement
    def call = (f, a) => self => (f.value, a.value) match {
      case (Closure(impl), arg) => impl(arg)
      case _ => sys error "can only call functions"
    }
  }
  object Expr_value extends Expr_value


  // Expr.ppp
  // -------
  trait Expr_ppp extends Expr.Syn[HasPP, HasValue, HasPP] {
    def const = v => self => s"Const($v): ${self.value}"
    def add = (l, r) => self => s"Add(${l.pp}, ${r.pp}): ${self.value}"
    def id = name => self => s"Id($name): ${self.value}"
    def lambda = (n, b) => self => s"Lambda($n, ${b.pp})"
    def call = (f, a) => self => s"Call(${f.pp}, ${a.pp}): ${self.value}"
  }
  object Expr_ppp extends Expr_ppp


  // Expr.pos
  // --------------
  // Calculuates the position within an in-order traversal
  trait Expr_pos extends Expr.Inh[HasPosition, HasPosition, HasPosition] {
    def add_lhs = parent => parent.pos + 1
    def add_rhs = lhs => parent => lhs.pos + 1
    def call_fun = parent => parent.pos + 1
    def call_arg = exp => parent => exp.pos + 1
    def lambda_body = name => parent => parent.pos + 1
  }
  object Expr_pos extends Expr_pos


  // Expr.variables
  // --------------
  // Just passing variables through
  val Expr_variables = Expr.Inh.Default[HasVariables]


  // Expr.state
  // ----------
  // Cool thing: We can defined combined synthesized and inherited attributes
  // that may share state
  trait Expr_state extends Expr.Syn[Any, Any, Any] with Expr.Inh[Any, Any, Any]

  // Expr.sayHello
  // -------------
  val Expr_sayHello = Expr.Syn.Decorate { 
    (s: Any) => CanSayHello(n => s"Hello '$n'")
  }

  // Expr.helloPP
  // ------------
  val Expr_helloPP = Expr.Syn.Decorate { 
    (s: CanSayHello with HasPP) => HasPP(s.sayHello(s.pp))
  }

  /** Statements */

  // Stmt.value
  // ----------
  trait Stmt_value extends Stmt.Syn[HasValue, HasValue, Any, HasValue] {
    def assign = (name, value) => _ => value
    def seq = (head, tail) => _ => tail
    def exprStmt = (expr) => _ => expr
  }
  object Stmt_value extends Stmt_value


  // Stmt.variables
  // --------------
  trait Stmt_variables 
      extends Stmt.Syn[HasValue, Any, HasVariables, HasVariables]
      with Stmt.InhStmt[HasVariables, HasVariables, HasVariables]
      with Stmt.InhExpr[HasVariables, HasVariables, HasVariables] {

    def assign_value = name => p => p
    def assign = (name, value) => self => self.variables + (name -> value.value)

    def seq_head = p => p
    def seq_tail = s => p => s 
    def seq = (head, tail) => self => self // ???
    
    def exprStmt_exp = p => p
    def exprStmt = expr => self => self
  }
  object Stmt_variables extends Stmt_variables


  // Stmt.pp
  // -------
  trait Stmt_pp extends Stmt.Syn[HasPP, HasPP, Any, HasPP] {
    def assign = (name, value) => _ => s"$name = ${value.pp}"
    def seq = (head, tail) => _ => s"${head.pp};\n${tail.pp}"
    def exprStmt = expr => _ => expr
  }
  object Stmt_pp extends Stmt_pp
}

object test {
  import attributes._
  import equations._
  import Expr.Syntax._
  import Stmt.Syntax._

  val program = Add(Add(Const(22), Id("x")), Const(8))

  val evalExpr = Expr(Expr_variables, Expr_value)
  // only works this way around (The same as depProduct)
  val debugEvalExpr = Expr(Expr_variables, Expr_value <+ Expr_pp)

  Expr_value <+ Expr.Syn.Default[Any] // test for identity
  // Expr.Syn.Default[Any] <+ Expr_value // does not work!

  // It does work however if we specify the inherited attributes manually as dependency
  val works = Expr.Syn.Require[HasVariables] <+ Expr_value

  // Same here: Dependencies only can be fulfilled from the left to the right, so 
  // we have to put HasValue as required dependency here:
  val worksAlso = Expr_pp[HasValue with HasVariables] <+ Expr_value

  val eval2Expr = Expr(Expr_variables, works)
  val debug2Expr = Expr(Expr_variables, Expr_value <+ worksAlso)

  println( eval2Expr(program)(HasVariables(Map("x" -> IntValue(12)))).value )
  println( debugEvalExpr(program)(HasVariables(Map("x" -> IntValue(12)))).pp )


  /* Statements */

  val program2 = Seq(
      Assign("y", Add(Const(2), Const(3))), Seq(
      Assign("x", Add(Const(2), Id("y"))),
      ExprStmt(program)))

  // We are using the same algebra for synthesized and inherited attributes!
  val evalStmt = Stmt(evalExpr, Stmt_variables, Stmt_variables, Stmt_variables <+ Stmt_value)

  val debugStmt = Stmt(debugEvalExpr,
    Stmt_variables,
    Stmt_variables,
    Stmt_variables <+ Stmt_value <+ Stmt_pp)

  println( evalStmt(program2)(HasVariables(Map.empty)).value )
  println( debugStmt(program2)(HasVariables(Map.empty)).pp )


  /* Polymorphic Embedding */

  def program3[Expr, Stmt](eAlg: Expr.Algebra[Expr], sAlg: Stmt.Algebra[Expr, Stmt]) = {
    import eAlg._
    import sAlg._

    seq(
      assign("y", add(const(2), const(3))), seq(
      assign("x", add(const(2), id("y"))),
      exprStmt(id("x"))))
  }

  println( program3(evalExpr, evalStmt)(HasVariables(Map.empty)).value )



  /**
   * Parametrized Attributes
   * -----------------------
   * We can support parametrized attributes without having to adapt the
   * macro by using anonymous function values.
   */
  locally {

    // helloPP invokes sayHello with the pretty printed string generated
    // by pp
    val parametrized = Expr(Expr_default, Expr_pp <+ Expr_sayHello <+ Expr_helloPP)

    println(parametrized(program)().pp)
  }
}