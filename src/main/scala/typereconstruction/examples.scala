package oa2ag
package typereconstruction

object examples {
  import Expr._
  import attributes._

  def program[E](sem: Algebra[E]): E = {
    import sem._
    appExpr(
      absExpr("x", ifExpr(isZeroExpr(varExpr("x")), succExpr(varExpr("x")), varExpr("x"))),
      numExpr(0))
  }

  def brokenProgram[E](sem: Algebra[E]): E = {
    import sem._
    appExpr(
      absExpr("x", ifExpr(isZeroExpr(varExpr("x")), succExpr(varExpr("x")), boolExpr(true))),
      numExpr(0))
  }

  locally {
    val evaluator = Expr(eval.inh, eval.syn)
    println(program(evaluator)(null).eval(Map.empty))
  }

  locally {
    val typechecker = Expr(typecheck.inh, prettyprinter.syn[HasTypeEnv] <+ typecheck.syn)
    val result = program(typechecker)(typecheck.init)
    println(result.tpe + " | " + result.constraints + " : " + result.unifier)
  }

  locally {
    val interpreter = Expr(typecheck.inh <+ eval.inh, prettyprinter.syn[HasTypeEnv] <+ typecheck.syn <+ eval.syn)
    val init = Compose(eval.init, typecheck.init)
    val result = program(interpreter)(init)
    println(result.eval(Map.empty) + ": " + result.principleType)

    // brokenProgram(interpreter)(init)
    //=> Cannot unify: if isZero x then succ x else true | Set(...)
  }
}
