package oa2ag
package typereconstruction

object eval {

  import attributes._
  import Expr._

  trait Eval extends Syn[CanEval, Any, CanEval]

  trait Abstractions extends Eval {
    def varExpr = name => ctx => CanEval(env => env(name))
    def absExpr = (name, body) => ctx => 
      CanEval(env => ClosureV(v => body.eval(env + (name -> v))))
    def appExpr = (fun, arg) => ctx => 
      CanEval(env => fun.eval(env) match {
        case ClosureV(impl) => impl(arg.eval(env))
        case _ => sys error "Can only call functions"
      })
  }

  trait Numbers extends Eval {
    def numExpr = v => ctx => CanEval(_ => IntV(v))
    def succExpr = e => ctx => CanEval(env => e.eval(env) match {
      case IntV(n) => IntV(n + 1)
      case _ => sys error "Can only call succ on numbers"
    })
    def predExpr = e => ctx => CanEval(env => e.eval(env) match {
      case IntV(n) => IntV(n - 1)
      case _ => sys error "Can only call pred on numbers"
    })
  }

  trait Conditionals extends Eval {
    def isZeroExpr = e => ctx => CanEval(env => e.eval(env) match {
      case IntV(0) => BooleanV(true)
      case _ => BooleanV(false)
    })
    def boolExpr = v => ctx => CanEval(_ => BooleanV(v))
    def ifExpr = (cond, t, e) => ctx => CanEval(env => cond.eval(env) match {
      case BooleanV(true) => t.eval(env)
      case _ => e.eval(env)
    })
  }

  val Eval = new Numbers with Abstractions with Conditionals {}

  val inh = Inh.Default[Any]
  val syn = Eval
  val init = new HasNothing {}
}
