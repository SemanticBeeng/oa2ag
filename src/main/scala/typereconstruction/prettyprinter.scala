package oa2ag
package typereconstruction

object prettyprinter {

  import attributes._
  import Expr._

  trait Expr_pp extends Syn[HasPP, Any, HasPP] {
    def varExpr = (name) => ctx => name
    def absExpr = (name, body) => ctx => s"λ$name.${body.pp}"
    def appExpr = (fun, arg) => ctx => s"(${fun.pp} ${arg.pp})"
    def numExpr = (value) => ctx => value.toString
    def succExpr = (base) => ctx => s"succ ${base.pp}"
    def predExpr = (base) => ctx => s"pred ${base.pp}"
    def isZeroExpr = (base) => ctx => s"isZero ${base.pp}"
    def boolExpr = (value) => ctx => value.toString
    def ifExpr = (cond, thn, els) => ctx => s"if ${cond.pp} then ${thn.pp} else ${els.pp}"
  }
  object Expr_pp extends Expr_pp

  val inh = Inh.Default[Any]
  val syn = Expr_pp
  val init = new HasNothing {}
}
