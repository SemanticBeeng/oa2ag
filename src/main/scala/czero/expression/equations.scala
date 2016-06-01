package oa2ag
package czero
package examples.compiler
package equations

import attributes._
import algebras.Expression

object ExpressionCode extends Expression.Syn.Complete[HasCode with HasCount, HasCode, HasFunctions with HasVariables] {

  def constExpr(value: Int, self: Self): Out = HasCode(Seq(s"ldc_w $value"))

  def idExpr(id: String, self: Self): Out = {
    val addr = self.variables.getOrElse(id, sys error s"undeclared variable: $id")
    Seq(s"iload $addr")
  }

  def prefixExpr(op: String, body: Child, self: Self): Out = op match {
    case "!" => Seq("bipush 44") ++ body.code ++ Seq("invokevirtual not_")
    case "-" => Seq("bipush 0") ++ body.code ++ Seq("isub")
  }
  
  def callExpr(fun: String, args: Args, self: Self): Out = {

    val expected = (self.functions orElse self.prototypes) applyOrElse (fun, (x: String) => sys error s"undeclared function: $x")

    if (expected.size != args.count) 
      sys error s"incorrect number of arguments: $fun"

    Seq("bipush 44") ++ args.code ++ Seq(s"invokevirtual $fun")
  }

  def binaryOpExpr(lhs: Child, op: String, rhs: Child, self: Self): Out =
    if (virtualOps isDefinedAt op)
      lhs.code ++ Seq("bipush 44", "swap") ++ rhs.code ++ Seq(virtualOps(op))
    else
      lhs.code ++ rhs.code ++ Seq(integerOps(op))
}
