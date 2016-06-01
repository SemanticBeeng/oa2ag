package oa2ag
package czero
package examples.compiler
package equations

import attributes._
import algebras.Statement

object StatementCode extends Statement.Syn.Complete[HasCode, HasCode, HasCode, HasVariables] {
  def assignStmt(id: String, value: Expr, self: Self): Out =
    if (!self.variables.isDefinedAt(id))
      sys error s"undeclared variable: $id"
    else
      value.code ++ Seq(s"istore ${self.variables(id)}")

  def whileStmt(cond: Expr, body: Stmts, self: Self): Out = {
    val loopLabel = nextLabel
    val doneLabel = nextLabel
    Seq(
      s"L${loopLabel}:"
    ) ++ cond.code ++ Seq(
      s"ifeq L${doneLabel}"
    ) ++ body.code ++ Seq(
      s"goto L${loopLabel}",
      s"L${doneLabel}:"
    )
  }
  def putCharStmt(char: Expr, self: Self): Out =
      Seq("bipush 44") ++ 
      char.code ++
      Seq("invokevirtual putchar")

  def returnStmt(value: Expr, self: Self): Out = value.code ++ Seq("ireturn")
  def ifStmt(cond: Expr, then: Stmts, els: Stmts, self: Self): Out = {
    val elseLabel = nextLabel
    val doneLabel = nextLabel
    cond.code ++ Seq(
      s"ifeq L$elseLabel"
    ) ++ then.code ++ Seq(
      s"goto L$doneLabel",
      s"L${elseLabel}:"
    ) ++ els.code ++ Seq(
      s"L${doneLabel}:"
    )
  }
}
