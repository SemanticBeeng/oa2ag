package oa2ag
package czero

object trees {
 
  trait Tree

  trait Program
  case class Prog(funcs: List[Function]) extends Program

  trait Function extends Tree
  case class FunDecl(id: String, formals: List[String]) extends Function
  case class FunDef(id: String, formals: List[String], decls: List[Declaration], stmts: List[Statement]) extends Function

  trait Declaration extends Tree
  case class IntDecl(id: String, init: Option[Int]) extends Declaration

  trait Statement extends Tree
  case class AssignStmt(id: String, value: Expression) extends Statement
  case class WhileStmt(cond: Expression, body: List[Statement]) extends Statement
  case class PutCharStmt(char: Expression) extends Statement
  case class ReturnStmt(value: Expression) extends Statement
  case class IfStmt(cond: Expression, then: List[Statement], els: List[Statement]) extends Statement

  trait Expression extends Tree
  case class ConstExpr(value: Int) extends Expression
  case class IdExpr(id: String) extends Expression
  case class PrefixExpr(op: String, body: Expression) extends Expression
  case class CallExpr(funName: String, args: List[Expression]) extends Expression
  case class BinaryOpExpr(lhs: Expression, op: String, rhs: Expression) extends Expression

}
