package oa2ag
package lc

object attributes {
  trait Value
  case class IntValue(value: Int) extends Value
  case class Closure(f: Value => Value) extends Value

  trait HasValue extends HasNothing { val value: Value }
  implicit def HasValue(v: Value) = new HasValue { val value = v }

  trait HasFV extends HasNothing { val fv: Set[String] }
  implicit def HasFV(vars: Set[String]) = new HasFV { val fv = vars }

  trait HasVariables extends HasNothing { val variables: Map[String, Value] }
  implicit def HasVariables(vars: Map[String, Value]) = new HasVariables { val variables = vars }

  trait HasPP extends HasNothing { val pp: String }
  implicit def HasPP(p: String) = new HasPP { val pp = p }

  // Needs to be polymorphic in the type of expressions
  trait CanDesugar[+Expr] extends HasNothing { val desugar: Expr  }
  implicit def CanDesugar[Expr](as: Expr) = new CanDesugar[Expr] { val desugar = as }

  trait HasSwappedArgs[Expr] extends HasNothing { val swappedArgs: Expr  }
  implicit def HasSwappedArgs[Expr](as: Expr) = new HasSwappedArgs[Expr] { val swappedArgs = as }

  trait HasPosition extends HasNothing { val pos: Int }
  implicit def HasPosition(p: Int) = new HasPosition { val pos = p }

  trait CanSayHello extends HasNothing { val sayHello: String => String }
  implicit def CanSayHello(f: String => String) = new CanSayHello { val sayHello = f }
}