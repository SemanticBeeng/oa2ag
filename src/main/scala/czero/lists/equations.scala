package oa2ag
package czero
package examples.compiler
package equations

import attributes._

// Declarations don't have an `Inh` interface since there is no nonterminal on the rhs.
// so here we are collecting the variables as synthesized attribute
// Then this syn. attribute is used as environment for statements
object DeclListVariables extends ConsList.Syn.Complete[HasVariables, HasVariables, HasVariables] {
  def empty(self: Self): Out = self
  def cons(head: Elem, tail: Child, self: Self): Out = tail
}

object DeclListOffset extends ConsList.Inh.Default[Any, HasOffset, Any, HasOffset, HasOffset] {
  override def consTail(parent: IA, head: ElemIA with ElemSA): Out = head.offset
}

object FuncListHasFunctions extends ConsList.Syn.Complete[HasFunctions, HasFunctions, HasFunctions] {
  def empty(self: Self): Out = self
  def cons(head: Elem, tail: Child, self: Self): Out = tail
}

object ListCode extends ConsList.Syn.Complete[HasCode, HasCode, Any] {
  def empty(self: Self): Out = Seq.empty
  def cons(head: Elem, tail: Child, self: Self): Out = head.code ++ tail.code
}

object ListCount extends ConsList.Syn.Complete[Any, HasCount, Any] {
  def empty(self: Self): Out = 0
  def cons(head: Elem, tail: Child, self: Self): Out = tail.count + 1
}

object FunctionInit extends ConsList.Inh.Complete[Any, HasFunctions with HasOffset with HasVariables, Any, HasFunctions] {
  def consHead(parent: IA): ElemIA = Compose(parent, HasOffset(1), HasVariables(Map.empty[String, Int]))
  def consTail(parent: IA, head: ElemIA with ElemSA): Out = head
}
