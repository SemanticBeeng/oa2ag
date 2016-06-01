package oa2ag
package czero
package examples.compiler
package equations

import attributes._
import algebras.Function

// DeclsSA, DeclsIA, StmtsIA, SA, IA, Out
object FunctionVariables extends Function.Inh[Any, HasVariables, HasVariables, Any, HasVariables with HasOffset, HasVariables] {
  override def funDefDecls(parent: IA, id: String, formals: List[String]): DeclsIA = 
    parent.variables ++ formals.zipWithIndex.map { 
      case (n, idx)  => (n, idx + parent.offset) 
    }.toMap

  override def funDefStmts(parent: IA, id: String, formals: List[String], decls: Decls): StmtsIA = 
    decls
}

// DeclsSA, DeclsIA, StmtsIA, SA, IA, Out
object FunctionOffset extends Function.Inh.Complete[Any, HasOffset, Any, Any, HasOffset] {
  override def funDefDecls(parent: HasOffset, id: String, formals: List[String]): DeclsIA = 
    parent.offset + formals.size
  override def funDefStmts(parent: HasOffset, id: String, formals: List[String], decls: Decls): StmtsIA = 
    decls
}


/**
 * The inherited "functions" attribute
 */

// same trick as above. Use synthesized function interface as environment for next
// element.
object FunctionHasFunctions extends Function.Syn.Complete[Any, Any, HasFunctions, HasFunctions] {
  def funDecl(id: String, formals: List[String], self: Self): Out =
    if (self.prototypes isDefinedAt id) {
      sys error (s"duplicate declaration of $id")
    } else if (self.functions.isDefinedAt(id) && self.functions(id) != formals) {
      sys error (s"conflicting declaration of $id")
    } else {
      HasFunctions(self.functions, self.prototypes + (id -> formals))
    }
  
  def funDef(id: String, formals: List[String], decls: Decls, stmts: Stmts, self: Self): Out =
    if (self.functions isDefinedAt id) {
      sys error (s"duplicate implementation of $id")
    } else if (self.prototypes.isDefinedAt(id) && self.prototypes(id) != formals) {
      sys error (s"conflicting implementation of $id")
    } else {
      HasFunctions(self.functions + (id -> formals), self.prototypes)
    }
}


object FunctionCode extends Function.Syn.Complete[HasCode with HasCount, HasCode, HasCode, Any] {
  def funDecl(id: String, formals: List[String], self: Self): Out = Seq.empty
  def funDef(id: String, formals: List[String], decls: Decls, stmts: Stmts, self: Self): Out = {
    val localsCode = if (decls.count > 0) Seq(s".locals ${decls.count}") else Seq.empty
    
    Seq(
      s".method $id",
      s".args ${formals.size + 1}"
    ) ++ localsCode ++ decls.code ++ stmts.code ++ Seq(
      "bipush 0",
      "ireturn"
    )
  }
}

