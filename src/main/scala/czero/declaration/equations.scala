package oa2ag
package czero
package examples.compiler
package equations

import attributes._
import algebras.Declaration

/**
 * The inherited "variables" attribute
 */  
object DeclarationVariables extends Declaration.Syn.Complete[HasVariables, HasVariables with HasOffset] {
  def intDecl(id: String, init: Option[Int], self: Self): Out =
    if (self.variables isDefinedAt id) {
      sys error s"Duplicate declaration of $id"
    } else {
      self.variables + (id -> self.offset)
    }
}

object DeclarationCode extends Declaration.Syn.Complete[HasCode, HasOffset] {
  def intDecl(id: String, init: Option[Int], self: Self): Out = init match {
    case Some(value) => Seq(
      s"bipush $value",
      s"istore ${self.offset}"
    )
    case None => Seq.empty
  }
}
