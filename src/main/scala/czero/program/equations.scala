package oa2ag
package czero
package examples.compiler
package equations

import attributes._
import algebras.Program

object ProgramCode extends Program.Syn.Complete[HasCode, HasCode, Any] {
  def prog(funcs: FuncList, self: Self): Out = 
    prelude ++ Seq("") ++ funcs.code ++ Seq("")
}

object FuncListInit extends Program.Inh.Complete[HasFunctions, Any, Any] {
  def progFuncs(parent: IA): FuncsIA = HasFunctions(buildIns, Map.empty)
}
