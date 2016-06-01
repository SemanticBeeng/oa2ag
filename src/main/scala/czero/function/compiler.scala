package oa2ag
package czero
package examples.compiler

package object function {
  
  import attributes.{ HasFunctions }
  import equations.{ FunctionCode, FunctionHasFunctions, FunctionOffset, FunctionVariables }
  import algebras.Function
  import algebras.Function.ops._
  import parsers.{ FunctionParser }

  lazy val compiler = FunctionParser(
    declarationList.compiler, 
    statementList.compiler, 
    FunctionCode <+ FunctionHasFunctions, 
    Function.Inh.Default[Any, Any, HasFunctions, Any, HasFunctions] <+ 
      FunctionOffset <+ FunctionVariables)
}
