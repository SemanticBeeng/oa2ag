package oa2ag
package czero
package examples.compiler

package object statement {
  
  import attributes.{ HasFunctions, HasCode, HasVariables }
  import equations.{ StatementCode }
  import algebras.Statement
  import parsers.{ StatementParser }

  type FunVar = HasFunctions with HasVariables

  lazy val compiler: StatementParser[HasCode, FunVar, HasCode, FunVar, HasCode, FunVar] =
    StatementParser(
      expression.compiler, 
      statementList.compiler, 
      StatementCode,
      Statement.Inh.Default[Any, FunVar, Any, FunVar, Any, FunVar])
}
