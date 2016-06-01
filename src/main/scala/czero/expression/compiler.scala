package oa2ag
package czero
package examples.compiler

package object expression {
  
  import attributes.{ HasFunctions, HasVariables }
  import equations.{ ExpressionCode }
  import algebras.Expression
  import parsers.{ ExpressionParser }

  lazy val compiler = ExpressionParser(
    argumentList.compiler,
    ExpressionCode,
    Expression.Inh.Default[HasFunctions with HasVariables, Any, HasFunctions with HasVariables])
  
}
