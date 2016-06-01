package oa2ag
package czero
package examples.compiler

import attributes._
import equations.{ DeclListOffset, DeclListVariables, ListCount, ListCode, 
                   FuncListHasFunctions, FunctionInit }
import ConsList.ops._
import parsers.{ ConsListParser, ArgumentListParser, ParsingAlgebra }
import tokens._

object argumentList {

  lazy val compiler: ArgumentListParser[
    HasCode, 
    HasFunctions with HasVariables, 
    HasCount with HasCode, 
    HasFunctions with HasVariables] = 
      ArgumentListParser(expression.compiler, 
        ListCount <+ ListCode, 
        ConsList.Inh.Default[Any, HasFunctions with HasVariables, Any, HasFunctions with HasVariables])
}

object declarationList {
  
  lazy val compiler = ConsListParser(
    declaration.compiler, 
    DeclListVariables <+ ListCount <+ ListCode, 
    ConsList.Inh.Forward[HasVariables] <+ DeclListOffset, 
    s => s is tInt)
}

object statementList {
  
  lazy val compiler = ConsListParser(
    statement.compiler, 
    ListCode, 
    ConsList.Inh.Default[Any, HasFunctions with HasVariables, Any, HasFunctions with HasVariables], 
    s => !(s is tRBrack))
}

object functionList {
  
  lazy val compiler = ConsListParser(
    function.compiler, 
    ListCode <+ FuncListHasFunctions, 
    FunctionInit, 
    s => (s is tInt) || (s is tId("main")))
  
}
