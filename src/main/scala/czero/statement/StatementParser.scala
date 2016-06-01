package oa2ag
package czero
package parsers

import algebras._
import tokens._

trait StatementParser[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] extends Parser[IA, IA with SA]
    with algebras.Statement[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] {

  def expr: ParsingAlgebra[trees.Expression, ExprSA, ExprIA]
  def stmts: ParsingAlgebra[List[trees.Statement], StmtsSA, StmtsIA]

  def parse(attr: IA)(implicit s: TokenStream): IA with SA = compose(attr, s.token match {
    case tId(id) => {
      s.eat;
      s skip tAssign
      s.skip()
      val assignInh = inh.assignStmtValue(attr, id)
      val assignAll = expr.parse(assignInh)
      s skip tSemi
      syn.assignStmt(id, assignAll, attr)
    }
    case `tReturn` => {
      s eat tReturn
      s.skip()
      val valueInh = inh.returnStmtValue(attr)
      val valueAll = expr.parse(valueInh)
      s skip tSemi
      syn.returnStmt(valueAll, attr)
    }
    case `tIf` => {
      // cond
      s eat tIf; s skip tLPar; s.skip()

      val condInh = inh.ifStmtCond(attr)
      val condAll = expr.parse(condInh)
      val thenInh = inh.ifStmtThen(attr, condAll)
      
      s skip tRPar; s skip tLBrack
      val thenAll = stmts.parse(thenInh)
      
      s skip tRBrack
      s.skip()

      // else
      val elseInh = inh.ifStmtElse(attr, condAll, thenAll)
      val elseAll = if (s is tElse) {
        s skip tElse
        s.skip()
        s skip tLBrack
        val elseAll = stmts.parse(elseInh)
        s skip tRBrack
        elseAll
      } else {
        stmts(Nil, elseInh) // Just fold over an empty list
      }
      syn.ifStmt(condAll, thenAll, elseAll, attr)
    }
    case `tWhile` => {
      s skip tWhile; s skip tLPar; s.skip(); 
      val condInh = inh.whileStmtCond(attr)
      val condAll = expr.parse(condInh)
      s skip tRPar; s skip tLBrack; s.skip()
      val bodyInh = inh.whileStmtBody(attr, condAll)
      val bodyAll = stmts.parse(bodyInh)
      s skip tRBrack
      syn.whileStmt(condAll, bodyAll, attr)
    }
    case `tPutChar` => {
      s skip tPutChar
      s skip tLPar
      s.skip()
      val charInh = inh.putCharStmtChar(attr)
      val charAll = expr.parse(charInh)        
      s skip tRPar
      s skip tSemi
      syn.putCharStmt(charAll, attr)
    }
    case _ => sys error "[error] expected statement"
  })
}
object StatementParser {
  import Statement._
  def apply[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA](
      e: => ParsingAlgebra[trees.Expression, ExprSA, ExprIA],
      ss: => ParsingAlgebra[List[trees.Statement], StmtsSA, StmtsIA],
      s: Syn.Complete[ExprSA, StmtsSA, SA, IA], 
      i: Inh.Complete[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA])(implicit c: Compose[IA, SA]) =
    new StatementParser[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] { 
      def expr = e; def stmts = ss; val syn = s; val inh = i; val compose = c
    }
}
