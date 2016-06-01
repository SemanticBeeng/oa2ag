package oa2ag
package czero
package parsers

import algebras._
import tokens._

trait FunctionParser[DeclsSA, DeclsIA, StmtsSA, StmtsIA, SA, IA] extends Parser[IA, IA with SA]
    with Function[DeclsSA, DeclsIA, StmtsSA, StmtsIA, SA, IA] {

  def decls: ParsingAlgebra[List[trees.Declaration], DeclsSA, DeclsIA]
  def stmts: ParsingAlgebra[List[trees.Statement], StmtsSA, StmtsIA]

  def parse(attr: IA)(implicit s: TokenStream): IA with SA = {
    if (s is tInt) s eat tInt; // can be omitted if main
    
    s.skip()
    val id = s.eatId().value
    s skip tLPar
    val formals = parseFormals
    s skip tRPar
    s.skip()

    // function declaration
    if (s is tSemi) {
      s eat tSemi
      compose(attr, syn.funDecl(id, formals, attr))
      
    // function definition
    } else {
      s skip tLBrack
      val funDefDsInh = inh.funDefDecls(attr, id, formals)
      val funDefDsAll = decls.parse(funDefDsInh)
      val funDefSsInh = inh.funDefStmts(attr, id, formals, funDefDsAll)
      val funDefSsAll = stmts.parse(funDefSsInh)
      s skip tRBrack
      compose(attr, syn.funDef(id, formals, funDefDsAll, funDefSsAll, attr))
    }
  }

  private def parseFormals(implicit s: TokenStream): List[String] = {

    var formals: List[String] = Nil

    s.skip()
    var readMore = false
    while (s is tInt) {
      s eat tInt
      s.skip()
      formals = formals :+ s.eatId().value
      s.skip()

      if (s is tComma) {
        s eat tComma
        s.skip()
        readMore = true
      } else {
        readMore = false
      }
    }
    if (readMore) sys error "[error]: Expected formal argument"
    formals
  }
}
object FunctionParser {
  import Function._
  def apply[DeclsSA, DeclsIA, StmtsSA, StmtsIA, SA, IA](
    ds: => ParsingAlgebra[List[trees.Declaration], DeclsSA, DeclsIA],
    ss: => ParsingAlgebra[List[trees.Statement], StmtsSA, StmtsIA],
    s: Syn.Complete[DeclsSA, StmtsSA, SA, IA], 
    i: Inh.Complete[DeclsSA, DeclsIA, StmtsIA, SA, IA])(implicit c: Compose[IA, SA]) =
      new FunctionParser[DeclsSA, DeclsIA, StmtsSA, StmtsIA, SA, IA] { 
        val syn = s; val inh = i; val compose = c; def decls = ds; def stmts = ss
      }
}