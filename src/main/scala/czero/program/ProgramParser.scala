package oa2ag
package czero
package parsers

import algebras._
import tokens._

trait ProgramParser[FuncsSA, FuncsIA, SA, IA] extends Program[FuncsSA, FuncsIA, SA, IA]
    with Parser[IA, IA with SA] {

  def funcs: ParsingAlgebra[List[trees.Function], FuncsSA, FuncsIA]

  def parse(attr: IA)(implicit s: TokenStream): IA with SA = {
    s skip tSharp; s eat tInclude; 
    s skip tLT; s eatId "stdio"; s eat tDot; s eatId "h"; s eat tGT;

    val progFuncsInh = inh.progFuncs(attr)
    val progFuncsSyn = funcs.parse(progFuncsInh)
    s skip tEOF
    compose(attr, syn.prog(progFuncsSyn, attr))
  }
}
object ProgramParser {
  import Program._
  def apply[FuncsSA, FuncsIA, SA, IA](
    fs: => ParsingAlgebra[List[trees.Function], FuncsSA, FuncsIA],
    s: Syn.Complete[FuncsSA, SA, IA], 
    i: Inh.Complete[FuncsIA, SA, IA])(implicit c: Compose[IA, SA]) =
      new ProgramParser[FuncsSA, FuncsIA, SA, IA] { 
        val syn = s; val inh = i; val compose = c; def funcs = fs
      }
}