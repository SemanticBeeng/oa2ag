package oa2ag
package czero
package parsers

import algebras._
import tokens._

// TODO change ConsListParser to also cover this case
trait ArgumentListParser[ExprSA, ExprIA, SA, IA] extends Parser[IA, IA with SA]
    with ConsList[trees.Expression, ExprSA, ExprIA, SA, IA] {

  def elem: ParsingAlgebra[trees.Expression, ExprSA, ExprIA]

  def parse(attr: IA)(implicit s: TokenStream): IA with SA = parse(attr, false)
  def parse(attr: IA, readMore: Boolean)(implicit s: TokenStream): IA with SA = { 
    s.skip()

    if (s is tRPar) {
      if (readMore) sys error "[error]: Expected argument for function call"
      compose(attr, syn.empty(attr))
    
    } else {
      val headInh = inh.consHead(attr)
      val headAll = elem.parse(headInh)
      val tailInh = inh.consTail(attr, headAll)
      val tailAll = if (s is tComma) {
        s eat tComma
        s.skip()
        parse(tailInh, true)
      } else {
        parse(tailInh)
      }
      compose(attr, syn.cons(headAll, tailAll, attr))
    }
  }
}
object ArgumentListParser {
  import ConsList._
  def apply[ElemSA, ElemIA, SA, IA](
    el: => ParsingAlgebra[trees.Expression, ElemSA, ElemIA],
    s: Syn.Complete[ElemSA, SA, IA], 
    i: Inh.Complete[ElemSA, ElemIA, SA, IA])(implicit c: Compose[IA, SA]) =
  new ArgumentListParser[ElemSA, ElemIA, SA, IA] { 
    val syn = s; val inh = i; val compose = c; def elem = el
  }
}