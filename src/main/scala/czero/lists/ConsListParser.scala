package oa2ag
package czero
package parsers

import algebras._
import tokens._

trait ConsListParser[T, ElemSA, ElemIA, SA, IA] extends Parser[IA, IA with SA]
    with ConsList[T, ElemSA, ElemIA, SA, IA] {

  def elem: ParsingAlgebra[T, ElemSA, ElemIA]

  def hasNext(implicit s: TokenStream): Boolean

  def parse(attr: IA)(implicit s: TokenStream): IA with SA = {
    s.skip()

    if (!hasNext) {
      compose(attr, syn.empty(attr))
    } else {
      val headInh = inh.consHead(attr)
      val headAll = elem.parse(headInh)
      val tailInh = inh.consTail(attr, headAll)
      val tailAll = parse(tailInh)
      compose(attr, syn.cons(headAll, tailAll, attr))
    }
  }
}
object ConsListParser {
  import ConsList._
  def apply[T, ElemSA, ElemIA, SA, IA](
    el: => ParsingAlgebra[T, ElemSA, ElemIA],
    s: Syn.Complete[ElemSA, SA, IA], 
    i: Inh.Complete[ElemSA, ElemIA, SA, IA],
    cont: TokenStream => Boolean)(implicit c: Compose[IA, SA]) =
  new ConsListParser[T, ElemSA, ElemIA, SA, IA] { 
    val syn = s
    val inh = i
    val compose = c
    def elem = el
    def hasNext(implicit s: TokenStream): Boolean = cont(s)
  }
}