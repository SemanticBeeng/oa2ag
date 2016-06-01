package oa2ag
package czero
package parsers

import algebras._
import tokens._

trait DeclarationParser[SA, IA] extends Parser[IA, IA with SA] 
    with Declaration[SA, IA] {

  def parse(attr: IA)(implicit s: TokenStream): IA with SA = {
    val tpe = s eat tInt // currently is ignored, always Int
    s.skip()
    val id = s.eatId().value
    s.skip()

    val init = if (s is tAssign) {
      s eat tAssign
      s.skip()
      Some(s.eatConst().value)
    } else {
      None
    }
    s eat tSemi;
    
    compose(attr, syn.intDecl(id, init, attr))
  }
}
object DeclarationParser {
  import Declaration._

  def apply[SA, IA](s: Syn.Complete[SA, IA])(implicit c: Compose[IA, SA]) =
    new DeclarationParser[SA, IA] { val syn = s; val compose = c }
}