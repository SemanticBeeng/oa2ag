package oa2ag
package czero
package object parsers {

  import java.io.{ StringReader, Reader, FileReader }
  import tokens._

  // A mutable token stream
  class TokenStream(s: Stream[Token]) {
    private var _stream = s
    
    def token: Token = _stream.head
    def is(t: Token): Boolean = token == t
    def eat: Token = { val t = token; _stream = _stream.tail; t }
    def eat(exp: Token): Token = token match {
      case `exp` => eat
      case t => sys error s"[error]: Expected token $exp, found $t"
    }
    def eatId(name: String = "*"): tId = (name, token) match {
      case ("*", t @ tId(_)) => { eat; t }
      case (_, t @ tId(n)) if n == name => { eat; t }
      case (_, t) => sys error s"[error]: Expected token tId($name), found $t"
    }
    def eatConst(): tConst = token match {
      case c: tConst => { eat; c }
      case _ => sys error "[error]: Expected constant"
    }
    def skip() { while (is(tWhite)) eat }
    def skip(exp: Token) { skip(); eat(exp) }
  }

  trait Parser[-Context, +Out] {
    def parse(attr: Context)(implicit s: TokenStream): Out
  }
  type ParsingAlgebra[T, SA, IA] = Algebra[T, SA, IA] with Parser[IA, IA with SA]
  
}