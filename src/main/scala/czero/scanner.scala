package oa2ag
package czero

import scala.util.parsing.input.StreamReader
import java.io.{ StringReader, Reader }

object tokens {

  trait Token

  case object tLPar extends Token
  case object tRPar extends Token
  case object tAssign extends Token
  case object tSemi extends Token
  case object tComma extends Token
  case object tEq extends Token { override def toString = "==" }
  case object tNE extends Token { override def toString = "!=" }
  case class tId(value: String) extends Token
  case class tConst(value: Int) extends Token
  case class tChar(value: Char) extends Token
  case object tAdd extends Token { override def toString = "+" }
  case object tSub extends Token { override def toString = "-" }
  case object tMul extends Token { override def toString = "*" }
  case object tDiv extends Token { override def toString = "/" }
  case object tMod extends Token { override def toString = "%" }
  case object tLBrack extends Token
  case object tRBrack extends Token
  case object tOr extends Token { override def toString = "||" }
  case object tAnd extends Token { override def toString = "&&" }
  case object tNot extends Token { override def toString = "!" }
  case object tLT extends Token { override def toString = "<" }
  case object tGT extends Token { override def toString = ">" }
  case object tLEq extends Token { override def toString = "<=" }
  case object tGEq extends Token { override def toString = ">=" }
  case object tSharp extends Token
  case object tDot extends Token
  case object tInt extends Token
  case object tIf extends Token
  case object tElse extends Token
  case object tWhile extends Token
  case object tGetChar extends Token { override def toString = "getchar" }
  case object tPutChar extends Token
  case object tInclude extends Token
  case object tReturn extends Token
  case object tEOF extends Token
  case object tError extends Token
  case object tWhite extends Token
}


object Scanner {

  import tokens._

  private def letter(c: Char) = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
  private def digit(c: Char) = ('0' <= c && c <= '9')
  private def alpha(c: Char) = letter(c) || digit(c)

  private object :: {
    def unapply(reader: StreamReader): Option[(Char, StreamReader)] = reader.atEnd match {
      case true => None
      case _ => Some((reader.first, reader.rest))
    }
  }

  private def readId(reader: StreamReader): (String, StreamReader) = 
    readWhile(reader, alpha) match { case (cs, r) => (cs mkString "", r) }

  private def readInt(reader: StreamReader): (Int, StreamReader) = 
    readWhile(reader, digit) match { case (cs, r) => ((cs mkString "").toInt, r) }

  private def readWhile(reader: StreamReader, cond: Char => Boolean): (List[Char], StreamReader) = reader match {
    case c :: r if cond(c) => readWhile(r, cond) match {
      case (cs, r2) => (c :: cs, r2)
    }
    case r => (Nil, r)
  }

  private val simpleTokens = Map(
    '(' -> tLPar, 
    ')' -> tRPar, 
    ';' -> tSemi, 
    ',' -> tComma, 
    '+' -> tAdd, 
    '-' -> tSub, 
    '*' -> tMul, 
    '/' -> tDiv, 
    '%' -> tMod,
    '{' -> tLBrack, 
    '}' -> tRBrack, 
    '|' -> tOr, 
    '&' -> tAnd, 
    '#' -> tSharp, 
    '.' -> tDot,
    '\n' -> tWhite,
    '\r' -> tWhite,
    ' ' -> tWhite
  )

  def apply(s: String): Stream[Token] = Scanner(new StringReader(s))

  def apply(in: Reader): Stream[Token] = Scanner(StreamReader(in))

  def apply(reader: StreamReader): Stream[Token] = reader match {
    case c :: r if simpleTokens isDefinedAt c => simpleTokens(c) #:: apply(r)

    case '\'' :: '\\' :: 'n' :: '\'' :: r => tConst(10) #:: apply(r)
    case '\'' :: c :: '\'' :: r => tConst(c) #:: apply(r)

    case '=' :: '=' :: r => tEq #:: apply(r)
    case '=' :: r => tAssign #:: apply(r)
    
    case '<' :: '=' :: r => tLEq #::apply(r)
    case '<' :: r => tLT #:: apply(r)

    case '>' :: '=' :: r => tGEq #:: apply(r)
    case '>' :: r => tGT #:: apply(r)

    case '!' :: '=' :: r => tNE #:: apply(r)
    case '!' :: r => tNot #:: apply(r)

    case c :: _ if letter(c) => readId(reader) match {
      case ("int", r)     => tInt #:: apply(r)
      case ("if", r)      => tIf #:: apply(r)
      case ("else", r)    => tElse #:: apply(r)
      case ("while", r)   => tWhile #:: apply(r)
      case ("getchar", r) => tGetChar #:: apply(r)
      case ("putchar", r) => tPutChar #:: apply(r)
      case ("include", r) => tInclude #:: apply(r)
      case ("return", r)  => tReturn #:: apply(r)
      case (id, r)        => tId(id) #:: apply(r)
    }
    case c :: _ if digit(c) => readInt(reader) match {
      case (n, r) => tConst(n) #:: apply(r)
    }
    case r if r.atEnd => tEOF #:: apply(r)
    case r => tError #:: apply(r)
  }
}