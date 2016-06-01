package oa2ag
package czero
package parsers

import algebras._
import tokens._

trait ExpressionParser[ArgsSA, ArgsIA, SA, IA] extends Parser[IA, IA with SA] 
    with algebras.Expression[ArgsSA, ArgsIA, SA, IA] {

  def args: ParsingAlgebra[List[trees.Expression], ArgsSA, ArgsIA]

  def parse(attr: IA)(implicit s: TokenStream): IA with SA = parseExp3(attr)

  // unary expressions
  private def parseExp0(attr: IA)(implicit s: TokenStream): IA with SA = compose(attr, s.token match {
    case tConst(value) => {
      s.eat
      syn.constExpr(value, attr)
    }
    // This also is just the syntax of a function call
    // TODO remove completely from scanner and parser!
    case `tGetChar` => {
      s eat tGetChar;
      s.skip();
      val id = tGetChar.toString
      val callInh = inh.callExprArgs(attr, id)
      s eat tLPar
      s.skip()
      val callAll = args.parse(callInh)
      s skip tRPar
      syn.callExpr(id, callAll, attr)
    }
    // group expressions
    case `tLPar` => {
      s.eat;
      val res = parse(attr)
      s skip tRPar
      res
    }
    // identifier / call expression
    case tId(id) => {
      s.eat;
      s.skip();

      // call expr
      if (s is tLPar) {
        val callInh = inh.callExprArgs(attr, id)
        s eat tLPar
        s.skip()
        val callAll = args.parse(callInh)
        s skip tRPar
        syn.callExpr(id, callAll, attr)

      // id expr
      } else {
        syn.idExpr(id, attr)
      }
    }
    case `tSub` => {
      val op = tSub.toString
      s eat tSub
      val prefixInh = inh.prefixExprBody(attr, op)  
      s.skip()
      val prefixAll = parseExp0(prefixInh)
      syn.prefixExpr(op, prefixAll, attr)
    }
    case `tNot` => {
      val op = tNot.toString
      s eat tNot
      val prefixInh = inh.prefixExprBody(attr, op)  
      s.skip()
      val prefixAll = parseExp0(prefixInh)
      syn.prefixExpr(op, prefixAll, attr)
    }
    case _ => sys error "[error] expected expression"
  })

  // binaryOpExpr * / && %
  private def parseExp1(attr: IA)(implicit s: TokenStream): IA with SA = {
    var lhs = parseExp0(attr)
    var _attr: IA = attr
    s.skip();

    while (List(tMul, tDiv, tAnd, tMod) contains s.token) {
      val op = s.token
      val rhsInh = inh.binaryOpExprRhs(_attr, lhs, op.toString)
      s.eat;
      s.skip();
      lhs = compose(rhsInh, syn.binaryOpExpr(lhs, op.toString, parseExp0(rhsInh), _attr))
      _attr = rhsInh
    }
    lhs
  }

  // binaryOpExpr + - ||
  private def parseExp2(attr: IA)(implicit s: TokenStream): IA with SA = {
    var lhs = parseExp1(attr)
    var _attr: IA = attr
    s.skip()

    while (List(tAdd, tSub, tOr) contains s.token) {
      val op = s.token
      val rhsInh = inh.binaryOpExprRhs(_attr, lhs, op.toString)
      s.eat;
      s.skip();
      lhs = compose(rhsInh, syn.binaryOpExpr(lhs, op.toString, parseExp1(rhsInh), _attr))
      _attr = rhsInh
    }
    lhs
  }

  // binaryOpExpr == != < > <= >=
  private def parseExp3(attr: IA)(implicit s: TokenStream): IA with SA = {

    var lhs = parseExp2(attr)
    var _attr: IA = attr
    s.skip()

    while (List(tEq, tNE, tLT, tGT, tLEq, tGEq) contains s.token) {
      val op = s.token
      val rhsInh = inh.binaryOpExprRhs(_attr, lhs, op.toString)
      s.eat;
      s.skip();
      lhs = compose(rhsInh, syn.binaryOpExpr(lhs, op.toString, parseExp2(rhsInh), _attr))
      _attr = rhsInh
    }
    lhs
  }
}
object ExpressionParser {
  import Expression._
  def apply[ArgsSA, ArgsIA, SA, IA](
    as: => ParsingAlgebra[List[trees.Expression], ArgsSA, ArgsIA],
    s: Syn.Complete[ArgsSA, SA, IA], 
    i: Inh.Complete[ArgsIA, SA, IA])(implicit c: Compose[IA, SA]) =
      new ExpressionParser[ArgsSA, ArgsIA, SA, IA] { 
        val syn = s; val inh = i; val compose = c; def args = as
      }
}