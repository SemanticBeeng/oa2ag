package oa2ag
package czero
package examples.prettyprinter

object attributes {

  import algebras._
  import Statement.ops._

  trait HasNothing
  object HasNothing extends HasNothing

  trait HasStmtPos extends HasNothing { val stmtPos: Int }
  object HasStmtPos { def apply(pos: Int) = new HasStmtPos { val stmtPos = pos } }
  trait HasExprPos extends HasNothing { val exprPos: Int }
  object HasExprPos { def apply(pos: Int) = new HasExprPos { val exprPos = pos } }
  trait HasPP extends HasNothing { val pp: String }
  object HasPP { def apply(s: String) = new HasPP { val pp = s } }

  // ignore any
  implicit def compAnyR[T]: Compose[T, Any] = new Compose[T, Any] {
    def apply(a: T, b: Any): T with Any = a
  }
  implicit def compAnyL[T]: Compose[Any, T] = new Compose[Any, T] {
    def apply(a: Any, b: T): Any with T = b
  }

  object ArgListPP extends ConsList.Syn.Complete[HasPP, HasPP, Any] {
    def empty(self: Self): Out = HasPP("")
    def cons(head: Elem, tail: Child, self: Self): Out = 
      if (tail.pp == "") head else HasPP(s"${head.pp}, ${tail.pp}")
  }
  object ExprPP extends Expression.Syn.Complete[HasPP, HasPP, Any] {
    def constExpr(value: Int, self: Self): Out = HasPP(s"const($value)")
    def idExpr(id: String, self: Self): Out = HasPP(s"id($id)")
    def prefixExpr(op: String, body: Child, self: Self): Out = HasPP(s"$op(${body.pp})")
    def callExpr(fun: String, args: Args, self: Self): Out = HasPP(s"""$fun(${args.pp})""")
    def binaryOpExpr(lhs: Child, op: String, rhs: Child, self: Self): Out = HasPP(s"(${lhs.pp} $op ${rhs.pp})")
  }

  lazy val exprPrettyPrinter: Algebra[trees.Expression, HasPP, Any] = 
    Expression.applyDefault(ConsList.applyDefault(exprPrettyPrinter, ArgListPP), ExprPP)

  def prettyExp(e: trees.Expression): String = exprPrettyPrinter(e, HasNothing).pp

  object StmtPP extends Statement.Syn.Complete[HasPP, HasPP, HasPP, Any] {
    def assignStmt(id: String, value: Expr, self: Self): Out = HasPP(s"$id = ${value.pp}")
    def whileStmt(cond: Expr, body: Stmts, self: Self): Out = HasPP(s"while(${cond.pp}) { ${body.pp} }")
    def putCharStmt(char: Expr, self: Self): Out = HasPP(s"putchar(${char.pp})")
    def returnStmt(value: Expr, self: Self): Out = HasPP(s"return ${value.pp}")
    def ifStmt(cond: Expr, then: Stmts, els: Stmts, self: Self): Out = HasPP(s"if(${cond.pp}) { ${then.pp} } else { ${els.pp} }")
  }

  object StmtListPP extends ConsList.Syn.Complete[HasPP, HasPP, Any] {
    def empty(self: Self): Out = HasPP("")
    def cons(head: Elem, tail: Child, self: Self): Out = HasPP(s"${head.pp};\n${tail.pp}")
  }

  lazy val stmtPrettyPrinter: Statement[HasPP, Any, HasPP, Any, HasPP, Any] = 
    Statement.applyDefault(exprPrettyPrinter, prettyPrinter, StmtPP)

  lazy val prettyPrinter = ConsList.applyDefault(stmtPrettyPrinter, StmtListPP)

  def pretty(s: List[trees.Statement]): String = prettyPrinter(s, HasNothing).pp

  object StmtsPos extends ConsList.Inh[Any, HasStmtPos, Any, HasStmtPos, HasStmtPos] {
    def consHead(parent: IA): ElemIA = parent
    def consTail(parent: IA, head: ElemIA with ElemSA): Out = HasStmtPos(head.stmtPos + 1)
  }

  // Improve the pretty printer
  object StmtPPImproved extends Statement.Syn[HasPP, HasPP, HasPP, HasPP, HasStmtPos, HasPP] {
    def assignStmt(id: String, value: Expr, self: Self): Out = HasPP(s"${self.stmtPos}: ${self.pp}")
    def whileStmt(cond: Expr, body: Stmts, self: Self): Out = HasPP(s"${self.stmtPos}: ${self.pp}")
    def putCharStmt(char: Expr, self: Self): Out = HasPP(s"${self.stmtPos}: ${self.pp}")
    def returnStmt(value: Expr, self: Self): Out = HasPP(s"${self.stmtPos}: ${self.pp}")
    def ifStmt(cond: Expr, then: Stmts, els: Stmts, self: Self): Out = HasPP(s"${self.stmtPos}: ${self.pp}")
  }

  lazy val imprStmtPrettyPrinter: Statement[HasPP, Any, HasPP, HasStmtPos, HasPP, HasStmtPos] = 
    Statement.applyDefault(
      exprPrettyPrinter,
      imprPrettyPrinter,
      StmtPP <+ StmtPPImproved)

  lazy val imprPrettyPrinter = ConsList(imprStmtPrettyPrinter, StmtListPP, StmtsPos)

  def pretty2(s: List[trees.Statement]): String = imprPrettyPrinter(s, HasStmtPos(1)).pp

}

object test {
  import trees._
  import attributes._

  val expr = BinaryOpExpr(IdExpr("a"), "+", CallExpr("exp", List(IdExpr("a"), IdExpr("b"))))

  println(prettyExp(expr))


  val prog = List(
    AssignStmt("c", BinaryOpExpr(IdExpr("a"), "+", IdExpr("b"))),
    AssignStmt("d", BinaryOpExpr(IdExpr("c"), "+", IdExpr("c"))),
    ReturnStmt(IdExpr("c")))

  println(pretty(prog))
  println(pretty2(prog))

}
