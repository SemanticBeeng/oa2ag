package oa2ag
package czero

object algebras {

  trait Expression[ArgsSA, ArgsIA, SA, IA] extends Algebra[trees.Expression, SA, IA] {

    import trees.{ Expression => _, _ }

    val syn: Expression.Syn.Complete[ArgsSA, SA, IA]
    val inh: Expression.Inh.Complete[ArgsIA, SA, IA]

    def args: Algebra[List[trees.Expression], ArgsSA, ArgsIA]

    // fold is now called `apply`
    def apply(t: trees.Expression, attr: IA): SA = {
      import trees._
      t match {
        case ConstExpr(value) => syn.constExpr(value, attr)
        case IdExpr(id) => syn.idExpr(id, attr)
        case PrefixExpr(op, body) => 
          val prefixInh = inh.prefixExprBody(attr, op)
          val prefixAll = compose(prefixInh, this(body, prefixInh))
          syn.prefixExpr(op, prefixAll, attr)
        
        case CallExpr(funName, as) => {
          val callInh = inh.callExprArgs(attr, funName)
          val callAll = args.compose(callInh, args(as, callInh))
          syn.callExpr(funName, callAll, attr)
        }

        case BinaryOpExpr(lhs, op, rhs) => {
          val binOpLhsInh = inh.binaryOpExprLhs(attr, op)
          val binOpLhsAll = compose(binOpLhsInh, this(lhs, binOpLhsInh))
          val binOpRhsInh = inh.binaryOpExprRhs(attr, binOpLhsAll, op)
          val binOpRhsAll = compose(binOpRhsInh, this(rhs, binOpRhsInh))
          syn.binaryOpExpr(binOpLhsAll, op, binOpRhsAll, attr)
        }
      }
    }
  }
  object Expression {

    trait Syn[-ArgsT, -Req, -Prov, -IA, +OutT] extends Algebra.Syn[Req, Prov, IA, OutT] {
      type Args = ArgsT
      def constExpr(value: Int, self: Self): Out
      def idExpr(id: String, self: Self): Out
      def prefixExpr(op: String, body: Child, self: Self): Out
      def callExpr(fun: String, args: Args, self: Self): Out
      def binaryOpExpr(lhs: Child, op: String, rhs: Child, self: Self): Out
    }
    object Syn {
      type Attr[Args, Req, Prov, IA] = Syn[Args, Req, Prov, IA, Prov]
      type Complete[Args, SA, IA] = Syn[Args, Any, SA, IA, SA]

      def composeAlg[
        Args1, Req1, Prov1, IA1, Out1, 
        Args2, Req2 >: Out1, Prov2, IA2, Out2](
          alg1: Syn[Args1, Req1, Prov1, IA1, Out1],
          alg2: Syn[Args2, Req2, Prov2, IA2, Out2])(implicit
          comp: Compose[Req1 with IA1 with IA2, Req2],
          comp2: Compose[Out1, Out2]) =
            // => still requires Req1, but since Req2 >: Out1 this can be fulfilled by alg1
        new Syn[Args1 with Args2, Req1, Prov1 with Prov2 with Req2, IA1 with IA2, Out1 with Out2] {
          private def assemble(f: Self => Out1, g: comp.Out => Out2, self: Self): comp2.Out = {
            val out = f(self)
            comp2(out, g(comp(self, out)))
          }

          def constExpr(value: Int, self: Self): Out = 
            assemble(alg1.constExpr(value, _), alg2.constExpr(value, _), self)

          def idExpr(id: String, self: Self): Out =
            assemble(alg1.idExpr(id, _), alg2.idExpr(id, _), self)

          def prefixExpr(op: String, body: Child, self: Self): Out =
            assemble(alg1.prefixExpr(op, body, _), alg2.prefixExpr(op, body, _), self)

          def callExpr(fun: String, args: Args, self: Self): Out =
            assemble(alg1.callExpr(fun, args, _), alg2.callExpr(fun, args, _), self)

          def binaryOpExpr(lhs: Child, op: String, rhs: Child, self: Self): Out =
            assemble(alg1.binaryOpExpr(lhs, op, rhs, _), alg2.binaryOpExpr(lhs, op, rhs, _), self)
        }
    }
    
    trait Inh[ArgsIAT, -SA, -IA, +Out] extends Algebra.Inh[SA, IA, Out] {

      type ArgsIA = ArgsIAT

      def prefixExprBody(parent: IA, op: String): Out
      def callExprArgs(parent: IA, fun: String): ArgsIA
      def binaryOpExprLhs(parent: IA, op: String): Out
      def binaryOpExprRhs(parent: IA, lhs: IA with SA, op: String): Out
    }
    object Inh {
      type Complete[ArgsIA, SA, IA] = Inh[ArgsIA, SA, IA, IA]
      
      trait Default[ArgsIA >: IA, -SA, -IA, +Out >: IA] extends Inh[ArgsIA, SA, IA, Out] {
        def prefixExprBody(parent: IA, op: String): Out = parent
        def callExprArgs(parent: IA, fun: String): ArgsIA = parent
        def binaryOpExprLhs(parent: IA, op: String): Out = parent
        def binaryOpExprRhs(parent: IA, lhs: IA with SA, op: String): Out = parent
      }
      object Default {
        def apply[ArgsIA >: IA, SA, IA] = new Default[ArgsIA, SA, IA, IA] {}
      }

      def composeAlg[ArgsIA1, SA1, IA1, Out1, ArgsIA2, SA2, IA2, Out2](
          alg1: Inh[ArgsIA1, SA1, IA1, Out1],
          alg2: Inh[ArgsIA2, SA2, IA2, Out2])(
            implicit 
              comp: Compose[Out1, Out2],
              comp2: Compose[ArgsIA1, ArgsIA2]) = 
        new Inh[ArgsIA1 with ArgsIA2, SA1 with SA2, IA1 with IA2, Out1 with Out2] {
          override def prefixExprBody(parent: IA, op: String): Out =
            comp(alg1.prefixExprBody(parent, op), alg2.prefixExprBody(parent, op))

          override def callExprArgs(parent: IA, fun: String): ArgsIA1 with ArgsIA2 =
            comp2(alg1.callExprArgs(parent, fun), alg2.callExprArgs(parent, fun))

          override def binaryOpExprLhs(parent: IA, op: String): Out =
            comp(alg1.binaryOpExprLhs(parent, op), alg2.binaryOpExprLhs(parent, op))

          override def binaryOpExprRhs(parent: IA, lhs: IA with SA, op: String): Out =
            comp(alg1.binaryOpExprRhs(parent, lhs, op), alg2.binaryOpExprRhs(parent, lhs, op))
        }
    }

    object ops {
      implicit class ExpressionSynOps[Args1, Req1, Prov1, IA1, Out1](self: Syn[Args1, Req1, Prov1, IA1, Out1]) {
        def <+[Args2, Req2 >: Out1, Prov2, IA2, Out2](other: Syn[Args2, Req2, Prov2, IA2, Out2])(
            implicit comp: Compose[Req1 with IA1 with IA2, Req2], comp2: Compose[Out1, Out2]) =
          Syn.composeAlg(self, other)
      }

      implicit class ExpressionInhOps[ArgsIA1, SA1, IA1, Out1](self: Inh[ArgsIA1, SA1, IA1, Out1]) {
        def <+[ArgsIA2, SA2, IA2, Out2](other: Inh[ArgsIA2, SA2, IA2, Out2])(
          implicit 
            comp: Compose[Out1, Out2],
            comp2: Compose[ArgsIA1, ArgsIA2]) = Inh.composeAlg(self, other)
      }
    }    

    def apply[ArgsSA, ArgsIA, SA, IA](
      as: => Algebra[List[trees.Expression], ArgsSA, ArgsIA],
      s: Syn.Complete[ArgsSA, SA, IA], 
      i: Inh.Complete[ArgsIA, SA, IA])(implicit c: Compose[IA, SA]) =
        new Expression[ArgsSA, ArgsIA, SA, IA] { 
          val syn = s; val inh = i; val compose = c
          def args = as
        }

    def applyDefault[ArgsSA, ArgsIA >: IA, SA, IA](
      as: => Algebra[List[trees.Expression], ArgsSA, ArgsIA],
      s: Syn.Complete[ArgsSA, SA, IA], 
      i: Inh.Complete[ArgsIA, SA, IA] = Inh.Default[ArgsIA, SA, IA])(implicit c: Compose[IA, SA]) =
        apply(as, s, i)
  }


  trait Statement[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] extends Algebra[trees.Statement, SA, IA] {
 
    val syn: Statement.Syn.Complete[ExprSA, StmtsSA, SA, IA]
    val inh: Statement.Inh.Complete[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA]

    def expr: Algebra[trees.Expression, ExprSA, ExprIA]
    def stmts: Algebra[List[trees.Statement], StmtsSA, StmtsIA]

    def apply(t: trees.Statement, attr: IA): SA = {
      import trees._

      t match {
        case AssignStmt(id, value) => {
          val assignInh = inh.assignStmtValue(attr, id)
          val assignAll = expr.compose(assignInh, expr(value, assignInh))
          syn.assignStmt(id, assignAll, attr)
        }
        case WhileStmt(cond, body) => {
          val condInh = inh.whileStmtCond(attr)
          val condAll = expr.compose(condInh, expr(cond, condInh))
          val bodyInh = inh.whileStmtBody(attr, condAll)
          val bodyAll = stmts.compose(bodyInh, stmts(body, bodyInh))
          syn.whileStmt(condAll, bodyAll, attr)
        }
        case PutCharStmt(char) => {
          val charInh = inh.putCharStmtChar(attr)
          val charAll = expr.compose(charInh, expr(char, charInh))
          syn.putCharStmt(charAll, attr)
        }
        case ReturnStmt(value) => {
          val valueInh = inh.returnStmtValue(attr)
          val valueAll = expr.compose(valueInh, expr(value, valueInh))
          syn.returnStmt(valueAll, attr)
        }
        case IfStmt(cond, then, els) => {
          val condInh = inh.ifStmtCond(attr)
          val condAll = expr.compose(condInh, expr(cond, condInh))
          val thenInh = inh.ifStmtThen(attr, condAll)
          val thenAll = stmts.compose(thenInh, stmts(then, thenInh))
          val elseInh = inh.ifStmtElse(attr, condAll, thenAll)
          val elseAll = stmts.compose(elseInh, stmts(els, elseInh))
          syn.ifStmt(condAll, thenAll, elseAll, attr)
        }
      }
    }
  }
  object Statement {

    trait Syn[-ExprT, -StmtsT, -Req, -Prov, -IA, +OutT] extends Algebra.Syn[Req, Prov, IA, OutT] {
      
      type Expr = ExprT
      type Stmts = StmtsT

      def assignStmt(id: String, value: Expr, self: Self): Out
      def whileStmt(cond: Expr, body: Stmts, self: Self): Out
      def putCharStmt(char: Expr, self: Self): Out
      def returnStmt(value: Expr, self: Self): Out
      def ifStmt(cond: Expr, then: Stmts, els: Stmts, self: Self): Out
    }
    object Syn {
      type Attr[Expr, Stmts, Req, Prov, IA] = Syn[Expr, Stmts, Req, Prov, IA, Prov]
      type Complete[Expr, Stmts, SA, IA] = Syn[Expr, Stmts, Any, SA, IA, SA]

      def composeAlg[
        Expr1, Stmts1, Req1, Prov1, IA1, Out1, 
        Expr2, Stmts2, Req2 >: Out1, Prov2, IA2, Out2](
          alg1: Syn[Expr1, Stmts1, Req1, Prov1, IA1, Out1],
          alg2: Syn[Expr2, Stmts2, Req2, Prov2, IA2, Out2])(implicit
          comp: Compose[Req1 with IA1 with IA2, Req2],
          comp2: Compose[Out1, Out2]) =
        new Syn[Expr1 with Expr2, Stmts1 with Stmts2, Req1, Prov1 with Prov2 with Req2, IA1 with IA2, Out1 with Out2] {

          private def assemble(f: Self => Out1, g: comp.Out => Out2, self: Self): comp2.Out = {
            val out = f(self)
            comp2(out, g(comp(self, out)))
          }

          def assignStmt(id: String, value: Expr, self: Self): Out =
            assemble(alg1.assignStmt(id, value, _), alg2.assignStmt(id, value, _), self)

          def whileStmt(cond: Expr, body: Stmts, self: Self): Out =
            assemble(alg1.whileStmt(cond, body, _), alg2.whileStmt(cond, body, _), self)

          def putCharStmt(char: Expr, self: Self): Out =
            assemble(alg1.putCharStmt(char, _), alg2.putCharStmt(char, _), self)

          def returnStmt(value: Expr, self: Self): Out =
            assemble(alg1.returnStmt(value, _), alg2.returnStmt(value, _), self)

          def ifStmt(cond: Expr, then: Stmts, els: Stmts, self: Self): Out =
            assemble(alg1.ifStmt(cond, then, els, _), alg2.ifStmt(cond, then, els, _), self)
        }
    }

    trait Inh[-ExprSAT, ExprIAT, -StmtsSAT, StmtsIAT, -SA, -IA, +Out] extends Algebra.Inh[SA, IA, Out] {

      type ExprIA = ExprIAT
      type ExprSA = ExprSAT
      type Expr = ExprIA with ExprSA

      type StmtsIA = StmtsIAT
      type StmtsSA = StmtsSAT
      type Stmts = StmtsIA with StmtsSA

      def assignStmtValue(parent: IA, id: String): ExprIA
      def whileStmtCond(parent: IA): ExprIA
      def whileStmtBody(parent: IA, cond: Expr): StmtsIA
      def putCharStmtChar(parent: IA): ExprIA
      def returnStmtValue(parent: IA): ExprIA
      def ifStmtCond(parent: IA): ExprIA
      def ifStmtThen(parent: IA, cond: Expr): StmtsIA
      def ifStmtElse(parent: IA, cond: Expr, then: Stmts): StmtsIA
    }
    object Inh {
      type Complete[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] = Inh[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA, IA]

      trait Default[ExprSA, ExprIA >: IA, StmtsSA, StmtsIA >: IA, SA, IA] extends Inh.Complete[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] {
        def assignStmtValue(parent: IA, id: String): ExprIA = parent
        def whileStmtCond(parent: IA): ExprIA = parent
        def whileStmtBody(parent: IA, cond: Expr): StmtsIA = parent
        def putCharStmtChar(parent: IA): ExprIA = parent
        def returnStmtValue(parent: IA): ExprIA = parent
        def ifStmtCond(parent: IA): ExprIA = parent
        def ifStmtThen(parent: IA, cond: Expr): StmtsIA = parent
        def ifStmtElse(parent: IA, cond: Expr, then: Stmts): StmtsIA = parent
      }
      object Default {
        def apply[ExprSA, ExprIA >: IA, StmtsSA, StmtsIA >: IA, SA, IA] = new Default[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] {}  
      }

      def composeAlg[
          ExprSA1, ExprIA1, StmtsSA1, StmtsIA1, SA1, IA1, Out1, 
          ExprSA2, ExprIA2, StmtsSA2, StmtsIA2, SA2, IA2, Out2](
        alg1: Inh[ExprSA1, ExprIA1, StmtsSA1, StmtsIA1, SA1, IA1, Out1],
        alg2: Inh[ExprSA2, ExprIA2, StmtsSA2, StmtsIA2, SA2, IA2, Out2])(
          implicit 
            comp: Compose[Out1, Out2],
            comp2: Compose[ExprIA1, ExprIA2],
            comp3: Compose[StmtsIA1, StmtsIA2]) = 
        new Inh[
            ExprSA1 with ExprSA2, ExprIA1 with ExprIA2, 
            StmtsSA1 with StmtsSA2, StmtsIA1 with StmtsIA2, 
            SA1 with SA2, IA1 with IA2, 
            Out1 with Out2] {
          def assignStmtValue(parent: IA, id: String): ExprIA = 
            comp2(alg1.assignStmtValue(parent, id), alg2.assignStmtValue(parent, id))

          def whileStmtCond(parent: IA): ExprIA = 
            comp2(alg1.whileStmtCond(parent), alg2.whileStmtCond(parent))

          def whileStmtBody(parent: IA, cond: Expr): StmtsIA = 
            comp3(alg1.whileStmtBody(parent, cond), alg2.whileStmtBody(parent, cond))

          def putCharStmtChar(parent: IA): ExprIA = 
            comp2(alg1.putCharStmtChar(parent), alg2.putCharStmtChar(parent))

          def returnStmtValue(parent: IA): ExprIA = 
            comp2(alg1.returnStmtValue(parent), alg2.returnStmtValue(parent))

          def ifStmtCond(parent: IA): ExprIA = 
            comp2(alg1.ifStmtCond(parent), alg2.ifStmtCond(parent))

          def ifStmtThen(parent: IA, cond: Expr): StmtsIA = 
            comp3(alg1.ifStmtThen(parent, cond), alg2.ifStmtThen(parent, cond))

          def ifStmtElse(parent: IA, cond: Expr, then: Stmts): StmtsIA = 
            comp3(alg1.ifStmtElse(parent, cond, then), alg2.ifStmtElse(parent, cond, then))
        }
    }

    object ops {
      implicit class StatementSynOps[Expr1, Stmts1, Req1, Prov1, IA1, Out1](self: Syn[Expr1, Stmts1, Req1, Prov1, IA1, Out1]) {
        def <+[Expr2, Stmts2, Req2 >: Out1, Prov2, IA2, Out2](other: Syn[Expr2, Stmts2, Req2, Prov2, IA2, Out2])(
            implicit comp: Compose[Req1 with IA1 with IA2, Req2], comp2: Compose[Out1, Out2]) =
          Syn.composeAlg(self, other)
      }

      implicit class StatementInhOps[
          ExprSA1, ExprIA1, 
          StmtsSA1, StmtsIA1, 
          SA1, IA1, Out1](self: Inh[ExprSA1, ExprIA1, StmtsSA1, StmtsIA1, SA1, IA1, Out1]) {
        def <+[
            ExprSA2, ExprIA2, 
            StmtsSA2, StmtsIA2, 
            SA2, IA2, Out2](other: Inh[ExprSA2, ExprIA2, StmtsSA2, StmtsIA2, SA2, IA2, Out2])(
          implicit 
            comp: Compose[Out1, Out2],
            comp2: Compose[ExprIA1, ExprIA2],
            comp3: Compose[StmtsIA1, StmtsIA2]) = Inh.composeAlg(self, other)
      }
    }

    def apply[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA](
        e: => Algebra[trees.Expression, ExprSA, ExprIA],
        ss: => Algebra[List[trees.Statement], StmtsSA, StmtsIA],
        s: Syn.Complete[ExprSA, StmtsSA, SA, IA], 
        i: Inh.Complete[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA])(implicit c: Compose[IA, SA]) =
      new Statement[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] { 
        def expr = e; def stmts = ss; val syn = s; val inh = i; val compose = c
      }

    def applyDefault[ExprSA, ExprIA >: IA, StmtsSA, StmtsIA >: IA, SA, IA](
        e: => Algebra[trees.Expression, ExprSA, ExprIA],
        ss: => Algebra[List[trees.Statement], StmtsSA, StmtsIA],
        s: Syn.Complete[ExprSA, StmtsSA, SA, IA], 
        i: Inh.Complete[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA] = 
          Inh.Default[ExprSA, ExprIA, StmtsSA, StmtsIA, SA, IA])(implicit c: Compose[IA, SA]) =
      apply(e, ss, s, i)
  }


  trait Declaration[SA, IA] extends Algebra[trees.Declaration, SA, IA] {

    val syn: Declaration.Syn.Complete[SA, IA]
    val inh = new Algebra.Inh.Complete[SA, IA] {} 
                  // Since Declaration currently does not have an nonterminal
                  // occurrence on the rhs, there is no need for an inh. alg.

    def apply(t: trees.Declaration, attr: IA): SA = t match {
      case trees.IntDecl(id, init) => syn.intDecl(id, init, attr)
    }
  }
  object Declaration {
    trait Syn[-Req, -Prov, -IA, +OutT] extends Algebra.Syn[Req, Prov, IA, OutT] {
      def intDecl(id: String, init: Option[Int], self: Self): Out
    }
    object Syn {
      type Attr[Req, Prov, IA] = Syn[Req, Prov, IA, Prov]
      type Complete[SA, IA] = Syn[Any, SA, IA, SA]

      def composeAlg[
        Req1, Prov1, IA1, Out1, 
        Req2 >: Out1, Prov2, IA2, Out2](
          alg1: Syn[Req1, Prov1, IA1, Out1],
          alg2: Syn[Req2, Prov2, IA2, Out2])(implicit
          comp: Compose[Req1 with IA1 with IA2, Req2],
          comp2: Compose[Out1, Out2]) =
        new Syn[Req1, Prov1 with Prov2 with Req2, IA1 with IA2, Out1 with Out2] {
          private def assemble(f: Self => Out1, g: comp.Out => Out2, self: Self): comp2.Out = {
            val out = f(self)
            comp2(out, g(comp(self, out)))
          }

          def intDecl(id: String, init: Option[Int], self: Self): Out =
            assemble(alg1.intDecl(id, init, _), alg2.intDecl(id, init, _), self)
        }
    }

    object ops {
      implicit class DeclarationSynOps[Req1, Prov1, IA1, Out1](self: Syn[Req1, Prov1, IA1, Out1]) {
        def <+[pr2, Req2 >: Out1, Prov2, IA2, Out2](other: Syn[Req2, Prov2, IA2, Out2])(
            implicit comp: Compose[Req1 with IA1 with IA2, Req2], comp2: Compose[Out1, Out2]) =
          Syn.composeAlg(self, other)
      }
    }

    def apply[SA, IA](s: Syn.Complete[SA, IA])(implicit c: Compose[IA, SA]) =
      new Declaration[SA, IA] { val syn = s; val compose = c }
  }


  trait Function[DeclsSA, DeclsIA, StmtsSA, StmtsIA, SA, IA] extends Algebra[trees.Function, SA, IA] {

    import trees.{ Function => _, Declaration => _, Statement => _,_ }

    val syn: Function.Syn.Complete[DeclsSA, StmtsSA, SA, IA]
    val inh: Function.Inh.Complete[DeclsSA, DeclsIA, StmtsIA, SA, IA]

    def decls: Algebra[List[trees.Declaration], DeclsSA, DeclsIA]
    def stmts: Algebra[List[trees.Statement], StmtsSA, StmtsIA]

    def apply(t: trees.Function, attr: IA): SA = t match {
      case FunDecl(id, formals) => syn.funDecl(id, formals, attr)
      case FunDef(id, formals, ds, ss) => {
        val funDefDsInh = inh.funDefDecls(attr, id, formals)
        val funDefDsAll = decls.compose(funDefDsInh, decls(ds, funDefDsInh))
        val funDefSsInh = inh.funDefStmts(attr, id, formals, funDefDsAll)
        val funDefSsAll = stmts.compose(funDefSsInh, stmts(ss, funDefSsInh))
        syn.funDef(id, formals, funDefDsAll, funDefSsAll, attr)
      }
    }
  }
  object Function {
    trait Syn[-DeclsT, -StmtsT, -Req, -Prov, -IA, +OutT] extends Algebra.Syn[Req, Prov, IA, OutT] {
      type Decls = DeclsT
      type Stmts = StmtsT

      def funDecl(id: String, formals: List[String], self: Self): Out
      def funDef(id: String, formals: List[String], decls: Decls, stmts: Stmts, self: Self): Out
    }
    object Syn {
      type Attr[Decls, Stmts, Req, Prov, IA] = Syn[Decls, Stmts, Req, Prov, IA, Prov]
      type Complete[Decls, Stmts, SA, IA] = Syn[Decls, Stmts, Any, SA, IA, SA]

      def composeAlg[
        Decls1, Stmts1, Req1, Prov1, IA1, Out1, 
        Decls2, Stmts2, Req2 >: Out1, Prov2, IA2, Out2](
          alg1: Syn[Decls1, Stmts1, Req1, Prov1, IA1, Out1],
          alg2: Syn[Decls2, Stmts2, Req2, Prov2, IA2, Out2])(implicit
          comp: Compose[Req1 with IA1 with IA2, Req2],
          comp2: Compose[Out1, Out2]) =
            // => still requires Req1, but since Req2 >: Out1 this can be fulfilled by alg1
        new Syn[Decls1 with Decls2, Stmts1 with Stmts2, Req1, Prov1 with Prov2 with Req2, IA1 with IA2, Out1 with Out2] {
          private def assemble(f: Self => Out1, g: comp.Out => Out2, self: Self): comp2.Out = {
            val out = f(self)
            comp2(out, g(comp(self, out)))
          }

          def funDecl(id: String, formals: List[String], self: Self): Out =
            assemble(alg1.funDecl(id, formals, _), alg2.funDecl(id, formals, _), self)

          def funDef(id: String, formals: List[String], decls: Decls, stmts: Stmts, self: Self): Out =
            assemble(alg1.funDef(id, formals, decls, stmts, _), alg2.funDef(id, formals, decls, stmts, _), self)
        }
    }
    
    trait Inh[-DeclsSAT, DeclsIAT, StmtsIAT, -SA, -IA, +Out] extends Algebra.Inh[SA, IA, Out] {
      type DeclsSA = DeclsSAT
      type DeclsIA = DeclsIAT
      type StmtsIA = StmtsIAT
      type Decls = DeclsIA with DeclsSA

      def funDefDecls(parent: IA, id: String, formals: List[String]): DeclsIA
      def funDefStmts(parent: IA, id: String, formals: List[String], decls: DeclsIA with DeclsSA): StmtsIA
    }
    object Inh {
      type Complete[DeclsSA, DeclsIA, StmtsIA, SA, IA] = Inh[DeclsSA, DeclsIA, StmtsIA, SA, IA, IA]

      trait Default[DeclsSA, DeclsIA >: IA, StmtsIA >: IA, SA, IA, +Out >: IA] extends Inh[DeclsSA, DeclsIA, StmtsIA, SA, IA, Out] {
        def funDefDecls(parent: IA, id: String, formals: List[String]): DeclsIA = parent
        def funDefStmts(parent: IA, id: String, formals: List[String], decls: Decls): StmtsIA = parent
      }
      object Default {
        def apply[DeclsSA, DeclsIA >: IA, StmtsIA >: IA, SA, IA] = new Default[DeclsSA, DeclsIA, StmtsIA, SA, IA, IA] {}
      }

      def composeAlg[
        DeclsSA1, DeclsIA1, StmtsIA1, SA1, IA1, Out1, 
        DeclsSA2, DeclsIA2, StmtsIA2, SA2, IA2, Out2](
          alg1: Inh[DeclsSA1, DeclsIA1, StmtsIA1, SA1, IA1, Out1],
          alg2: Inh[DeclsSA2, DeclsIA2, StmtsIA2, SA2, IA2, Out2])(
            implicit 
              comp: Compose[DeclsIA1, DeclsIA2],
              comp2: Compose[StmtsIA1, StmtsIA2]) = 
        new Inh[DeclsSA1 with DeclsSA2, DeclsIA1 with DeclsIA2, StmtsIA1 with StmtsIA2, SA1 with SA2, IA1 with IA2, Out1 with Out2] {
          override def funDefDecls(parent: IA, id: String, formals: List[String]): DeclsIA =
            comp(alg1.funDefDecls(parent, id, formals), alg2.funDefDecls(parent, id, formals))

          override def funDefStmts(parent: IA, id: String, formals: List[String], decls: Decls): StmtsIA = 
            comp2(alg1.funDefStmts(parent, id, formals, decls), alg2.funDefStmts(parent, id, formals, decls))
        }
    }

    object ops {
      implicit class FunctionSynOps[Decls1, Stmts1, Req1, Prov1, IA1, Out1](self: Syn[Decls1, Stmts1, Req1, Prov1, IA1, Out1]) {
        def <+[Decls2, Stmts2, Req2 >: Out1, Prov2, IA2, Out2](other: Syn[Decls2, Stmts2, Req2, Prov2, IA2, Out2])(
            implicit comp: Compose[Req1 with IA1 with IA2, Req2], comp2: Compose[Out1, Out2]) =
          Syn.composeAlg(self, other)
      }

      implicit class FunctionInhOps[DeclsSA1, DeclsIA1, StmtsIA1, SA1, IA1, Out1](self: Inh[DeclsSA1, DeclsIA1, StmtsIA1, SA1, IA1, Out1]) {
        def <+[DeclsSA2, DeclsIA2, StmtsIA2, SA2, IA2, Out2](other: Inh[DeclsSA2, DeclsIA2, StmtsIA2, SA2, IA2, Out2])(
            implicit comp: Compose[DeclsIA1, DeclsIA2], comp2: Compose[StmtsIA1, StmtsIA2]) = 
          Inh.composeAlg(self, other)
      }
    }

    def apply[DeclsSA, DeclsIA, StmtsSA, StmtsIA, SA, IA](
      ds: => Algebra[List[trees.Declaration], DeclsSA, DeclsIA],
      ss: => Algebra[List[trees.Statement], StmtsSA, StmtsIA],
      s: Syn.Complete[DeclsSA, StmtsSA, SA, IA], 
      i: Inh.Complete[DeclsSA, DeclsIA, StmtsIA, SA, IA])(implicit c: Compose[IA, SA]) =
        new Function[DeclsSA, DeclsIA, StmtsSA, StmtsIA, SA, IA] { 
          val syn = s; val inh = i; val compose = c; def decls = ds; def stmts = ss
        }

    def applyDefault[DeclsSA, DeclsIA >: IA, StmtsSA, StmtsIA >: IA, SA, IA](
      ds: => Algebra[List[trees.Declaration], DeclsSA, DeclsIA],
      ss: => Algebra[List[trees.Statement], StmtsSA, StmtsIA],
      s: Syn.Complete[DeclsSA, StmtsSA, SA, IA], 
      i: Inh.Complete[DeclsSA, DeclsIA, StmtsIA, SA, IA] = Inh.Default[DeclsSA, DeclsIA, StmtsIA, SA, IA])(implicit c: Compose[IA, SA]) =
        apply(ds, ss, s, i)
  }

  trait Program[FuncsSA, FuncsIA, SA, IA] extends Algebra[trees.Program, SA, IA] {

    val syn: Program.Syn.Complete[FuncsSA, SA, IA]
    val inh: Program.Inh.Complete[FuncsIA, SA, IA]

    def funcs: Algebra[List[trees.Function], FuncsSA, FuncsIA]

    def apply(t: trees.Program, attr: IA): SA = t match {
      case trees.Prog(fs) => {
        val progInh = inh.progFuncs(attr)
        val progAll = funcs.compose(progInh, funcs(fs, progInh))
        syn.prog(progAll, attr)
      }
    }
  }
  object Program {
    trait Syn[-Funcs, -Req, -Prov, -IA, +OutT] extends Algebra.Syn[Req, Prov, IA, OutT] {
      type FuncList = Funcs
      def prog(funcs: FuncList, self: Self): Out
    }
    object Syn {
      type Attr[Funcs, Req, Prov, IA] = Syn[Funcs, Req, Prov, IA, Prov]
      type Complete[Funcs, SA, IA] = Syn[Funcs, Any, SA, IA, SA]

      def composeAlg[
        Funcs1, Req1, Prov1, IA1, Out1, 
        Funcs2, Req2 >: Out1, Prov2, IA2, Out2](
          alg1: Syn[Funcs1, Req1, Prov1, IA1, Out1],
          alg2: Syn[Funcs2, Req2, Prov2, IA2, Out2])(implicit
          comp: Compose[Req1 with IA1 with IA2, Req2],
          comp2: Compose[Out1, Out2]) =
        new Syn[Funcs1 with Funcs2, Req1, Prov1 with Prov2 with Req2, IA1 with IA2, Out1 with Out2] {
          private def assemble(f: Self => Out1, g: comp.Out => Out2, self: Self): comp2.Out = {
            val out = f(self)
            comp2(out, g(comp(self, out)))
          }

          def prog(funcs: FuncList, self: Self): Out =
            assemble(alg1.prog(funcs, _), alg2.prog(funcs, _), self)
        }
    }

    // We even do not need `SA` and `Out` here! maybe refactor to
    // Inh[FuncsIA, -IA] extends Algebra.Inh[Any, IA, Any]
    trait Inh[FuncsIAT, -SA, -IA, +Out] extends Algebra.Inh[SA, IA, Out] {
      type FuncsIA = FuncsIAT
      def progFuncs(parent: IA): FuncsIA
    }
    object Inh {
      type Complete[FuncsIA, SA, IA] = Inh[FuncsIA, SA, IA, IA]
      trait Default[FuncsIA >: IA, SA, IA, Out >: IA] extends Inh[FuncsIA, SA, IA, Out] {
        def progFuncs(parent: IA): FuncsIA = parent
      }
      object Default {
        def apply[FuncsIA >: IA, SA, IA] = new Default[FuncsIA, SA, IA, IA] {}
      }
      def composeAlg[FuncsIA1, SA1, IA1, Out1 >: IA1, FuncsIA2, SA2, IA2, Out2 >: IA2](
          alg1: Inh[FuncsIA1, SA1, IA1, Out1],
          alg2: Inh[FuncsIA2, SA2, IA2, Out2]
        )(implicit comp: Compose[FuncsIA1, FuncsIA2]) = 
        new Inh[FuncsIA1 with FuncsIA2, SA1 with SA2, IA1 with IA2, Out1 with Out2] {
          override def progFuncs(parent: IA): FuncsIA =
            comp(alg1.progFuncs(parent), alg2.progFuncs(parent))
        }
    }

    object ops {
      implicit class ProgramSynOps[Funcs1, Req1, Prov1, IA1, Out1](self: Syn[Funcs1, Req1, Prov1, IA1, Out1]) {
        def <+[Funcs2, Req2 >: Out1, Prov2, IA2, Out2](other: Syn[Funcs2, Req2, Prov2, IA2, Out2])(
            implicit comp: Compose[Req1 with IA1 with IA2, Req2], comp2: Compose[Out1, Out2]) =
          Syn.composeAlg(self, other)
      }

      implicit class ProgramInhOps[FuncsIA1, SA1, IA1, Out1](self: Inh[FuncsIA1, SA1, IA1, Out1]) {
        def <+[FuncsIA2, SA2, IA2, Out2](other: Inh[FuncsIA2, SA2, IA2, Out2])(
          implicit comp: Compose[FuncsIA1, FuncsIA2]) = Inh.composeAlg(self, other)
      }
    }

    def apply[FuncsSA, FuncsIA, SA, IA](
      fs: => Algebra[List[trees.Function], FuncsSA, FuncsIA],
      s: Syn.Complete[FuncsSA, SA, IA], 
      i: Inh.Complete[FuncsIA, SA, IA])(implicit c: Compose[IA, SA]) =
        new Program[FuncsSA, FuncsIA, SA, IA] { 
          val syn = s; val inh = i; val compose = c; def funcs = fs
        }

    def applyDefault[FuncsSA, FuncsIA >: IA, SA, IA](
      fs: => Algebra[List[trees.Function], FuncsSA, FuncsIA],
      s: Syn.Complete[FuncsSA, SA, IA],
      i: Inh.Complete[FuncsIA, SA, IA] = Inh.Default[FuncsIA, SA, IA])(implicit c: Compose[IA, SA]) =
        apply(fs, s, i)
  }
}
