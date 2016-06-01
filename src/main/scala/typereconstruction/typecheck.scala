package oa2ag
package typereconstruction

object typecheck {
  import attributes._
  import Expr._

  private var _count = 0
  private def freshName = {
    _count += 1
    "?x_" + _count
  }
  private def freshTypeVar = VarT(freshName)

  private def unify(cs: Set[Constraint]): Option[Unifier] = cs match {
    case cs if cs.isEmpty => Some(Map.empty)
    case cs => (cs.head, cs.tail) match {
      case (Constraint(s, t), rest) if s == t => unify(rest)
      case (Constraint(s: VarT, t), rest) if !occurs(s, t) => 
        unify(substitute(s, t, rest)).map(_ + (s -> t))
      case (Constraint(s, t: VarT), rest) if !occurs(t, s) => 
        unify(substitute(t, s, rest)).map(_ + (t -> s))
      case (Constraint(ArrowT(f1, a1), ArrowT(f2, a2)), rest) =>
        unify(rest ++ Set(f1 === f2, a1 === a2))
      case _ => None
    }
  }

  private def occurs(x: VarT, in: Type): Boolean = in match {
    case `x` => true
    case ArrowT(f, a) => occurs(x, f) || occurs(x, a)
    case _ => false
  }
  private def substitute(x: VarT, by: Type, in: Type): Type = in match {
    case `x` => by
    case ArrowT(f, a) => ArrowT(substitute(x, by, f), substitute(x, by, a))
    case other => other
  }
  private def substitute(x: VarT, by: Type, in: Set[Constraint]): Set[Constraint] = in.map {
    case Constraint(l, r) => substitute(x, by, l) === substitute(x, by, r)
  }
  private def substitute(t: Type, unifier: Unifier): Type = t match {
    case x: VarT => unifier.getOrElse(x, x)
    case ArrowT(f, a) => ArrowT(substitute(f, unifier), substitute(a, unifier))
    case other => other
  }

  trait Expr_env extends Inh.Default[Any, HasTypeEnv, HasTypeEnv] {
    override def absExpr_body = (name) => ctx => ctx.typeEnv + (name -> freshTypeVar)
  }
  object Expr_env extends Expr_env

  trait Expr_tpe extends Syn[HasTpe with HasTypeEnv, HasTypeEnv, HasTpe] {
    def varExpr = (name) => ctx => ctx.typeEnv(name)
    // Since a fresh type is used for `name` and stored in the typeenv of body
    // we need to extract it here.
    def absExpr = (name, body) => ctx => ArrowT(body.typeEnv(name), body.tpe)
    def appExpr = (fun, arg) => ctx => freshTypeVar // rest is done in constraints
    def numExpr = (value) => ctx => IntT
    def succExpr = (base) => ctx => IntT
    def predExpr = (base) => ctx => IntT
    def isZeroExpr = (base) => ctx => BooleanT
    def boolExpr = (value) => ctx => BooleanT
    def ifExpr = (cond, thn, els) => ctx => thn.tpe // σ(thn.tpe) == σ(els.tpe)
  }
  object Expr_tpe extends Expr_tpe

  trait Expr_constraints extends Syn[HasConstraints with HasTpe, HasTpe, HasConstraints] {
    def varExpr = (name) => ctx => Set.empty[Constraint]
    def absExpr = (name, body) => ctx => body
    def appExpr = (fun, arg) => ctx => 
      fun.constraints ++ arg.constraints + (fun.tpe === ArrowT(arg.tpe, ctx.tpe))
    def numExpr = (value) => ctx => Set.empty[Constraint]
    def succExpr = (base) => ctx => base.constraints + (base.tpe === IntT)
    def predExpr = (base) => ctx => base.constraints + (base.tpe === IntT)
    def isZeroExpr = (base) => ctx => base.constraints + (base.tpe === IntT)
    def boolExpr = (value) => ctx => Set.empty[Constraint]
    def ifExpr = (cond, thn, els) => ctx => cond.constraints ++ thn.constraints ++
      els.constraints ++ Set(cond.tpe === BooleanT, thn.tpe === els.tpe)
  }
  object Expr_constraints extends Expr_constraints

  val Expr_unifier = Syn.Decorate[HasConstraints with HasPP, HasUnifier] { ctx =>
    unify(ctx.constraints).getOrElse(sys error s"Cannot unify: ${ctx.pp} | ${ctx.constraints}")
  }

  val Expr_principleType = Syn.Decorate[HasTpe with HasUnifier, HasPrincipleType] {
    ctx => substitute(ctx.tpe, ctx.unifier)
  }

  val inh = Expr_env
  val syn = Syn.Require[HasPP with HasTypeEnv] <+ Syn.Default[HasTypeEnv] <+ 
    Expr_tpe <+ Expr_constraints <+ Expr_unifier <+ Expr_principleType
  val init = HasTypeEnv(Map.empty)
}
