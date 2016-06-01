package oa2ag
package lc
package typed

object attributes {
  trait HasType[T] extends HasNothing { val tpe: T }
  implicit def HasType[T](t: T) = new HasType[T] { val tpe = t }

  trait HasTypeEnv[T] extends HasNothing { val tpeEnv: Map[String, T] }
  implicit def HasTypeEnv[T](te: Map[String, T]) = new HasTypeEnv[T] { val tpeEnv = te }
}

object equations {
  import lc.attributes._
  import attributes._

  // Expr.pp
  // -------
  trait Expr_pp extends Expr.Syn[HasPP, HasPP, Any, HasPP] {
    def const = v => self => s"Const($v)"
    def add = (l, r) => self => s"Add(${l.pp}, ${r.pp})"
    def id = name => self => s"Id($name)"
    def lambda = (n, t, b) => self => s"Lambda($n, ${t.pp}, ${b.pp})"
    def call = (f, a) => self => s"Call(${f.pp}, ${a.pp})"
  }
  object Expr_pp extends Expr_pp

  // Expr.fv
  // -------
  trait Expr_fv extends Expr.Syn[Any, HasFV, Any, HasFV] {
    def const = v => self => Set.empty[String]
    def add = (l, r) => self => l.fv ++ r.fv
    def id = name => self => Set(name)
    def lambda = (n, t, b) => self => b.fv - n
    def call = (f, a) => self => f.fv ++ a.fv
  }
  object Expr_fv extends Expr_fv

  // Expr.tpeEnv
  // -----------
  trait Expr_tpeEnv[T] extends Expr.InhExpr.Default[T, Any, HasTypeEnv[T], HasTypeEnv[T]] {
    override def lambda_body = (name, tpe) => parent => parent.tpeEnv + (name -> tpe)
  }
  def Expr_tpeEnv[T] = new Expr_tpeEnv[T] {}

  // Expr.tpe
  // ---------
  trait Expr_tpe[T] extends Expr.Syn[T, HasType[T], HasTypeEnv[T], HasType[T]] {

    def alg: Type.Algebra[Any => T]

    def const = v => self => alg.intT(null)
    def add = (l, r) => self => alg.intT(null) // TODO use attribute on T like IsInt, or HasEquals
    def id = name => self => self.tpeEnv(name)
    def lambda = (n, t, b) => self => alg.arrowT(_ => t, _ => b.tpe)(null)
    def call = (f, a) => self => ??? // TODO
  }
  def Expr_tpe[T](a: Type.Algebra[Any => T]) = new Expr_tpe[T] { val alg = a }

  object Type_pp extends Type.Syn[HasPP, Any, HasPP] {
    def intT = self => "Int"
    def arrowT = (f, a) => self => s"${f.pp} => ${a.pp}"
  }

  val Type_nothing = Type.Inh.Default[Any]
}

object test {
  import lc.attributes._
  import attributes._
  import equations._
  import Expr.Syntax._
  import Type.Syntax._

  val program = Lambda("x", IntT(), Add(Add(Const(22), Id("x")), Const(8)))

  val tpePrintAlg = Type(Type_nothing, Type_pp)

  val alg = Expr(tpePrintAlg, 
    Expr_tpeEnv[HasPP], 
    Expr.InhType.Default[Any],
    Expr_tpe[HasPP](tpePrintAlg))

  println( alg(program)(Map.empty[String, HasPP]).tpe.pp )

}