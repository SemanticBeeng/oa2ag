package oa2ag
package typereconstruction

object attributes {
  trait Value
  case class IntV(value: Int) extends Value
  case class BooleanV(value: Boolean) extends Value
  case class ClosureV(value: Value => Value) extends Value

  type Eval = Map[String, Value] => Value
  trait CanEval { val eval: Eval }
  implicit def CanEval(e: Eval) = new CanEval { val eval = e }


  trait Type
  case object IntT extends Type
  case object BooleanT extends Type
  case class ArrowT(from: Type, to: Type) extends Type
  case class VarT(name: String) extends Type

  trait HasTpe { val tpe: Type }
  implicit def HasTpe(t: Type) = new HasTpe { val tpe = t }

  type TypeEnv = Map[String, Type]
  trait HasTypeEnv { val typeEnv: TypeEnv }
  implicit def HasTypeEnv(env: TypeEnv) = new HasTypeEnv { val typeEnv = env }

  case class Constraint(left: Type, right: Type)
  implicit class TypeConstraint(self: Type) {
    def ===(other: Type) = Constraint(self, other)
  }
  trait HasConstraints { val constraints: Set[Constraint] }
  implicit def HasConstraints(cs: Set[Constraint]) = new HasConstraints { val constraints = cs }

  type Unifier = Map[VarT, Type]
  trait HasUnifier { val unifier: Unifier }
  implicit def HasUnifier(u: Unifier) = new HasUnifier { val unifier = u }

  trait HasPrincipleType { val principleType: Type }
  implicit def HasPrincipleType(t: Type) = new HasPrincipleType { val principleType = t }


  trait HasPP { val pp: String }
  implicit def HasPP(s: String) = new HasPP { val pp = s }
}
