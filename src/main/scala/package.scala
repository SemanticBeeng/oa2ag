package oa2ag

trait HasNothing
object HasNothing extends HasNothing

trait Nonterminal {

  /**
   * Assembly scheme for DepProduct
   */
  trait DepProductHelper[Self1, Out1, Out2] {
    
    implicit val compose1: Compose[Out1, Out2]
    implicit val compose2: Compose[Self1, Out1]

    def assemble(self: Self1, f: Self1 => Out1, g: (Self1 with Out1) => Out2) = {
      val out1 = f(self)
      compose1(out1, g(compose2(self, out1)))
    }
  }
}

/**
 * The interface for full algebras
 *
 * @deprecated use new, generated interfaces instead
 *
 * @tparam T the element type to fold over
 * @tparam SA the synthesized attributes
 * @tparam IA the inherited attributes
 */
trait Algebra[T, SA, IA] {
  // This is fold
  def apply(t: T, attr: IA): SA

  val compose: Compose[IA, SA]
  val syn: Algebra.Syn.Complete[SA, IA]
  val inh: Algebra.Inh.Complete[SA, IA]
}
object Algebra {
  trait Syn[-Req, -Prov, -IA, +OutT] {
    type Require = Req
    type Provide = Prov
    type Inherited = IA
    type Out = OutT

    // L attributed stuff
    type Self = Require with Inherited
    type Child = Self with Provide
  }
  object Syn {
    type Attr[Req, Prov, IA] = Syn[Req, Prov, IA, Prov]
    type Complete[SA, IA] = Syn[Any, SA, IA, SA]
  }

  trait Inh[-SAT, -IAT, +OutT] {
    type IA = IAT
    type SA = SAT
    type Out = OutT
  }

  object Inh {
    type Complete[SA, IA] = Inh[SA, IA, IA]
    // object Default extends Inh[Any, Any, Any, Any]
  }
}
