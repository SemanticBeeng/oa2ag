package oa2ag

trait ConsList[Elem, ElemSA, ElemIA, SA, IA] extends Algebra[List[Elem], SA, IA] {
  val syn: ConsList.Syn.Complete[ElemSA, SA, IA]
  val inh: ConsList.Inh.Complete[ElemSA, ElemIA, SA, IA]

  def elem: Algebra[Elem, ElemSA, ElemIA]

  def apply(t: List[Elem], attr: IA): SA = t match {
    case Nil => syn.empty(attr)
    case head :: tail => {
      val headInh = inh.consHead(attr)
      val headAll = elem.compose(headInh, elem(head, headInh))
      val tailInh = inh.consTail(attr, headAll)
      val tailAll = compose(tailInh, this(tail, tailInh))
      syn.cons(headAll, tailAll, attr)
    }
  }
}
object ConsList {

  trait Syn[-ElemT, -Req, -Prov, -IA, +OutT] extends Algebra.Syn[Req, Prov, IA, OutT] {
    type Elem = ElemT
    def empty(self: Self): Out
    def cons(head: Elem, tail: Child, self: Self): Out
  }

  object Syn {
    type Attr[Elem, Req, Prov, IA] = Syn[Elem, Req, Prov, IA, Prov]
    type Complete[Elem, SA, IA] = Syn[Elem, Any, SA, IA, SA]

    def composeAlg[
        Elem1, Req1, Prov1, IA1, Out1, 
        Elem2, Req2 >: Out1, Prov2, IA2, Out2](
          alg1: Syn[Elem1, Req1, Prov1, IA1, Out1],
          alg2: Syn[Elem2, Req2, Prov2, IA2, Out2])(implicit
          comp1: Compose[Req1 with IA1 with IA2, Req2],
          comp2: Compose[Out1, Out2]) =
        new Syn[Elem1 with Elem2, Req1, Prov1 with Prov2 with Req2, IA1 with IA2, Out1 with Out2] {
          private def assemble(f: Self => Out1, g: comp1.Out => Out2, self: Self): comp2.Out = {
            val out = f(self)
            comp2(out, g(comp1(self, out)))
          }

          def empty(self: Self): Out = 
            assemble(alg1.empty(_), alg2.empty(_), self)

          def cons(head: Elem, tail: Child, self: Self): Out =
            assemble(alg1.cons(head, tail, _), alg2.cons(head, tail, _), self)
    }
  }

  trait Inh[-ElemSAT, ElemIAT, -SA, -IA, +Out] extends Algebra.Inh[SA, IA, Out] {
    type ElemIA = ElemIAT
    type ElemSA = ElemSAT

    def consHead(parent: IA): ElemIA
    def consTail(parent: IA, head: ElemIA with ElemSA): Out
  }
  object Inh {
    type Complete[ElemSA, ElemIA, SA, IA] = Inh[ElemSA, ElemIA, SA, IA, IA]

    trait Default[ElemSA, ElemIA >: IA, -SA, -IA, +Out >: IA] extends Inh[ElemSA, ElemIA, SA, IA, Out] {
      def consHead(parent: IA): ElemIA = parent
      def consTail(parent: IA, head: ElemIA with ElemSA): Out = parent
    }
    object Default {
      def apply[ElemSA, ElemIA >: IA, SA, IA] = new Default[ElemSA, ElemIA, SA, IA, IA] {}
    }

    trait Forward[ElemSA, ElemIA >: IA, -SA, -IA, +Out >: ElemSA] extends Inh[ElemSA, ElemIA, SA, IA, Out] {
      def consHead(parent: IA): ElemIA = parent
      def consTail(parent: IA, head: ElemIA with ElemSA): Out = head
    }
    object Forward {
      def apply[IA] = new Forward[IA, IA, Any, IA, IA] {}
    }

    def composeAlg[
      ElemSA1, ElemIA1, SA1, IA1, Out1, 
      ElemSA2, ElemIA2, SA2, IA2, Out2](
        alg1: Inh[ElemSA1, ElemIA1, SA1, IA1, Out1],
        alg2: Inh[ElemSA2, ElemIA2, SA2, IA2, Out2]
      )(implicit 
      comp1: Compose[Out1, Out2],
      comp2: Compose[ElemIA1, ElemIA2]) = 
    new Inh[ElemSA1 with ElemSA2, ElemIA1 with ElemIA2, SA1 with SA2, IA1 with IA2, Out1 with Out2] {
      def consHead(parent: IA): ElemIA =
        comp2(alg1.consHead(parent), alg2.consHead(parent))

      def consTail(parent: IA, head: ElemIA with ElemSA): Out =
        comp1(alg1.consTail(parent, head), alg2.consTail(parent, head))
    }
  }
  
  object ops {
    implicit class ConsListSynOps[Elem1, Req1, Prov1, IA1, Out1](self: Syn[Elem1, Req1, Prov1, IA1, Out1]) {
      def <+[Elem2, Req2 >: Out1, Prov2, IA2, Out2](other: Syn[Elem2, Req2, Prov2, IA2, Out2])(
          implicit comp: Compose[Req1 with IA1 with IA2, Req2], comp2: Compose[Out1, Out2]) =
        Syn.composeAlg(self, other)
    }

    implicit class ConsListInhOps[ElemSA1, ElemIA1, SA1, IA1, Out1](self: Inh[ElemSA1, ElemIA1, SA1, IA1, Out1]) {
      def <+[ElemSA2, ElemIA2, SA2, IA2, Out2](other: Inh[ElemSA2, ElemIA2, SA2, IA2, Out2])(
        implicit 
          comp: Compose[Out1, Out2],
          comp2: Compose[ElemIA1, ElemIA2]) = Inh.composeAlg(self, other)
    }
  }

  def apply[Elem, ElemSA, ElemIA, SA, IA](
      el: => Algebra[Elem, ElemSA, ElemIA],
      s: Syn.Complete[ElemSA, SA, IA], 
      i: Inh.Complete[ElemSA, ElemIA, SA, IA])(implicit c: Compose[IA, SA]) =
    new ConsList[Elem, ElemSA, ElemIA, SA, IA] { 
      val syn = s
      val inh = i
      val compose = c
      def elem = el
    }

  def applyDefault[Elem, ElemSA, ElemIA >: IA, SA, IA](
      el: => Algebra[Elem, ElemSA, ElemIA],
      s: Syn.Complete[ElemSA, SA, IA], 
      i: Inh.Complete[ElemSA, ElemIA, SA, IA] = Inh.Default[ElemSA, ElemIA, SA, IA])(implicit c: Compose[IA, SA]) =
    apply(el, s, i)
}

