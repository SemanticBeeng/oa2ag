package oa2ag

import scala.language.experimental.macros
import scala.reflect.macros.Context

// The compose parameter is needed as long as we do not have a generic dynamic mixin composition function.
// Compose is a type class that can be used for evidence that two types can be composed. It has an `apply`
trait Compose[A, B] {
  type Out = A with B
  def apply(a: A, b: B): Out
}
object Compose {

  def apply[A, B](a: A, b: B)(implicit comp: Compose[A, B]) = comp(a, b)
  
  // naive implementation of multi argument composition:
  def apply[A, B, C](a: A, b: B, c: C)(implicit 
    comp1: Compose[A, B], comp2: Compose[A with B, C]) = comp2(comp1(a, b), c)
  
  def apply[A, B, C, D](a: A, b: B, c: C, d: D)(implicit 
      comp1: Compose[A, B], 
      comp2: Compose[A with B, C],
      comp3: Compose[A with B with C, D]) = comp3(comp2(comp1(a, b), c), d)
  
  def apply[A, B, C, D, E](a: A, b: B, c: C, d: D, e: E)(implicit 
      comp1: Compose[A, B], 
      comp2: Compose[A with B, C],
      comp3: Compose[A with B with C, D],
      comp4: Compose[A with B with C with D, E]) = comp4(comp3(comp2(comp1(a, b), c), d), e)

  // maybe introduce some caching mechanism
  implicit def compose[A, B]: Compose[A, B] = macro Compose.composeImpl[A, B]

  def composeImpl[A, B](c: Context)(implicit aT: c.WeakTypeTag[A], bT: c.WeakTypeTag[B]): c.Expr[Compose[A, B]] = {
    import c.universe._
    
    def isAbstract(sym: Symbol): Boolean = sym
      .asInstanceOf[scala.reflect.internal.Symbols#Symbol]
      .hasFlag(scala.reflect.internal.Flags.DEFERRED)

    def isGetter(sym: Symbol): Boolean = sym
      .asInstanceOf[scala.reflect.internal.Symbols#Symbol]
      .isGetter

    def dealias(t: Type) = t
      .asInstanceOf[scala.reflect.internal.Types#Type]
      .dealias
      .asInstanceOf[Type]

    def parents[T](tt: WeakTypeTag[T]) = tt
      .tpe
      .asInstanceOf[scala.reflect.internal.Types#Type]
      .parents
      .asInstanceOf[List[Type]]

    /**
     * Should return components of intersection types as list
     * If type is not intersection type returns the singleton list
     */
    def getTypeComponents(t: Type): List[Type] = dealias(t) match {
      case RefinedType(parents, _) => parents.flatMap( p => getTypeComponents(p) )
      case t => List(t)
    }

    def filterOutObjectLikeThings[T](types: List[Type]): List[Type] = types.filter { t =>
      ! (t =:= typeOf[AnyRef] || t =:= typeOf[AnyVal] || t =:= typeOf[Any])
    }

    val a = newTermName(c.fresh("a"))
    val b = newTermName(c.fresh("b"))

    def abstractMembers[T](tt: WeakTypeTag[T]) = 
       tt.tpe.members.toList.filter(isAbstract)

    def defineMember(member: Symbol, delegatee: TermName) = {
      val name = newTermName(member.name.toString)
      if (isGetter(member))
        q"""val $name = $delegatee.$name;"""
      else
        q"""def $name = $delegatee.$name;"""
    }

    val abstractMembersA = abstractMembers(aT)
    val abstractMembersB = abstractMembers(bT)
    val memberDefs = abstractMembersB.map { defineMember(_, b) } ++ (abstractMembersA
      .filterNot(abstractMembersB contains _)
      .map { defineMember(_, a) })

    val types = filterOutObjectLikeThings((getTypeComponents(aT.tpe) ++ getTypeComponents(bT.tpe)).distinct)

    val constrName = newTypeName(c.fresh("Impl"))

    val defaultConstr = DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), 
      TypeTree(), 
      Block(List(
        Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), 
      Literal(Constant(()))))

    val anonimpl = ClassDef(NoMods, constrName, Nil, Template(
      types.map(t => tq"$t"), 
      emptyValDef,
      defaultConstr +: memberDefs))

    c.Expr[Compose[A, B]](q"""new Compose[$aT, $bT] { 
      def apply($a: $aT, $b: $bT) = {
        $anonimpl ;
        new $constrName
      }
    }""")
  }

  implicit def anyBoth: Compose[Any, Any] = new Compose[Any, Any] {
    def apply(a: Any, b: Any): Any with Any = a
  }

  implicit def anyL[T]: Compose[Any, T] = new Compose[Any, T] {
    def apply(a: Any, b: T): Any with T = b
  }

  implicit def anyR[T]: Compose[T, Any] = new Compose[T, Any] {
    def apply(a: T, b: Any): T with Any = a
  }
}
