package oa2ag
package czero
package examples.declarations

object attributes {

  import algebras._

  trait HasNothing
  object HasNothing extends HasNothing

  trait HasDeclarations extends HasNothing { val decls: Set[String] }
  object HasDeclarations { def apply(ds: Set[String]) = new HasDeclarations { val decls = ds } }

  // ignore any
  implicit def compAnyR[T]: Compose[T, Any] = new Compose[T, Any] {
    def apply(a: T, b: Any): T with Any = a
  }
  implicit def compAnyL[T]: Compose[Any, T] = new Compose[Any, T] {
    def apply(a: Any, b: T): Any with T = b
  }

  // syn HasDeclarations
  // Req, Prov, IA
  object DeclarationDecls extends Declaration.Syn.Attr[Any, HasDeclarations, HasDeclarations] {
    def intDecl(id: String, init: Option[Int], self: Self): Out = {
      println(s"(syn) intDecl $id (${self.decls})")
      if (self.decls contains id) {
        sys error s"Duplicate declaration of $id"
      } else {
        HasDeclarations(self.decls + id)
      }
    }
  }

  // syn HasDeclarations
  // Attr[Elem, Req, Prov, IA]
  object DeclListDeclsSyn extends ConsList.Syn.Attr[HasDeclarations, Any, HasDeclarations, HasDeclarations] {
    def empty(self: Self): Out = self
    def cons(head: Elem, tail: Child, self: Self): Out = tail
  }

  val declarationAlg = Declaration[HasDeclarations, HasDeclarations](DeclarationDecls)
  val declList = ConsList(declarationAlg, DeclListDeclsSyn, ConsList.Inh.Forward[HasDeclarations])

  def listDeclarations(t: List[trees.Declaration], init: Set[String] = Set.empty): Set[String] = 
    declList(t, HasDeclarations(init)).decls

}

object test {
  import trees._
  import attributes._

  val prog = List(
    IntDecl("a", None),
    IntDecl("b", None),
    IntDecl("c", None)
  )

  println(listDeclarations(prog))
  // => Set(a,b,c)

  // println(listDeclarations(prog :+ IntDecl("a", None)))
  // => error duplicate declaration of a
}
