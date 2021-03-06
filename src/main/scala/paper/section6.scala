package oa2ag.paper

/**
 * 6. Formalization
 * ================
 * The formalization of our encoding is accompanied by a code generator that
 * automatically performs the necessary code transformations.
 *
 * This file serves as a brief tutorial on how to use the code generator to
 * facilitate experimenting with the formalization.
 */
package object section6 {

  import ast._

  /**
   * The input to the generator is given as a tree structure similar to the
   * algebraic signature in figure 12 (repeated below).
   *
   *     Lit : n         → E
   *     Add : E × E     → E
   *     Var : X         → E
   *     Set : X × E × S → S
   *     Exp : E         → S
   *
   *    Figure 12. Operations
   *
   * The operations are grouped based on the returned nonterminal as discussed
   * in section 6.3.
   */
  val expressions = Decl('Expr,
    Constructor('Lit, 'value -> int),
    Constructor('Add, 'lhs -> 'Expr, 'rhs -> 'Expr),
    Constructor('Vari, 'name -> string))

  val statements = Decl('Stmt,
    Constructor('Set, 'name -> string, 'exp -> 'Expr, 'body -> 'Stmt),
    Constructor('Exp, 'value -> 'Expr))

  /**
   * There are two ways to use the code generator.
   *
   * 1. Add the code generator as dependency for the compile task of sbt (as it
   *    is done in all other source files using generated code)
   *
   * 2. Manually invoke the generator (as it is done in this file) to generate
   *    support code.
   *
   * To allow convenient experiments with the generator we introduced the second
   * alternative, for production use the first alternative is recommended. For
   * easy reference, the source code of `Generator.scala` is located both in
   * `project` as well as `src/main/scala/paper/section6`.
   */
  def generate(decl: Decl, filename: String) {

    val defaultPath = "src/main/scala/paper/"
    val defaultPrelude = "package oa2ag\npackage paper.section6\npackage test"

    // This invokes the generator and writes the results to the provided filename
    Generator(decl, defaultPath + filename, defaultPrelude)
  }

  /**
   * You may want to change the input to the generator above and then (after
   * reloading the console) invoke:
   *
   *     scala> paper.section6.generateExprAndStmt
   *
   * The files generated by calling the above command are located in
   * `src/main/scala/paper/section6/` and are named `<NAME>.test.scala`
   */
  def generateExprAndStmt {
    generate(expressions, "section6/Expr.test.scala")
    generate(statements, "section6/Stmt.test.scala")
  }

  /**
   * Format of the Generator-Input
   * -----------------------------
   * *Built-in Types*. The following types are considered built-in types and
   * thus belong to the Set `B` introduced in section 6.1:
   *
   * - `int`
   * - `string`
   * - `boolean`
   *
   * In the above examples `expressions` and `statements` we can see the usage
   * of the built-in types `int` and `string`.
   *
   * *User defined Types*. Nonterminals occurring in the body of a production
   * are called "user defined types" in terms of the input format. User defined
   * types can either be specified by using the constructor
   *
   *     UserDefined(name: Symbol)
   *
   * or by using the short hand syntax of `'Name` as
   * in the above examples.
   *
   * *Applied Types*. If a type is given by a type application, such as
   * `List[Expr]`, the generator will treat the type (similar to user defined
   * types) as an additional nonterminal dependency. Applied types can either be
   * defined by using the constructor
   *
   *     AppliedType(name: Symbol, arg: Type)
   *
   * or by using the two provided shorthand functions `list(arg: Type)` and
   * `option(arg: Type)` as can be seen in the above examples.
   *
   * The tree format is defined in the file `Generator.scala` within the object
   * definition of `oa2ag.paper.section6.ast`.
   */

  /**
   * Since the input to the generator is already close to the algebraic
   * signature the first step of extracting the algebra from a context free
   * grammar can be omitted.
   * However, the extraction of *context decorators* algebras is part of the
   * generator and can be found in the method `transformToInh`. This method
   * implements the procedure described in the last paragraphs of section 6.2.
   *
   * After having extracted the signatures of the context decorator algebras
   * the rest of the generator is used for both the original algebraic signature
   * as well as the context decorators.
   *
   * We now will take a closer look the generated output. So if you did not yet
   * run `sbt compile` or `sbt console` you might want to do it now in order to
   * (re)generate the file `paper/section6/Stmt.gen.scala`. This file contains
   * the output for the following signature description annotated with named
   * arguments:
   *
   *     Set : (name: String) × (exp: Expr) × (body: Stmt) → Stmt
   *     Exp : (value: Expr)                               → Stmt
   *
   * To assure separation of namespaces all code is generated into one object
   * declaration named after the grouping nonterminal (here `Stmt`).
   *
   * The object contains:
   *
   * 1. Syntax            Classes representing physical trees.
   *
   * 2. Signature         The pre-function-algebraic signature which would be
   *                      called `CtxStmtAlg` in the paper.
   *                      The corresponding companion object contains the
   *                      implementation of `DepProduct` which is called
   *                      `ExprCompose` in section 2 of the paper. It also
   *                      contains a binary function `alg1 <+ alg2` that serves
   *                      as factory for
   *                      `DepProduct { left = alg1, right = alg2 }`.
   *
   * 3. Inh<NAME>Sig      For every nonterminal occurring on the right hand side
   *                      of a production a context decorator is defined.
   *                      Similar to `Signature`, the companion object contains
   *                      the functionality to compose two instances of
   *                      `Inh<NAME>Sig`.
   *
   * 4. AttributeGrammar  A trait that allows the final assembly of `Signature`
   *                      and the context decorators. This trait is called
   *                      `ExprAssemble` in section 4 of the paper.
   *
   * 5. Foldable          A trait that specifies how the physical trees can be
   *                      folded to convert to a church encoded representation.
   *
   * 6. Algebra           A type alias, called `SSig` in figure 14 (e).
   *
   * 7. Syn               A type alias, called `CtxSSig` in figure 14 (e).
   *                      The corresponding companion object contains some
   *                      auxiliary functions that abstract over common
   *                      attribute definitions. They are discussed in the
   *                      later comment sections.
   *
   * 8. Inh<NAME>         Type aliases for the context decorators similar
   *                      to `Syn`. Also the companion object contains
   *                      the same auxiliary functions as in `Syn`.
   *
   * In addition the method `Stmt.apply` acts as factory for `AttributeGrammar`.
   */

  /**
   * Example
   * -------
   * Let's define a synthesized attribute "pp" for statements using the
   * generated code in `Stmt.gen.scala`.
   */
  import oa2ag.lc.attributes.HasPP
  val pp = new Stmt.Syn[
      HasPP, // In:  Requirements on child nodes of type `Expr`
      HasPP, // In:  Requirements on child nodes of type `Stmt`
      Any,   // In:  Requirements on the current node
      HasPP  // Out: Resulting attribute
  ] {
    def set = (name: String, exp: HasPP, body: HasPP) => (ctx: Any) =>
      HasPP(name + " := " + exp.pp + " in " + body.pp)

    def exp = (value: HasPP) => (ctx: Any) =>
      HasPP(value.pp)
  }

  // Since we can omit type annotations and HasPP also defines an implicit
  // conversion we also can express the above attribute as:
  val pp2 = new Stmt.Syn[HasPP, HasPP, Any, HasPP] {
    def set = (name, exp, body) => _ => name + " := " + exp.pp + " in " + body.pp
    def exp = value => _ => value.pp
  }

  // Which is almost as concise as the original AG specification.

  /**
   * Auxiliary Functions
   * -------------------
   * As already mentioned above, the generator automatically generates some
   * auxiliary functions which we found very useful when implementing various
   * attribute grammars as object algebras.
   *
   * Almost all of them build on the trait `Decorate` which is similar to
   * the `decorate` combinator in "Oliveira - Feature Oriented Programming with
   * Object Algebras". However, since we do not decorate the results of an
   * algebra, but the computational context our decorator is more general: It is
   * not restricted to functions of type `A => A` but instead allows defining
   * new attributes in terms of already computed ones.
   */

  import oa2ag.lc.attributes.CanSayHello
  import oa2ag.Compose

  // Does not impose restrictions on the computational context of the current
  // node, by requiring `Any`.
  val sayHello: Stmt.Syn[Any, Any, Any, CanSayHello] = Stmt.Syn.Decorate {
    (s: Any) => CanSayHello(n => s"Hello '$n'")
  }

  // The following code defines an attribute `HasPP` by combining two other
  // attributes `HasPP` and `CanSayHello`, which have to be already computed
  // on the current node.
  val helloPP: Stmt.Syn[Any, Any, CanSayHello with HasPP, HasPP] = Stmt.Syn.Decorate {
    (s: CanSayHello with HasPP) => HasPP(s.sayHello(s.pp))
  }

  // We can satisfy the requirements with `sayHello` and `pp` from above:
  val composed: Stmt.Syn[
      HasPP,
      HasPP,
      Any,                    // requirements on the context have been satisfied
      CanSayHello with HasPP
  ] = sayHello <+ pp <+ helloPP

  /**
   * The other auxiliary functions are defined in terms of `Decorate`
   *
   *    Default[T]              = Decorate[Any, Any, T, T](identity[T])
   *    Dummy[Expr, Stmt, S, T] = Decorate[Expr, Stmt, S, T](???)
   *    Require[Req]            = Decorate[Any, Any, Req, Any](identity[Any])
   *
   * For example the attribute grammar that takes a value of HasPP as context
   * information, passes it down and returns the same value as the result can
   * be implemented as:
   */
  val someExprAlg = Expr(Expr.Inh.Default[Any], Expr.Syn.Default[Any])
  val passPP = Stmt(
    // On final assembly, for every non-terminal dependency we have to give an
    // instance of an algebra that satisfies the corresponding requirements.
    // Here we do not depend on computation of child nodes of type `Expr` so
    // we can pass an arbitrary algebra.
    someExprAlg,
    Stmt.InhExpr.Default[HasPP],
    Stmt.InhStmt.Default[HasPP],
    Stmt.Syn.Default[HasPP])
}
