package oa2ag.paper
package object section7 {

  import section6._
  import ast._

  /**
   * The case study has been conducted in order to support our claims that the
   * encoding presented in the paper is:
   *
   * 1. Modular
   * 2. Scalable
   * 3. Compositional
   *
   * This files provides an overview on how the claims might be evaluated.
   */

   /**
    * Modularity
    * ----------
    * It is easy to see that the source code of the case study (located at
    * `src/main/scala/czero`) is split into many source files. The attribute
    * equations are grouped by nonterminal, but this choice is rather arbitrary.
    * Since equation sets are just values of the host language they can be
    * freely organized and compiled separately. The equation sets can be found
    * in `src/main/scala/czero/<NONTERMINAL>/equations.scala.`
    *
    * Even single equations can be defined, compiled separately and combined
    * later using mixin composition. For instance consider the pretty printer
    * seen earlier in `section6.scala`, now split up into two traits:
    */
  import oa2ag.lc.attributes.HasPP

  trait PP_set extends Stmt.Syn[HasPP, HasPP, Any, HasPP] {
    def set = (name, exp, body) => _ => 
      HasPP(name + " := " + exp.pp + " in " + body.pp)
  }
  trait PP_exp extends Stmt.Syn[HasPP, HasPP, Any, HasPP] {
    def exp = value => _ =>  HasPP(value.pp)
  }

  // It is statically checked, that all equations are provided
  val pp = new PP_set with PP_exp


  /**
   * Scalability
   * -----------
   * "The encoding scales linearly in the number of nonterminals"
   *
   * We can investigate how the generated code scales in the
   * number of nonterminals as well as how it scales in the
   * number of dependencies between nonterminals.
   *
   * (a) The generated code clearly scales linearly in the number
   *     of nonterminals since per nonterminal almost the same
   *     amount of code has to be generated.
   *
   * (b) The generated code also scales linearly in the number
   *     of dependencies between nonterminals.
   *
   * To see the latter, we add nonterminal dependencies and see
   * what happens.  A nonterminal depends on another nonterminal
   * if the latter occurs in a production of the former.
   *
   * Let's say we change
   *
   *     Set : X × E × S → S
   *     Exp : E         → S
   *
   * to
   *
   *     Set : X × E × S → S
   *     Exp : ListOfE   → S
   *
   * in order to allow multiple expression to occur in statement
   * position.  The new nonterminal ListOfE occurs in a
   * production for the nonterminal S, so this change adds a
   * dependency between the nonterminals S and ListOfE.
   *
   * We encode the extended S terminal for our generator:
   */

  val extendedStatement = Decl('Stmt,
    Constructor('Set, 'name -> string, 'exp -> 'Expr, 'body -> 'Stmt),
    Constructor('Exp, 'value -> 'ListOfExpr))

  def test {
    generate(extendedStatement, "section7/Stmt.test.extended")
  }

  /**
   * You can call paper.section7.test from the Scala console to
   * generate a file
   *
   *   src/main/scala/paper/section7/Stmt.test.extended.scala
   *
   * with the generated code for the extended S nonterminal. To
   * understand how the generated code changed with the
   * introduction of the additional dependency between S and
   * ListOfE, compare this file with the generate code for the
   * original S nonterminal:
   *
   *   src/main/scala/paper/section6/Stmt.gen.scala
   *
   * Let's see how the generated code has changed. The signature
   *
   *     trait Signature[-Expr, -Stmt, +Out] {
   *       def set: (String, Expr, Stmt) => Out
   *       def exp: (Expr) => Out 
   *     }
   *
   * as found in `section6/Stmt.gen.scala` has become 
   *
   *     trait Signature[-Expr, -ListOfExpr, -Stmt, +Out] {
   *       def set: (String, Expr, Stmt) => Out
   *       def exp: (ListOfExpr) => Out 
   *     }
   *
   * in `section7/Stmt.gen.extended.scala`. One additional
   * dependency leads to one additional generic type parameter
   * for the `Signature` trait. Uses of the `Signature` trait
   * change accordingly, because they have to provide the new
   * type parameter.
   *
   * Another impact of the change can be seen in the implementation
   * of the trait for assembly:
   *
   *     trait AttributeGrammar[ExprIA, ExprSA, IA, SA] 
   *         extends Algebra[ExprIA => ExprIA with ExprSA, 
   *                         IA => IA with SA] {
   *       ...
   *     }
   *
   * now becomes
   *
   *     trait AttributeGrammar[ExprIA, ExprSA, ListOfExprIA, ListOfExprSA, IA, SA] 
   *         extends Algebra[ExprIA => ExprIA with ExprSA, 
   *                         ListOfExprIA => ListOfExprIA with ListOfExprSA, 
   *                         IA => IA with SA] {
   *       ...
   *     }
   *
   * Here we have to add two type parameters per added
   * dependency, still allowing for linear scaling. Over all,
   * this example illustrates how the size of the encoding scales
   * linearly in the number of nonterminal dependencies.
   *
   * It is important to note that transitive dependencies do not
   * contribute to the number of type parameters. For example, in
   * the C0 case study the start-symbol `Program` directly
   * depends on the nonterminal for function lists, and
   * indirectly depends on the nonterminals for expressions and
   * statements. Only the direct dependencies are reflected as
   * type parameters.
   */


  /**
   * Compositionality
   * ----------------
   * "Each AG artifact is represented as a semantic object of the host language"
   *
   * in particular:
   * 
   * "The embedding into a high level language allows introducing novel
   * abstractions not anticipated by the original encoding."
   *
   * We were able to abstract over nonterminals that represent lists and thus
   * reuse equations specified on lists in general.
   * This can be seen in the file `src/main/scala/czero/lists/equations.scala`.
   *
   * For instance the attribute `ListCount` is used for both argument lists as
   * well as declaration lists.
   */

}
