package oa2ag.paper
package generator

/**
 * Since we are now always using the same definition of algebras (namely the
 * generated one) all examples can be located in one place
 */
object examples {

  def threepluszeroplusfive[E](alg: Expr.Algebra[E]) =
    alg.add(alg.add(alg.lit(3), alg.lit(0)), alg.lit(5))

  def threeplusfive[E](alg: Expr.Algebra[E]) =
    alg.add(alg.lit(3), alg.lit(5))

  def zeropluszeroplusfive[E](alg: Expr.Algebra[E]) =
    alg.add(alg.add(alg.lit(0), alg.lit(0)), alg.lit(5))

  def threeplusfourplusfive[E](alg: Expr.Algebra[E]) =
    alg.add(alg.lit(3), alg.add(alg.lit(4), alg.lit(5)))

  def threeplusfourplusfivepluszero[E](alg: Expr.Algebra[E]) =
    alg.add(alg.add(alg.lit(3), alg.lit(4)), alg.add(alg.lit(5), alg.lit(0)))

}