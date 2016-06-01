package oa2ag
package paper
package object mix {

  /**
   * The first artifact we are submitting is a macro that allows dynamic mixin
   * composition. Instead of using runtime reflection as it is done by Oliveira
   * et al. in "Feature Oriented Programming with Object Algebras", we are using
   * compile time reflection utilizing macros.
   */
  trait HasFoo {
    def foo: Int
  }
  
  trait HasBar {
    def bar: String
  }
  
  /**
   * Given the two simple traits `HasFoo` and `HasBar` above we can manually
   * provide a method to compose instances of the two traits as follows:
   */
  def mix1(a: HasFoo, b: HasBar) = new HasFoo with HasBar {
    def foo = a.foo
    def bar = b.bar
  }
  
  /**
   * We are using delegation to implement the abstract methods `foo` and `bar`.
   * Supplying the two arguments `a` and `b` to `mix1` we can 
   * dynamically create an instance that fulfills both interfaces:
   */
  val a: HasFoo = new HasFoo { def foo = 42 }
  val b: HasBar = new HasBar { def bar = "Hello World" }
  val both1: HasFoo with HasBar = mix1(a, b)
  
  /**
   * In order to be able extend the definition of `mix1` with new combinations
   * traits to be mixed together we can define a version parametrized over a
   * type-class.
   *
   * The typeclass Compose[A, B] is a witness we need to provide, that `A` and
   * `B` can be mixed together.
   */  
  def mix[A, B](a: A, b: B)(implicit c: Compose[A, B]): A with B = c(a, b)

  /**
   * We now can manually provide the witness for `HasFoo` and `HasBar` following
   * the same pattern as with `mix1`:
   */
  locally {
    
    implicit object canMixTogetherFooAndBar extends Compose[HasFoo, HasBar] {
      def apply(a: HasFoo, b: HasBar) = mix1(a, b)
    }
    
    val both2: HasFoo with HasBar = mix[HasFoo, HasBar](a, b)
    
  }
  
  /**
   * Indeed, for statically known types `A` and `B` we can materialize the
   * witness automatically by the means of a macro. Please note that in the
   * following example the implicit object `canMixTogetherFooAndBar` is not in
   * scope anymore, but the instance is materialized by our macro
   * implementation.
   */
  val both3: HasFoo with HasBar = mix[HasFoo, HasBar](a, b)


  /**
   * You may want to experiment with the macro by defining your own traits and
   * mixing them together, using `mix`.
   */


  /**
   * Our macro does not support methods with arguments, yet. However, this is
   * not a restriction in expressiveness, since the result of a method can be a
   * function type.
   */
  trait CanSayHey {
    def hey: Int => Unit
  }

  val c = new CanSayHey { def hey = (n: Int) => println(s"Hey, it's an $n") }

  def test {
    val fooAndHey = mix[HasFoo, CanSayHey](a, c)
    fooAndHey.hey(42)
    println(fooAndHey.foo)
  }
}
