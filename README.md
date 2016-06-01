From Object Algebras to Attribute Grammars
==========================================
This website contains the source files for our experiments on encoding attribute grammars as
object algebras. The experiments form the basis of our paper:

<div id=paper>

  **From Object Algebras to Attribute Grammars**
  by Tillmann Rendel, Jonathan Immanuel Brachthäuser, Klaus Ostermann.
  To appear in *Proceedings of the Conference on Object-Oriented Programming, Systems, Languages & Applications, 2014*.

  [Download the pdf](rendel14object.pdf)

</div>

There are two ways to inspect the sources:

  1. by browsing them online on this page,
  2. by downloading the [packaged zip file][1].

### Code Accompanying the Paper

It might be most convenient to start with the files in the folder
[`paper`](src/main/scala/paper). Those files are accompanying the paper sections
3 to 5 which develop the encoding by example. They are intended to be read
alongside the paper. Each file in the folder is named after the corresponding
section and contains additional examples and documentation. The implementation
also allows to run the examples from the paper.

### The Code Generator

To get a deeper understanding of the formalization in section 6 we suggest to
consult the files related to the code generator. The code generator has been
developed in parallel with the formalization and thus can aid the understanding.
However, be warned, the terminology of the code generator is not yet consistent
with the one introduced in the paper. The following table provides a short
overview of the differences.

| Paper           | Code Generator              |
|-----------------|-----------------------------|
| `mix`           | `Compose`                   |
| `ExprAssemble`  | `Expr.AttributeGrammar`     |
| `ExprCompose`   | `Expr.Signature.DepProduct` |
| `PreExprSig`    | `Expr.Signature`            |
| `ExprSig`       | `Expr.Syn.Complete`         |
| `InhSig`        | `Expr.Inh.Complete`         |
| `CtxExprSig`    | `Expr.Syn`                  |
| `CtxInhSig`     | `Expr.Inh`                  |

Almost all examples from the files accompanying section 3 to 5 are translated to
make use of the code generator in folder
[`paper/generator`](src/main/scala/paper/generator). Comparing `paper` with
`paper/generator` might help understanding the differences in terminology.
[`paper/section6.scala`](src/main/scala/paper/section6.scala) contains a more
in-depth tutorial to the code generator.


| File / Folder              | Description                                     |
|----------------------------|-------------------------------------------------|
| [`paper/section6.scala`](src/main/scala/paper/section6.scala) | Introduction into the code generator. |
| [`paper/generator`](src/main/scala/paper/generator) | Examples from [`paper`](src/main/scala/paper) translated to use the generator.
| [`project/Generator.scala`](project/Generator.scala)  | Generator for support code (Implementation of the formalization as found in section 6). |
| [`project/CodeToGenerate.scala`](project/CodeToGenerate.scala)     | The input for the generator.                     |


### Case studies in `src/main/scala/`
All case studies but the last one listed here have been conducted prior to the submission. While packaging the code we rewrote parts of an already existing case study resulting in the ["typereconstruction"](src/main/scala/typereconstruction) example. All case studies are located in `src/main/scala/`.


| File / Folder          | Description                                         |
|:-----------------------|:----------------------------------------------------|
| [`czero/`](src/main/scala/czero) | Code for the czero case study (section 7)           |
| [`lc/higherorder.scala`](src/main/scala/lc/higherorder.scala) | Advanced examples related to section 8              |
| [`lc/plain.scala`](src/main/scala/lc/plain.scala), [`lc/typed/examples.scala`](src/main/scala/lc/typed/examples.scala) | More examples using single and multi sorted algebras |
| [`typereconstruction/`](src/main/scala/typereconstruction)  | Example, based on a single sorted algebra, illustrating the use of separately defined attributes and their composition |


### Support Code

In the REPL started with `sbt console` (see below) our macro implementation of the right-biased object composition
function (called `mix` in the paper, but called `Compose` in the code) can be used as follows:

~~~
scala> :paste
// Entering paste mode (ctrl-D to finish)

trait A { def answer = 42 }
trait B { def double = 84 }
val a = new A {}
val b = new B {}

// Exiting paste mode, now interpreting.

b: B = $anon$1@7fb74002
scala> Compose(a, b).answer
res1: Int = 42

scala> Compose(a, b).double
res2: Int = 84
~~~

Also see [`mix.scala`](src/main/scala/paper/mix.scala) for a short introduction.

*The [mixin composition macro][3] is now maintained as a separate project. The project page also contains further [documentation][3].*



Setting up and building the project
-----------------------------------
In order to build the project, please install [sbt >=
0.13][2]. Unpack the downloaded [zip file][1] containing the source code and open a
terminal in the unpacked folder. Enter `sbt console`. The necessary support code will be generated and a scala REPL starts.

Some examples are packaged in Scala objects and will be evaluated once the
object is referenced, others are simply methods that can be executed multiple
times. For example

~~~
scala> paper.section3.subsection1.test
8
~~~

calls the method `test` on the object `subsection1` located in package
`oa2ag.paper.section3`. It uses the object algebra from figure 3 to evaluate
`3 + 5`.

As one can see, the package `oa2ag` can be omitted since it is already being
imported after starting the console.

[1]: http://www.informatik.uni-marburg.de/~rendel/oa2ag/rendel14object.zip
[2]: http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html
[3]: https://github.com/b-studios/MixinComposition