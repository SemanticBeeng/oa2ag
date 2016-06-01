Example: C0 Case Study
======================
The support code for this example is based on the following signature:

    Prog: FuncList → Program

    FunDecl: String × List[String] → Function
    FunDef: String × List[String] × DeclList × StatementList → Function

    Empty: FuncList
    Cons: Function × FuncList → FuncList

    IntDecl: String × Option[Int] → Declaration

    Empty: DeclList
    Cons: Declaration × DeclList → DeclList

    AssignStmt: String × Expression → Statement
    WhileStmt: Expression × StmtList → Statement
    PutCharStmt: Expression → Statement
    ReturnStmt: Expression → Statement
    IfStmt: Expression × StmtList × StmtList → Statement

    Empty: StmtList
    Cons: Statement × StmtList → StmtList

    ConstExpr: Int → Expression
    IdExpr: String → Expression
    PrefixExpr: String × Expression → Expression
    CallExpr: String × ArgList → Expression
    BinaryOpExpr: Expression × String × Expression → Expression

    Empty: ArgList
    Cons: Expression × ArgList → ArgList


In order to compile C0 files you can run the following commands:

~~~
> sbt console
Welcome to Scala version 2.10.3

scala> import czero.examples.compiler._

scala> program.parseAndCompileFile("src/test/resources/helloworld.c")
res1: String =
".method mul_
.args 3
.locals 2
bipush 1
istore 4
...
~~~

This will first import all attributes packaged per nonterminal and then invoke `parseAndCompileFile` on the [`program`](program/compiler.scala) module (corresponding to the start symbol).


The following table illustrates the mapping between names used in the paper
and names in the source files of this example:

| Paper           | This Example                |
|-----------------|-----------------------------|
| `mix`           | `Compose`                   |
| `ExprAssemble`  | `Expr.AttributeGrammar`     |
| `ExprCompose`   | `Expr.Signature.DepProduct` |
| `PreExprSig`    | `Expr.Signature`            |
| `ExprSig`       | `Expr.Syn.Complete`         |
| `InhSig`        | `Expr.Inh.Complete`         |
| `CtxExprSig`    | `Expr.Syn`                  |
| `CtxInhSig`     | `Expr.Inh`                  |