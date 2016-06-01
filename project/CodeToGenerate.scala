package generator

import sbt._
import Keys._

object CodeToGenerate {

  import ast._
  import sbt.Path._

  lazy val generateLcCode = taskKey[Seq[File]]("Generates the code for the lc example")

  /** LC and Statements **/

  val expr = Decl('Expr,
    Constructor('Const, 'value -> int),
    Constructor('Add, 'lhs -> 'Expr, 'rhs -> 'Expr),
    Constructor('Id, 'name -> string),
    Constructor('Lambda, 'name -> string, 'body -> 'Expr),
    Constructor('Call, 'fun -> 'Expr, 'arg -> 'Expr))

  val stmt = Decl('Stmt,
    Constructor('Assign, 'name -> string, 'value -> 'Expr),
    Constructor('Seq, 'head -> 'Stmt, 'tail -> 'Stmt),
    Constructor('ExprStmt, 'exp -> 'Expr))

  val packageDecl = """package oa2ag
                      |package lc""".stripMargin

  val lcExample = (generateLcCode := {
    val path = baseDirectory.value / "src" / "main" / "scala" / "lc"
    println("Generating LC Code")
    Generator(expr, path / "expr.gen.scala", packageDecl) ::
    Generator(stmt, path / "stmt.gen.scala", packageDecl) :: Nil
  })

  /** LC with Let **/
  lazy val generateLetCode = taskKey[Seq[File]]("Generates the code for the let extension")

  val let = Decl('Let,
    Constructor('Let, 'name -> string, 'exp -> 'Let, 'body -> 'Let))

  val letExample = (generateLetCode := {
    val path = baseDirectory.value / "src" / "main" / "scala" / "lc"
    println("Generating Let Code")
    Generator(let, path / "let.gen.scala", packageDecl) :: Nil
  })


  /** STLC **/
  lazy val generateStlcCode = taskKey[Seq[File]]("Generates the code for the stlc example")

  val packageDeclStlc = """package oa2ag
                    |package lc
                    |package typed""".stripMargin


  val typedExpr = Decl('Expr,
    Constructor('Const, 'value -> int),
    Constructor('Add, 'lhs -> 'Expr, 'rhs -> 'Expr),
    Constructor('Id, 'name -> string),
    Constructor('Lambda, 'name -> string, 'tpe -> 'Type, 'body -> 'Expr),
    Constructor('Call, 'fun -> 'Expr, 'arg -> 'Expr))

  val typedType = Decl('Type,
    Constructor('IntT),
    Constructor('ArrowT, 'src -> 'Type, 'trg -> 'Type))

  val stlcExample = (generateStlcCode := {
    val path = baseDirectory.value / "src" / "main" / "scala" / "lc" / "typed"
    println("Generating STLC Code")
    Generator(typedExpr, path / "expr.gen.scala", packageDeclStlc) ::
    Generator(typedType, path / "type.gen.scala", packageDeclStlc) :: Nil
  })


  /** Czero */

  lazy val generateCZeroCode = taskKey[Seq[File]]("Generates the code for the czero case study")

  val czero = {
    val packageDecl =
      """package oa2ag
        |package czeronew"""


    val program = Decl('Program,
      Constructor('Prog, 'funcs -> list('Function)))

    val function = Decl('Function,
      Constructor('FunDecl, 'id -> string, 'formals -> list(string)),
      Constructor('FunDef, 'id -> string, 'formals -> list(string),
        'decls -> list('Declaration), 'stmts -> list('Statement)))

    val declaration = Decl('Declaration,
      Constructor('IntDecl, 'id -> string, 'init -> option(int)))

    val statement = Decl('Statement,
      Constructor('AssignStmt, 'id -> string, 'value -> 'Expression),
      Constructor('WhileStmt, 'cond -> 'Expression, 'body -> list('Statement)),
      Constructor('PutCharStmt, 'char -> 'Expression),
      Constructor('ReturnStmt, 'value -> 'Expression),
      Constructor('IfStmt, 'cond -> 'Expression,
        'thn -> list('Statement), 'els -> list('Statement)))

    val expression = Decl('Expression,
      Constructor('ConstExpr, 'value -> int),
      Constructor('IdExpr, 'id -> string),
      Constructor('PrefixExpr, 'op -> string, 'body -> 'Expression),
      Constructor('CallExpr, 'funName -> string, 'args -> list('Expression)),
      Constructor('BinaryOpExpr, 'lhs -> 'Expression, 'op -> string, 'rhs -> 'Expression))

    generateCZeroCode := {
      val path = baseDirectory.value / "src" / "main" / "scala" / "czeronew" / "generated"
      println("Generating CZero code")
      Generator(program, path / "program.gen.scala", packageDecl) ::
      Generator(function, path / "function.gen.scala", packageDecl) ::
      Generator(declaration, path / "declaration.gen.scala", packageDecl) ::
      Generator(statement, path / "statement.gen.scala", packageDecl) ::
      Generator(expression, path / "expression.gen.scala", packageDecl) :: Nil
    }
  }

  /** Cons Lists */
  val generateConsListCode = taskKey[Seq[File]]("Generates the basis code for cons lists")
  val consList = {
    val packageDecl =
      """package oa2ag
        |package czeronew"""

    val list = Decl('ConsList,
      Constructor('Empty),
      Constructor('Cons, 'head -> 'Elem, 'tail -> 'ConsList))

    generateConsListCode := {
      val path = baseDirectory.value / "src" / "main" / "scala" / "czeronew"
      println("Generating ConsList basis code")
      Generator(list, path / "ConsList.scala", packageDecl) :: Nil
    }
  }

  /** Type Reconstruction */
  val generateTypeReCode = taskKey[Seq[File]]("Generates the code for lc with let and if")

  val typeRe = {
    val packageDecl = """package oa2ag
      |package typereconstruction""".stripMargin

    val expr = Decl('Expr,
      Constructor('VarExpr, 'name -> string),
      Constructor('AbsExpr, 'name -> string, 'body -> 'Expr),
      Constructor('AppExpr, 'fun -> 'Expr, 'arg -> 'Expr),
      Constructor('NumExpr, 'value -> int),
      Constructor('SuccExpr, 'base -> 'Expr),
      Constructor('PredExpr, 'base -> 'Expr),
      Constructor('IsZeroExpr, 'base -> 'Expr),
      Constructor('BoolExpr, 'value -> bool),
      Constructor('IfExpr, 'cond -> 'Expr, 'thn -> 'Expr, 'els -> 'Expr))


    generateTypeReCode := {
      val path = baseDirectory.value / "src" / "main" / "scala" / "typereconstruction"
      println("Generating type reconstruction code")
      Generator(expr, path / "Expr.gen.scala", packageDecl) :: Nil
    }
  }

    /** Examples from the paper */
  val generatePaperCode = taskKey[Seq[File]]("Generates the support code for the paper examples")
  val paper = {
    val packageDecl =
      """package oa2ag
        |package paper"""

    val sec3to5expr = Decl('Expr,
      Constructor('Lit, 'value -> int),
      Constructor('Add, 'lhs -> 'Expr, 'rhs -> 'Expr)
    )

    val sec6expr = Decl('Expr,
      Constructor('Lit, 'value -> int),
      Constructor('Add, 'lhs -> 'Expr, 'rhs -> 'Expr),
      Constructor('Vari, 'name -> string))

    val sec6stmt = Decl('Stmt,
      Constructor('Set, 'name -> string, 'exp -> 'Expr, 'body -> 'Stmt),
      Constructor('Exp, 'value -> 'Expr))

    generatePaperCode := {
      val path = baseDirectory.value / "src" / "main" / "scala" / "paper"
      println("Generating paper code")
      Generator(sec3to5expr, path / "generator" / "Expr.gen.scala", packageDecl + ".generator") ::
      Generator(sec6expr, path / "section6" /" Expr.gen.scala", packageDecl + ".section6") ::
      Generator(sec6stmt, path / "section6" /" Stmt.gen.scala", packageDecl + ".section6") :: Nil
    }
  }

  val settings = Seq(lcExample, letExample, stlcExample, czero, consList, paper, typeRe)
}
