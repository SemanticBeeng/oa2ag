package oa2ag
package czero
package examples.compiler

import scala.language.implicitConversions

object test {
  import program._
  import trees._

  val prog = Prog(List(
    FunDef("sayHello", List("num"), Nil, ReturnStmt(IdExpr("num")) :: Nil),

    FunDef("helloWorld", List("a", "b"),
      IntDecl("c", None) :: Nil, 
      List(
        IfStmt(BinaryOpExpr(IdExpr("a"), "==", IdExpr("b")),
          List(PutCharStmt(CallExpr("sayHello", List(IdExpr("a"))))),
          List(PutCharStmt(CallExpr("sayHello", List(IdExpr("c")))))),
        AssignStmt("c", BinaryOpExpr(IdExpr("a"), "+", IdExpr("b"))),
        ReturnStmt(IdExpr("c"))))
  ))

  val ast = Prog(List(
    FunDecl("helloWorld", List("a", "b")),
    FunDef("helloWorld", List("a", "b"), 
      IntDecl("c", Some(42)) :: Nil,
      AssignStmt("c", 
        BinaryOpExpr(IdExpr("a"), "+", CallExpr("helloWorld", 
          List(IdExpr("a"), IdExpr("b"))))) :: Nil)))

  // println(compile(prog))

  println(compile(ast))

  println("----")

  println(parseAndCompile("""
    #include <stdio.h>

    int helloWorld(int a, int b); 
    int helloWorld(int a, int b) { 
      int c = 42; 
      c = a + helloWorld(a, b);
      if (c == 42) {
        putchar('h');
        putchar('e');
        putchar('l');
        putchar('l');
        putchar('o');
      } else {}
    }

    main() {}
  """))
}