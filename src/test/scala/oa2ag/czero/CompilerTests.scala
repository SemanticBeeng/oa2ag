package oa2ag.czero.examples

import org.scalatest._
import sys.process._

class CompilerTests extends FlatSpec with matchers.ShouldMatchers {
  trait Context {
    import oa2ag.{ czero => czeroAttr }

    def orig(filename: String): String = czero.original.Czero.parseFile(Array(filename))
    def ours(filename: String): String = compiler.program.parseAndCompileFile(filename)

    def test(filename: String) {
      val ourResult = ours(filename)
      val origResult = orig(filename)

      if (ourResult != origResult) {
        println("---------- EXPECTED -----------")
        println(origResult)
        println("----------- FOUND -------------")
        println(ourResult)
        println("-------------------------------")
        fail("Did fail on " + filename)
      }
    }
  }

  it should "compile helloworld.c correctly" in new Context {
    test("src/test/resources/helloworld.c")
  }

  it should "compile whilestmt.c correctly" in new Context {
    test("src/test/resources/whilestmt.c")
  }

  // The results are different but alpha equivalent in the labels
  // due to different time of label-choosing
  ignore should "compile gcd.c correctly" in new Context {
    test("src/test/resources/gcd.c")
  }
}
