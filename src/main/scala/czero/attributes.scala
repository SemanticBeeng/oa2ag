package oa2ag
package czero

object attributes {

  // inh
  trait HasFunctions extends HasNothing {
    // name => args
    val functions: Map[String, List[String]]
    val prototypes: Map[String, List[String]]
  }
  object HasFunctions {
    def apply(funs: Map[String, List[String]], protos: Map[String, List[String]]) = new HasFunctions { 
      val functions = funs
      val prototypes = protos
    }
  }

  // inh
  trait HasVariables extends HasNothing {
    // name => offset
    val variables: Map[String, Int]
  }
  implicit def HasVariables(vars: Map[String, Int]) = new HasVariables {
    val variables = vars
  }

  // inh
  trait HasOffset extends HasNothing {
    val offset: Int
  }
  implicit def HasOffset(off: Int) = new HasOffset { val offset = off }

  // syn
  trait HasCode extends HasNothing {
    val code: Seq[String]
  }
  implicit def HasCode(c: Seq[String]) = new HasCode { val code = c }

  // syn
  trait HasCount extends HasNothing {
    val count: Int
  }
  implicit def HasCount(c: Int) = new HasCount { val count = c }
}
