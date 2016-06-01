package oa2ag
package czero
package examples.compiler

package object declaration {
  
  import attributes.{ HasCode, HasOffset, HasVariables }
  import equations.{ DeclarationVariables, DeclarationCode }
  import algebras.Declaration.ops._
  import parsers.{ DeclarationParser, ParsingAlgebra }

  lazy val compiler: ParsingAlgebra[trees.Declaration, 
      HasVariables with HasCode, 
      HasVariables with HasOffset] = 
        DeclarationParser(DeclarationVariables <+ DeclarationCode)
  
}
