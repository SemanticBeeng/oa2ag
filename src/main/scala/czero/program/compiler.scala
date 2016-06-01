package oa2ag
package czero
package examples.compiler

package object program {
  
  import equations.{ ProgramCode, FuncListInit }
  import parsers.{ ProgramParser, TokenStream }
  import java.io.{ FileReader, StringReader }
  
  lazy val compiler = ProgramParser(functionList.compiler, ProgramCode, FuncListInit)
  
  def parseAndCompile(in: String): String = 
    compiler.parse(HasNothing)(new TokenStream(Scanner(new StringReader(in)))).code mkString ("\n")

  def parseAndCompileFile(filename: String): String =
    compiler.parse(HasNothing)(new TokenStream(Scanner(new FileReader(filename)))).code mkString ("\n")

  def compile(t: trees.Program): String = compiler(t, HasNothing).code mkString ("\n")
}
