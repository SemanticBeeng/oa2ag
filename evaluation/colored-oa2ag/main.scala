// taken from `program/compiler.scala
 def parseAndCompile(in: String): String = 
  compiler.parse(HasNothing)(new TokenStream(Scanner(new StringReader(in)))).code mkString ("\n")

def parseAndCompileFile(filename: String): String =
  compiler.parse(HasNothing)(new TokenStream(Scanner(new FileReader(filename)))).code mkString ("\n")

def compile(t: trees.Program): String = compiler(t, HasNothing).code mkString ("\n")
