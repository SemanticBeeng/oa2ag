package oa2ag.paper
package section6

object ast {

  case class Decl(name: Symbol, cons: Constructor*)
  case class Constructor(name: Symbol, fields: Field*)
  case class Field(name: Symbol, tpe: Type)

  trait Type
  trait BuiltIn extends Type
  case object StringT extends BuiltIn {
    override def toString = "String"
  }
  case object IntT extends BuiltIn {
    override def toString = "Int"
  }

  case object BooleanT extends BuiltIn {
    override def toString = "Boolean"
  }
  case class AppliedType(name: Symbol, arg: Type) extends Type
  case class UserDefined(name: Symbol) extends Type

  val string = StringT
  val int = IntT
  val bool = BooleanT

  def list(base: Type) = AppliedType('List, base)
  def option(base: Type) = AppliedType('Option, base)

  implicit def symToType(sym: Symbol): Type = UserDefined(sym)
  implicit def pairToField1(p: (Symbol, Type)) = Field(p._1, p._2)
  implicit def pairToField2(p: (Symbol, Symbol)) = Field(p._1, p._2)

}

class Generator(decl: ast.Decl) {

  import ast._

  implicit class StringOps(self: String) {
    def indent(n: Int) = self.lines.map ((" " * n) + _) mkString "\n"
  }

  /**
   * Utility for generation of type param lists  relative to a given fixed
   * declaration.
   */
  trait RelativeParams {
    def decl: Decl
    def inName: String

    lazy val inParams: Seq[String] = otherSortNames :+ inName
    lazy val covariantParams = inParams map {"-" + _} mkString ", "
    lazy val paramList = inParams mkString ", "
    lazy val anyParamList = inParams map {_ => "Any"} mkString ", "

    lazy val otherSortNames: Seq[String] = (for {
      con   <- decl.cons
      field <- con.fields
      tpe   <- guessSort(field.tpe)
      if tpe != decl.name.name
    } yield tpe).distinct

    lazy val otherSorts: Seq[Type] = (for {
      con   <- decl.cons
      field <- con.fields
      tpe   <- guessSort(field.tpe)
      if tpe != decl.name.name
    } yield field.tpe).distinct
  }

  /**
   * Utility for generation of method signatures and definitions
   */
  trait Operations {

    def outName: String

    def generateOpDecl(cons: Constructor): String =
      if (cons.fields.isEmpty)
        s"""def ${name(cons)}: $outName"""
      else
        s"""def ${name(cons)}: (${cons.fields map { fieldType(_) } mkString ", " }) => $outName"""

    def generateDepProductAssemble(cons: Constructor): String =
      s"""${generateOpDef(cons)} assemble(_,
         |  left.${name(cons)}${generateOpCall(cons)},
         |  right.${name(cons)}${generateOpCall(cons)})""".stripMargin

    def generateOpCall(cons: Constructor): String =
      if (cons.fields.isEmpty)
        ""
      else
        generateArgList(cons)

    def generateOpDef(cons: Constructor): String =
      if (cons.fields.isEmpty)
        s"""def ${name(cons)} ="""
      else
        s"""def ${name(cons)} = ${generateArgList(cons)} =>"""

    def generateArgList(cons: Constructor): String =
      s"""(${cons.fields map { name } mkString ", "})"""

    def name(field: Field): String = field.name.name
    def name(cons: Constructor): String = {
      val n = cons.name.name
      n(0).toLower + n.substring(1)
    }
  }

  case class Syntax(decl: Decl) {

    def generate: String =
      s"""trait Syntax
         |object Syntax {
         |${decl.cons map generate mkString "\n" indent 2}
         |}
         """.stripMargin

    def generate(cons: Constructor): String =
      (cons.name.name, cons.fields map generate mkString ", ") match {
        case (name, fields) => s"case class $name($fields) extends Syntax"
      }

    def generate(field: Field): String = s"${name(field)}: ${name(field.tpe)}"

    def name(field: Field): String = field.name.name

    def name(tpe: Type): String = tpe match {
      case UserDefined(name) if decl.name == name => "Syntax"
      case UserDefined(name) => name.name + ".Syntax"
      case AppliedType(n, arg) => s"${n.name}[${name(arg)}]"
      case b: BuiltIn => b.toString
    }
  }

  case class Algebra(decl: Decl)(
      val sigName: String = "Signature",
      val inName: String = decl.name.name,
      val outName: String = "Out") extends RelativeParams with Operations {

    def generate: String =
      s"""trait $sigName[$covariantParams, +$outName] {
         |${decl.cons map generateOpDecl mkString "\n" indent 2}
         |}
         |
         |object $sigName {
         |
         |  // Operations for signatures with function type as Out
         |  implicit class SigOps[${paramsFor(1)}, Self1, Out1]
         |    (alg1: $sigName[${paramsFor(1)}, Self1 => Out1]) {
         |
         |    // Impose additional requirements on the argument type
         |    def apply[Req]: $sigName[${paramsFor(1)}, (Req with Self1) => Out1] = alg1
         |
         |    // DepProduct - A combinator to compose signatures
         |    def <+[${paramsFor(2)}, Self2 >: Self1 with Out1, Out2](
         |        alg2: $sigName[${paramsFor(2)}, Self2 => Out2])(implicit
         |        comp1: Compose[Out1, Out2],
         |        comp2: Compose[Self1, Out1]) =
         |      new DepProduct[
         |          ${paramsFor(1)}, Self1, Out1,
         |          ${paramsFor(2)}, Self2, Out2] {
         |        val (left, right, compose1, compose2) = (alg1, alg2, comp1, comp2)
         |      }
         |  }
         |
         |  trait DepProduct[
         |    ${paramsFor(1)}, Self1, Out1,
         |    ${paramsFor(2)}, Self2 >: Self1 with Out1, Out2]
         |      extends $sigName[${inParams map { p => p + 1 + " with " + p + 2 } mkString ", "}, Self1 => Out1 with Out2]
         |      with DepProductHelper[Self1, Out1, Out2] {
         |
         |    val left: $sigName[${paramsFor(1)}, Self1 => Out1]
         |    val right: $sigName[${paramsFor(2)}, Self2 => Out2]
         |
         |${decl.cons map generateDepProductAssemble mkString "\n\n" indent(4)}
         |  }
         |}
         |""".stripMargin


    def generateUtility(name: String): String = {

      s"""type $name[$covariantParams, -Self, +Out] =
         |  $sigName[$paramList, Self => Out]
         |
         |object $name {
         |
         |  /**
         |   * Decorates the output of an algebra
         |   *
         |   *     Alg[Any, A, B] <+ Decorate(B => C) == Alg[Any, A, B with C]
         |   */
         |  trait Decorate[$covariantParams, -S, +T]
         |      extends $sigName[$paramList, S => T] {
         |
         |    val transform: S => T
         |
         |${decl.cons map { generateOpDef(_) + " transform" } mkString "\n" indent 4}
         |  }
         |  def Decorate[S, T](f: S => T) = new Decorate[$anyParamList, S, T] {
         |    val transform = f
         |  }
         |
         |  trait Default[$covariantParams, -S <: T, +T] extends Decorate[$paramList, S, T] {
         |    val transform: S => T = identity
         |  }
         |  def Default[T] = new Default[$anyParamList, T, T] {}
         |
         |  def Dummy[$paramList, S, T] = new Decorate[$paramList, S, T] {
         |    val transform = (self: S) => ???
         |  }
         |
         |  def Require[Req] = new Default[$anyParamList, Req, Any] {}
         |}""".stripMargin
    }

    def generateAlias: String =
      s"""type Algebra[$paramList] = $sigName[$paramList, $inName]"""

    // example output: "Expr1, Exprs1, Stmt1"
    def paramsFor(n: Int) = inParams map {_ + n } mkString ", "

    def name(decl: Decl): String = decl.name.name
  }

  case class AttributeGrammar(decl: Decl, inhs: List[Algebra])
      extends RelativeParams with Operations {

    def inName = decl.name.name
    def outName = ""

    def generate: String =
      s"""
        |/**
        | * Instances of this trait represent the corresponding attribute grammar
        | * and can be used either in polymorphic embedding style or for folding.
        | */
        |trait AttributeGrammar[$iaSaParamList] extends Algebra[$iaToSaParamList] {
        |${inhs map { inhVal } mkString "\n" indent 2}
        |  val syn: $synSignature
        |
        |  val compose: Compose[IA, SA]
        |
        |  // Implementation of the operations by delegation to the inh. and
        |  // syn. attributes:
        |  // Please note that the infix compose is really just ⋅
        |${decl.cons map { generateAttributeComposition } mkString "\n\n" indent 2}
        |}
        |
        |trait Foldable[$paramList] extends (Syntax => $inName) {
        |     self: Algebra[$paramList] =>
        |
        |${otherSorts map depAlgVal mkString "\n" indent 2}
        |
        |  def apply(t: Syntax): $inName = t match {
        |${decl.cons map generateFold mkString "\n" indent 4}
        |  }
        |}
        |
        |def apply[$iaSaParamList](
        |${attributeParams mkString ",\n" indent 4})
        |  (implicit
        |    _compose: Compose[IA, SA]) =
        |  new AttributeGrammar[$iaSaParamList]
        |      with Foldable[$iaToSaParamList] {
        |    lazy val (${memberAssignments mkString ", "}) =
        |      (${memberAssignments map { "_" + _ } mkString ", "})
        |  }
        |""".stripMargin

    val memberAssignments: Seq[String] =
      (otherSorts map { depAlgName(_) }) ++
      (inhs map { inhValName }) ++ Seq("syn", "compose")

    /**
     * Methods for generation of composition mechanism
     */

    /** This is very similar to transformInh! */
    def generateAttributeComposition(c: Constructor): String = {

      val fields = c.fields.toList

      lazy val subComputations = for {
        (pre, Field(Symbol(n), tpe)) <- fields.inits.toList.reverse zip fields
        respInh <- guessSort(tpe)
      } yield s"""val ${n}Out = ($n compose inh${respInh}.${name(c)}_${n}${processedArgList(pre)})(attr)"""

      def processedFields(fields: Seq[Field]): Seq[String] = for {
        f @ Field(_, tpe) <- fields
      } yield (guessSort(tpe) map { _ => name(f) + "Out" } getOrElse (name(f)))

      def processedArgList(fields: Seq[Field]): String =
        if (fields.isEmpty) "" else s"""(${processedFields(fields) mkString ", "})"""

      s"""${generateOpDef(c)} attr => compose(attr, {
         |${subComputations mkString "\n" indent 2}
         |  syn.${name(c)}${processedArgList(fields)}(attr)
         |})""".stripMargin
    }

    def inhVal(inh: Algebra): String =
      s"""val ${inhValName(inh.decl)}: ${inhSignature(inh)}"""

    def inhParam(inh: Algebra): String =
      s"""_${inhValName(inh.decl)}: ${inhSignature(inh)}"""

    lazy val attributeParams: Seq[String] =
      (otherSorts map { depAlgParam }) ++
      (inhs map { inhParam }) ++ Seq(s"_syn: $synSignature")

    lazy val iaSaParamList = inParams map {
      case n if n == decl.name.name => "IA, SA"
      case n => s"${n}IA, ${n}SA"
    } mkString ", "

    def iaWithSa(name: String): String =
      if (name == decl.name.name)
        "IA with SA"
      else
        s"${name}IA with ${name}SA"

    def iaToIa(name: String): String =
      if (name == decl.name.name)
        "IA => IA"
      else
        s"IA => ${name}IA"

    def iaToIaWithSA(name: String): String =
      if (name == decl.name.name)
        "IA => IA with SA"
      else
        s"${name}IA => ${name}IA with ${name}SA"

    def iaToSaParamList: String =
      (inParams :+ inName).distinct map { iaToIaWithSA } mkString ", "

    def inhSignature(inh: Algebra): String =
      s"""${inh.sigName}[${inh.inParams map iaWithSa mkString ", "}, ${iaToIa(inh.decl.name.name)}]"""

    def synSignature: String =
      s"""Signature[${inParams map iaWithSa mkString ", "}, IA => SA]"""


    /**
     * Methods for generation of "fold"
     */

    def depAlgSyntaxName(tpe: Type): String = tpe match {
      case UserDefined(name) if decl.name == name => "Syntax"
      case UserDefined(name) => s"${name.name}.Syntax"
      case AppliedType(name, arg) => s"${name.name}[${depAlgSyntaxName(arg)}]"
      case b: BuiltIn => ""
    }

    def depAlgVal(tpe: Type): String = {
      s"""def ${depAlgName(tpe)}: ${depAlgSyntaxName(tpe)} => ${depAlgTypeName(tpe)}"""
    }

    def depAlgParam(tpe: Type): String = {
      s"""_${depAlgName(tpe)}: => (${depAlgSyntaxName(tpe)} => ${iaToIaWithSA(depAlgTypeName(tpe))})"""
    }

    // Naming scheme for dependency algebras
    def depAlgTypeName(t: Type): String = guessSort(t).get
    def depAlgName(n: String): String = n.toLowerCase + "Alg"
    def depAlgName(t: Type): String = depAlgName(depAlgTypeName(t))

    def generateFold(c: Constructor): String = {
      val params = for {
        Field(Symbol(name), tpe) <- c.fields.toList
        resFold = responsibleFold(tpe)
      } yield resFold map { _ + s"($name)" } getOrElse (name)

      val paramsList = params match {
        case Nil => ""
        case l => s"""(${l mkString ", "})"""
      }

      s"""case Syntax.${c.name.name}${generateArgList(c)} => ${name(c)}$paramsList"""
    }

    def responsibleFold(t: Type): Option[String] = guessSort(t) match {
      case Some(n) if n == decl.name.name => Some("this")
      case Some(n) => Some(depAlgName(n))
      case None => None
    }
  }


  // Naming scheme for inherited attributes
  def inhName(inh: Decl): String = "Inh" + inh.name.name
  def inhSigName(inh: Decl): String = s"Inh${inh.name.name}Sig"
  def inhValName(inh: Decl): String = "inh" + inh.name.name
  def inhValName(inh: Algebra): String = "inh" + inh.decl.name.name
  def inhOutName(inh: Decl): String = inh.name.name + "Out"

  def guessSort(t: Type): Option[String] = t match {
    case UserDefined(name) => Some(name.name)
    case AppliedType('List, arg) => guessSort(arg) map { _ + "s" }
    case AppliedType(name, arg) => guessSort(arg) map { _ + name.name }
    case _ => None
  }

  def fieldType(field: Field): String =
    guessSort(field.tpe) getOrElse name(field.tpe)

  def transformToInh(decl: Decl): Iterable[Decl] = {

    val prods = for {
      con <- decl.cons
      fields = con.fields.toList
      (pre, Field(Symbol(name), tpe)) <- fields.inits.toList.reverse zip fields
      sort <- guessSort(tpe)
    } yield (sort, Constructor(Symbol(con.name.name + "_" + name), pre: _*))

    // Group by output (one algebra per output sort)
    // i.e. Map("Expr" -> Seq(Production(...)), "Stmt" -> Seq(...))
    val algebras = prods.groupBy(_._1).mapValues(_.map {_._2})

    algebras map {
      case (name, cons) => Decl(Symbol(name), cons: _*)
    }
  }

  val defaultPackage =
    """package updownalgebras
      |package generated""".stripMargin

  def generate(prelude: String = defaultPackage): String = {
    val syntax = Syntax(decl)

    val algebra = Algebra(decl)()

    val inhs = transformToInh(decl).toList

    val inhAlgs = inhs.map { case decl =>
      val name = decl.name.name
      Algebra(decl)(inhSigName(decl), name, inhOutName(decl))
    }

    val synUtility = algebra.generateUtility("Syn")
    val inhUtility =
      if (inhs.size == 1)
        inhAlgs.head.generateUtility("Inh") :: Nil
      else
        inhAlgs.map { inh => inh.generateUtility(inhName(inh.decl)) }

    val composition = AttributeGrammar(decl, inhAlgs)

    s"""$prelude
       |
       |/**
       | * ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
       | * ┃ THIS FILE HAS AUTOMATICALLY BEEN GENERATED!                      ┃
       | * ┠──────────────────────────────────────────────────────────────────┨
       | * ┃ If you consider changing this file, please be aware that your    ┃
       | * ┃ modifications will be overwritten when regenerating.             ┃
       | * ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
       | *
       | *
       | * Every nonterminal / object algebra consists of three parts:
       | *
       | * 1. Signature - the signature of the algebra used for syn. attributes
       | *    and the finally composed algebra itself.
       | *
       | * 2. InhSignatures - the transformed signatures for specification of
       | *    inh.  attributes, split by the output sort.
       | *
       | * 3. AttributeGrammar - the composition mechanism that allows combining
       | *    syn + inh* to a foldable algebra / attribute grammar.
       | */
       |object ${name(decl)} extends Nonterminal {
       |
       |
       |  /**
       |   * 0. Syntax
       |   * ---------
       |   */
       |${syntax.generate indent 2}
       |
       |  /**
       |   * 1. The Language Interface
       |   * -------------------------
       |   */
       |${algebra.generate indent 2}
       |
       |
       |  /**
       |   * 2. Algebras for Inherited Attributes
       |   * ------------------------------------
       |   */
       |${inhAlgs map { _.generate } mkString "\n\n" indent 2}
       |
       |
       |  /**
       |   * 3. The Composition Mechanism
       |   * ----------------------------
       |   */
       |${composition.generate indent 2}
       |
       |
       |  /**
       |   * 4. Attribute Definition Utility
       |   * -------------------------------
       |   * The user interface for attribute definition.
       |   */
       |${algebra.generateAlias indent 2}
       |${(synUtility :: inhUtility) mkString "\n\n" indent 2}
       |}
       |""".stripMargin
  }

  def name(decl: Decl): String = decl.name.name

  def name(tpe: Type): String = tpe match {
    case UserDefined(name) => name.name
    case AppliedType(n, arg) => s"${n.name}[${name(arg)}]"
    case b: BuiltIn => b.toString
  }
}
object Generator {
  import ast.Decl
  import java.io.{ File, PrintWriter }

  def apply(decl: Decl, filename: String, prelude: String): File =
    apply(decl, new File(filename), prelude)

  def apply(decl: Decl, file: File, prelude: String): File = {
    val f = new PrintWriter(file)

    try {
      f.write(new Generator(decl).generate(prelude))
    } finally {
      f.close
    }
    file
  }
}