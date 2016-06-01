/* This is an attempt to encode the TINY example 
for reference attribute grammars as described in
the following paper:

G. Hedin. Reference Attributed Grammars. Informatica 24(2000) 301-317, Slovenia
http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.200.149

The specification of the TINY RAG is in Fig. 1 of that paper. 

The code typechecks and works as expected. But it is rather complicated.
*/

trait ADownAlg[-B,-C,-Self,+Out] {
  def bc(b: => B, c: => C, self: => Self): Out
}
trait BDownAlg[-Self,+Out] {
  def empty(self: => Self): Out
}
trait CDownAlg[-Self,+Out] {
  def empty(self: => Self): Out
}

trait UpAlgB[-B,-C,-Self,+Out] {
  def ab1(b: => B, c: => C, self: => Self): Out
}

trait UpAlgC[-B,-C,-Self,+Out] {
  def ab2(b: => B, c: => C, self: => Self): Out
}

trait hasNothing

trait hasb extends hasNothing {
  def b: Int
}
trait hasrC extends hasNothing{
  def rc: CIfc
}
trait BIfc extends hasb with hasrC

trait hasrB extends hasNothing {
  def rb: BIfc
}
trait hasc extends hasNothing{
  def c: Int
}
trait CIfc extends hasrB with hasc  


object cattr extends CDownAlg[hasNothing, hasc] {
  def empty(self: => hasNothing) = new hasc { def c = 7 }
}  

object battr extends BDownAlg[hasrC,hasb] {
  def empty(self: => hasrC) = new hasb { def b = self.rc.c }
}

object rCAttr extends UpAlgB[hasNothing,CIfc,hasNothing,hasrC] {
  def ab1(b: => hasNothing, c: => CIfc,self: => hasNothing) = new hasrC { def rc = c }
}  

object rBAttr extends UpAlgC[BIfc,hasNothing,hasNothing,hasrB] {
  def ab2(b: => BIfc, c: => hasNothing,self: => hasNothing) = new hasrB { def rb = b }
}  

// This attribute is not in the example in the paper. Its purpose is to make
// the result visible on the top level node.
object abattr extends ADownAlg[hasb,hasc,hasNothing, hasb with hasc] {
 def bc(bb: => hasb, cc: => hasc, self: => hasNothing) = new hasb with hasc { def b = bb.b; def c = cc.c; }
}

case class CompleteAlg[A,B,C, B1 >: B, C1 >: C, B2 >: B, C2 >: C](ada: ADownAlg[B,C,A,A], bda: BDownAlg[B,B1], cda: CDownAlg[C,C1], uab: UpAlgB[B,C,A,B2], uac: UpAlgC[B,C,A,C2])


object Main {
    def fold[A,B,C,B1>:B,C1>:C,B2>:B,C2>:C](alg:CompleteAlg[A,B,C,B1,C1,B2,C2], compose1: ( => B,B1) => B, compose2: (=>C,C1) => C, compose3: (=>B,B2) => B, compose4: (=>C,C2) => C) : A = {
      lazy val b : B = compose1(b,alg.bda.empty(compose3(b,alg.uab.ab1(b,c,a))));
      lazy val c : C = compose2(c,alg.cda.empty(compose4(c,alg.uac.ab2(b,c,a))));
      lazy val a : A = alg.ada.bc(b,c,a);
      a
    }  
    def composebb(b1: => BIfc, b2: hasb) : BIfc = new BIfc { def rc = b1.rc; def b = b2.b }
    def composebrc(b1: => BIfc, b2: hasrC) : BIfc = new BIfc { def rc = b2.rc; def b = b1.b }
    def composecc(c1: => CIfc, c2: hasc) : CIfc = new CIfc { def rb = c1.rb; def c = c2.c }
    def composecrb(c1: => CIfc, c2: hasrB) : CIfc = new CIfc { def rb = c2.rb; def c = c1.c }

    def main(args: Array[String]) = {
      print(fold[hasb with hasc, BIfc, CIfc,hasb,hasc,hasrC,hasrB](CompleteAlg(abattr, battr, cattr, rCAttr, rBAttr),composebb _ , composecc _ ,composebrc _, composecrb _).b)
    }  
}    