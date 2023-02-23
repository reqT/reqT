/***     
**                  _______        
**                 |__   __|   reqT - a free requirements engineering tool  
**   _ __  ___   __ _ | |      (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is free open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
***************************************************************************/

package reqT 

import scala.language.{postfixOps, implicitConversions}

trait ImplicitContraints { // mixed into reqT package object
  implicit def modelToCP(m: Model): ConstraintProblem = ConstraintProblem(m.constraints ++ m.intAttrToConstraints)
  
  implicit def constrVectorToConst(cs: Vector[Constr]): Constraints = Constraints(cs)
  implicit def constraintsToVector(cs: Constraints): Vector[Constr] = cs.value
  //implicit def seqConstrToConstraints[T](cs: Seq[Constr]): Constraints = Constraints(cs.toVector)  ???

  implicit def constrToConstraints(c:Constr) = Constraints(c)
  
  implicit def constraintsToCP(cs: Constraints): ConstraintProblem = ConstraintProblem(cs)
  implicit def constrVectorToCP(cs: Vector[Constr]): ConstraintProblem = ConstraintProblem(Constraints(cs))
  
  implicit def attrRefToVar(ar: AttrRef[Int]): Var = Var(ar) 
  
  implicit def seqAttrRefToVar(rs: Seq[AttrRef[Int]]) = rs.map(Var(_))
  implicit def rangeToInterval(r: Range): Interval = Interval(r.min, r.max)

  implicit class RangeSeqOps(rs: Seq[Range]) { 
    //to enable > Var("x")::Seq(1 to 10, 12 to 15)
    def ::(v: Var): Bounds = Bounds(Seq(v), rs.map(rangeToInterval(_)))
    def ::(vs: Seq[Var]): Bounds = Bounds(vs, rs.map(rangeToInterval(_)))
  }
  implicit class RangeIntervalOps(ivls: Seq[Interval]) { 
    //to enable > Var("x")::Seq(Interval(1 to 10), Interval(12 to 15))
    def ::(v: Var): Bounds = Bounds(Seq(v), ivls)
    def ::(vs: Seq[Var]): Bounds = Bounds(vs, ivls)
  }  

}

trait ConstraintGenerators { // mixed into reqT package object
  def vars[T <: AnyRef](vs: T *): Seq[Var] = vs.map(Var(_)).toIndexedSeq
  def vars(n: Int, prefix: String): Vector[Var] = 
    (for (i <- 0 until n) yield Var(s"$prefix$i")).toVector
  def forAll[T](xs:Seq[T])(f: T => Constr) = Constraints(xs.map(f(_)).toVector)
  def forAll[T1, T2](x1s:Seq[T1], x2s: Seq[T2])(f: (T1, T2) => Constr) = Constraints(
    ( for (x1 <- x1s; x2 <- x2s) yield f(x1, x2) ) .toVector
  )
  def forAll[T1, T2, T3](x1s:Seq[T1], x2s: Seq[T2], x3s: Seq[T3])(f: (T1, T2, T3) => Constr) = Constraints(
    ( for (x1 <- x1s; x2 <- x2s; x3 <- x3s) yield f(x1, x2, x3) ) .toVector
  )
  def sumForAll[T](xs:Seq[T])(f: T => Var) = SumBuilder(xs.map(f(_)).toVector)
}

case class Interval(min: Int, max: Int) extends DSL {
  //if (min > max) jacop.Settings.warningPrinter("Negative interval min > max: " + this )
  def ::(v: Var): Bounds = Bounds(Seq(v), Seq(this))
  def ::(vs: Seq[Var]): Bounds = Bounds(vs, Seq(this))
  def ::(b: Bounds): Bounds = Bounds(b.seq1, b.domain ++ Seq(this))
  override def toScala =  s"$prefix($min, $max)" 
}  

case class Result(
  conclusion: Conclusion, 
  solutionCount: Int = 0, 
  lastSolution: Map[Var, Int] = Map[Var, Int](),
  interruptOption: Option[SearchInterrupt] = None,
  solutionsOption: Option[Solutions] = None
)  
  
sealed trait SearchInterrupt 
case object SearchTimeOut extends SearchInterrupt
case object SolutionLimitReached extends SearchInterrupt

sealed trait Conclusion
case object SolutionFound extends Conclusion
case object SolutionNotFound extends Conclusion
case object InconsistencyFound extends Conclusion
case class SearchFailed(msg: String) extends Conclusion  
 
sealed trait SearchType   
case object Satisfy extends SearchType
case object CountAll extends SearchType
case object FindAll extends SearchType 
sealed trait Optimize extends SearchType { def cost: Var }
case class Minimize(cost: Var) extends Optimize
case class Maximize(cost: Var) extends Optimize

case class Var(ref: AnyRef) extends DSL { 
  def ===(that: Var) = XeqY(this, that)
  def ===(const: Int) = XeqC(this, const)
  def ===(const: Boolean) = XeqBool(this, const)
  def ===(sumThat: SumBuilder) = SumEq(sumThat.vs, this) 
  def ===(mulThat: MulBuilder) = XmulYeqZ(mulThat.x, mulThat.y, this)
  def >(that: Var) = XgtY(this, that)  
  def >(const: Int) = XgtC(this, const)
  def >=(that: Var) = XgteqY(this, that)  
  def >=(const: Int) = XgteqC(this, const)
  def <(that: Var) = XltY(this, that)  
  def <(const: Int) = XltC(this, const)
  def <=(that: Var) = XlteqY(this, that)  
  def <=(const: Int) = XlteqC(this, const)
  def =/=(that: Var) = XneqY(this, that)
  def =/=(const: Int) = XneqC(this, const)
  def =/=(const: Boolean) = XeqBool(this, !const)
  
  def *(that: Var) = MulBuilder(this, that)  
  def +(that: Var) = PlusBuilder(this, that)  
  
  override def toScala: String = {
    def boxVar(str: String): String = s"Var($str)" 
    ref match {
      case ar@AttrRef(_, t) if t.isInt => ar.toScala  
      case t : DSL => boxVar(t.toScala) 
      case s : String => boxVar(s.toScala)
      case any => boxVar(any.toString)
    } 
  }
}

case class SumBuilder(vs: Vector[Var]) { 
  def ===(that: Var) = SumEq(vs, that) 
}

case class MulBuilder(x: Var, y: Var) {
  def ===(z: Var) = XmulYeqZ(x, y, z)
}

case class PlusBuilder(x: Var, y: Var) {
  def ===(z: Var) = XplusYeqZ(x, y, z)
  def <=(z: Var) = XplusYlteqZ(x, y, z)
}

object Sum {
  def apply(v: Var, vs: Var *) = SumBuilder(v +: vs.toVector)
  def apply(vs: Seq[Var]) = SumBuilder(vs.toVector)
}

trait BoundingConstr //marker trait to enable Bounds filter in jacop.scala

trait Variables extends DSL { 
  def variables: Seq[Var]  
  def varSeqToScala(vs: Seq[Var]) = vs.map(_.toScala).mkString("Seq(",",",")")
  override def toScala = prefix + variables.map(_.toScala).mkString("(",",",")")
} 

trait Constr extends Variables { override def toString = toScala }

trait PrimitiveConstr extends Constr {  //marker trait to prevent wrong usage of jacob primitive constr
  def <=>(that: Var) = Reified(this, that)
}
trait Constr1IntConst extends Constr { 
  val x: Var
  val c: Int
  val variables: Seq[Var] = Seq(x) 
  override def toScala = prefix + "(" + x.toScala + "," + c + ")"
}
trait Constr1BoolConst extends Constr { 
  val x: Var
  val c: Boolean
  val variables: Seq[Var] = Seq(x) 
  override def toScala = prefix + "(" + x.toScala + "," + c + ")"
}
trait Constr2 extends Constr { 
  val x: Var; val y: Var
  val variables: Seq[Var] = Seq(x, y) 
}
trait Constr3 extends Constr { 
  val x: Var; val y: Var; val z: Var
  val variables: Seq[Var] = Seq(x, y, z) 
}  
trait ConstrSeq1 extends Constr { 
  val seq1: Seq[Var]
  val variables: Seq[Var] = seq1
  override def toScala = prefix + "(" + varSeqToScala(seq1) + ")"
}
trait Constr1Seq1 extends Constr { 
  val x: Var
  val seq1: Seq[Var]
  val variables: Seq[Var] = Seq(x) ++ seq1 
  override def toScala = prefix + "(" + varSeqToScala(seq1) + "," + x.toScala + ")"
}
trait Constr1Seq1IntConst extends Constr { 
  val x: Var
  val seq1: Seq[Var]
  val c: Int
  val variables: Seq[Var] = Seq(x) ++ seq1 
  override def toScala = prefix + "(" + varSeqToScala(seq1) + "," + x.toScala + "," + c + ")"
}
trait Constr2Seq1 extends Constr { 
  val x: Var; val y: Var
  val seq1: Seq[Var]
  val variables: Seq[Var] = Seq(x, y) ++ seq1
  override def toScala = prefix + "(" + x.toScala + "," + varSeqToScala(seq1) + "," + y.toScala + ")"
}
trait ConstrSeq2ConstSeq1 extends Constr { 
  def seq1: Seq[Var]
  def seq2: Seq[Var]
  def constSeq1: Seq[Int]
  lazy val variables: Seq[Var] = seq1 ++ seq2
  override def toScala = prefix + "(" + varSeqToScala(seq1) + "," + varSeqToScala(seq2) + "," + constSeq1 + ")"
}
trait ConstrMatrix extends Constr { 
  def matrix: Vector[Vector[Var]]
  val variables: Seq[Var] = matrix.flatten
  override def toScala = prefix + "(" + matrix.map(varSeqToScala(_)).mkString("Seq(",",",")") + ")"
}  

trait CompoundConstr extends Constr {
  val constraints: Seq[Constr]
  lazy val variables: Seq[Var]  = constraints.flatMap(_.variables).distinct
  override def toScala = prefix + constraints.map(_.toScala).mkString("(",",",")")
}

trait CompoundConstr1 extends CompoundConstr {
  val c1: Constr
  val constraints = Seq(c1)
}

trait CompoundConstr2 extends CompoundConstr {
  val c1: Constr
  val c2: Constr
  val constraints = Seq(c1, c2)
}
trait CompoundConstr3 extends CompoundConstr {
  val c1: Constr
  val c2: Constr
  val c3: Constr
  val constraints = Seq(c1, c2, c3)
}

trait CompoundConstr1Var1 extends CompoundConstr1 {
  val x: Var
  override lazy val variables: Seq[Var]  = (constraints.flatMap(_.variables) :+ x).distinct
}

case class Bounds(seq1: Seq[Var], domain: Seq[Interval]) 
extends ConstrSeq1 with BoundingConstr {
  def addDomainOf(that: Bounds): Bounds = Bounds(seq1, domain ++ that.domain)
  def seqToScala[A <: DSL](xs: Seq[A]) = xs.size match {
      case 0 => "Seq()"
      case 1 => xs.head.toScala
      case _ => xs.map(_.toScala).mkString("Seq(",",",")")
  }
  override def toScala: String =  seqToScala(seq1) + " :: " + seqToScala(domain)
}
object Bounds {
  def apply(v: Var, ivls: Interval *) = new Bounds(Seq(v), ivls) 
  def apply(vs: Var *) = new Bounds(vs, Seq()) 
}

case class AbsXeqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr

case class AllDifferent(seq1: Seq[Var]) extends ConstrSeq1

case class And(constraints: Seq[Constr]) extends CompoundConstr
case object And { 
  def apply(c1: Constr, c2: Constr) = new And(Seq(c1, c2)) 
}

case class IndexValue(index: Var, varSeq: Seq[Var], valueAtIndex: Var) extends Constr2Seq1 {
  val x = index
  val y = valueAtIndex
  val seq1 = varSeq
}
case class SumEq(seq1: Seq[Var], x: Var) extends Constr1Seq1 
case class Count(seq1: Seq[Var], x: Var, c: Int) extends Constr1Seq1IntConst
case class XeqC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr {
  override def toScala = x.toScala + " === " + c
}
case class XeqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr {
  override def toScala = x.toScala + " === " + y.toScala
}

case class XdivYeqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr
case class XexpYeqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr 
case class XmulYeqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr 
case class XplusYeqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr 
case class XplusYlteqZ(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr
case class Distance(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr

case class XgtC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr {
  override def toScala = x.toScala + " > " + c
}
case class XgteqC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr {
  override def toScala = x.toScala + " >= " + c
}
case class XgteqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr {
  override def toScala = x.toScala + " >= " + y.toScala
}
case class XgtY(x: Var, y: Var) extends Constr2 with PrimitiveConstr {
  override def toScala = x.toScala + " > " + y.toScala
}
case class XltC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr {
  override def toScala = x.toScala + " < " + c
}
case class XlteqC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr {
  override def toScala = x.toScala + " <= " + c
}
case class XlteqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr {
  override def toScala = x.toScala + " <= " + y.toScala
}
case class XltY(x: Var, y: Var) extends Constr2 with PrimitiveConstr {
  override def toScala = x.toScala + " < " + y.toScala
}
case class XneqC(x: Var, c: Int) extends Constr1IntConst with PrimitiveConstr {
  override def toScala = x.toScala + " =/= " + c
}
case class XneqY(x: Var, y: Var) extends Constr2 with PrimitiveConstr {
  override def toScala = x.toScala + " =/= " + y.toScala
}
case class XeqBool(x: Var, c: Boolean) extends Constr1BoolConst with PrimitiveConstr 

case class IfThen(c1: PrimitiveConstr, c2: PrimitiveConstr) extends CompoundConstr2 with PrimitiveConstr

case class IfThenElse(c1: PrimitiveConstr, c2: PrimitiveConstr, c3: PrimitiveConstr) extends CompoundConstr3 with PrimitiveConstr

case class IfThenBool(x: Var, y: Var, z: Var) extends Constr3 with PrimitiveConstr


case class Reified(c1: PrimitiveConstr, x: Var) extends  CompoundConstr1Var1 {
  override def toScala = "(" + c1.toScala + ") <=> " + x.toScala
}

case class Rectangle(x: Var, y: Var, dx: Var, dy: Var) extends Variables {
  lazy val toVector: Vector[Var] = Vector(x, y, dx, dy)
  lazy val variables: Seq[Var] = toVector
}

case class Diff2(rectangles: Vector[Vector[Var]]) extends ConstrMatrix {
  lazy val matrix = rectangles     
  assert(rectangles.map(_.size).distinct == Vector(4), "size of all rectangle vectors must be 4")
  override def toScala = "Diff2" + rectangles.map(_.mkString("Rectangle(",",",")")).mkString("(",",",")")
}
object Diff2 {
  def apply(rectangles: Rectangle *) = new Diff2(rectangles.toVector.map(_.toVector))
}

case class Binpacking(item: Vector[Var], load: Vector[Var], size: Vector[Int]) 
    extends ConstrSeq2ConstSeq1 {
  lazy val seq1 = item
  lazy val seq2 = load
  lazy val constSeq1 = size    
}
