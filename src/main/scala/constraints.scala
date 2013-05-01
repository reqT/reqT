/****************************************************************     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2013, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/

package reqt {

  /** A module for constraint satisfaction programming over reqT models
  *   Depends on: reqt._, reqt.jacop._
  */
  import language.postfixOps
  
  //---- integration with class Model: (for implicits conversions see package.scala)
  
  case class CSP[T](m: Model, cs: Seq[Constr[T]]) {
    lazy val intValues: List[(Entity, Attribute[Int])] = (m collect { 
      case (Key(e,has), NodeSet(ns)) => ns.toList collect { 
        case a: IntValue => (e,a: Attribute[Int]) 
      } } ).flatten.toList
    lazy val intModelConstr: List[Constr[Any]] = intValues.collect { 
      case (e, a) =>  Var(Ref[Int](Vector(e),a.kind)) #== a.value } .toList
    lazy val allConstr: Seq[Constr[Any]] = cs ++ intModelConstr
    //Any propagates because of Map invariance in first Type arg
    def updateModel(vmap: Map[Var[Any], Int]): Model = { 
      var newModel = m
      val modelVariables = vmap collect { case (Var(r @ Ref(_,_)), i) => (r, i)  } //check r.attrKind is IntValue??
      modelVariables foreach { case (r, i) => newModel = newModel.updated(r, i)  } 
      newModel
    }
    def solve(objective: Objective): (Vector[Model], Vector[Map[Var[Any], Int]]) = {
      val Result(conclusion, i, solutions) = allConstr.solve(objective)
      if (conclusion == SolutionFound) (solutions.map(updateModel), solutions) 
      else { warn(conclusion.toString); (Vector(), Vector()) } 
    }
    def satisfy: Model = solve(Satisfy)._1.headOption.getOrElse(Model())
    def optimum(ms: (Vector[Model], Seq[Map[Var[Any], Int]]), v: Var[Any]): (Model, Option[Int]) = 
      (ms._1.headOption.getOrElse(Model()), ms._2.headOption.map(m => m.get(v)).flatten)
    def maximize(v: Var[Any]): (Model, Option[Int]) = optimum(solve(Maximize(v)), v)
    def minimize(v: Var[Any]): (Model, Option[Int]) = optimum(solve(Minimize(v)), v)
  }
  
  //---- main classes for constraints API

  case class Interval(min: Int, max: Int) extends CanGenerateScala with Prefixed {
    if (min > max) jacop.Settings.warningPrinter("Negative interval min > max: " + this )
    def ::[T](v: Var[T]): Bounds[T] = Bounds(Seq(v), Seq(this))
    def ::[T](vs: Seq[Var[T]]): Bounds[T] = Bounds(vs, Seq(this))
    def ::[T](b: Bounds[T]): Bounds[T] = Bounds(b.seq1, b.domain ++ Seq(this))
    override def toScala =  s"$prefix($min, $max)" 
  }  
  
  case class Result[T](conclusion: Conclusion, solutionCount: Int, solutions: Vector[Map[Var[T], Int]])  
 
  sealed trait Conclusion
  case object SolutionFound extends Conclusion
  case object SolutionNotFound extends Conclusion
  case object InconsistencyFound extends Conclusion
  case class SearchFailed(msg: String) extends Conclusion  
   
  sealed trait Objective   
  case object Satisfy extends Objective
  case object Count extends Objective
  case object FindAll extends Objective //TODO check jacop example gates.java to see how listener is built
  sealed trait Optimize[+T] extends Objective { def cost: Var[T] }
  case class Minimize[+T](cost: Var[T]) extends Optimize[T]
  case class Maximize[+T](cost: Var[T]) extends Optimize[T]

  case class Var[+T](ref: T) extends CanGenerateScala with Prefixed { 
    def #==[B >:T](that: Var[B]) = XeqY(this, that)
    def #==[B >:T](const: Int) = XeqC(this, const)
    def #==[B >:T](const: Boolean) = XeqBool(this, const)
    def #>[B >:T](that: Var[B]) = XgtY(this, that)  
    def #>[B >:T](const: Int) = XgtC(this, const)
    def #>=[B >:T](that: Var[B]) = XgteqY(this, that)  
    def #>=[B >:T](const: Int) = XgteqC(this, const)
    def #<[B >:T](that: Var[B]) = XltY(this, that)  
    def #<[B >:T](const: Int) = XltC(this, const)
    def #<=[B >:T](that: Var[B]) = XlteqY(this, that)  
    def #<=[B >:T](const: Int) = XlteqC(this, const)
    def #!=[B >:T](that: Var[B]) = XneqY(this, that)
    def #!=[B >:T](const: Int) = XneqC(this, const)
    def #!=[B >:T](const: Boolean) = XeqBool(this, !const)
    
    override def toScala: String = {
      def boxVar(str: String): String = s"Var($str)" 
      ref match {
        case iv: ImplicitVar => iv.toScala
        case t : CanGenerateScala => boxVar(t.toScala) 
        case s : String => boxVar(s.toScala)
        case any => boxVar(any.toString)
      } 
    }
  }
  // object Var { 
    // private var nextAnonymousVariableNumber = 0
    // private def nextVarNum() = { nextAnonymousVariableNumber += 1; nextAnonymousVariableNumber }
    // def anonVarName() = "v"+nextVarNum
    // def apply() = new Var(anonVarName()) 
  // }
  

  
  trait BoundingConstr //marker trait to enable Bounds filter in jacop.scala
  
  trait Variables[+T] extends Prefixed with CanGenerateScala { 
    def variables: Seq[Var[T]]  
    def varSeqToScala[B >: T](vs: Seq[Var[B]]) = vs.map(_.toScala).mkString("Seq(",",",")")
    override def toScala = prefix + variables.map(_.toScala).mkString("(",",",")")
  } 
  
  trait Constr[+T] extends Variables[T]
  
  trait PrimitiveConstr //TODO marker trait to prevent wrong usage of jacob primitive constr 
  
  trait Constr1IntConst[+T] extends Constr[T] { 
    val x: Var[T]
    val c: Int
    val variables: Seq[Var[T]] = Seq(x) 
    override def toScala = prefix + "(" + x.toScala + "," + c + ")"
  }
  trait Constr1BoolConst[+T] extends Constr[T] { 
    val x: Var[T]
    val c: Boolean
    val variables: Seq[Var[T]] = Seq(x) 
    override def toScala = prefix + "(" + x.toScala + "," + c + ")"
  }
  trait Constr2[+T] extends Constr[T] { 
    val x: Var[T]; val y: Var[T]
    val variables: Seq[Var[T]] = Seq(x, y) 
  }
  trait Constr3[+T] extends Constr[T] { 
    val x: Var[T]; val y: Var[T]; val z: Var[T]
    val variables: Seq[Var[T]] = Seq(x, y, z) 
  }  
  trait ConstrSeq1[+T] extends Constr[T] { 
    val seq1: Seq[Var[T]]
    val variables: Seq[Var[T]] = seq1
    override def toScala = prefix + "(" + varSeqToScala(seq1) + ")"
  }
  trait Constr1Seq1[+T] extends Constr[T] { 
    val x: Var[T]
    val seq1: Seq[Var[T]]
    val variables: Seq[Var[T]] = Seq(x) ++ seq1 
    override def toScala = prefix + "(" + varSeqToScala(seq1) + "," + x.toScala + ")"
  }
  trait Constr2Seq1[+T] extends Constr[T] { 
    val x: Var[T]; val y: Var[T]
    val seq1: Seq[Var[T]]
    val variables: Seq[Var[T]] = Seq(x, y) ++ seq1
    override def toScala = prefix + "(" + x.toScala + "," + varSeqToScala(seq1) + "," + y.toScala + ")"
  }
  trait ConstrMatrix[+T] extends Constr[T] { 
    def matrix: Vector[Vector[Var[T]]]
    val variables: Seq[Var[T]] = matrix.flatten
    override def toScala = prefix + "(" + matrix.map(varSeqToScala(_)).mkString("Seq(",",",")") + ")"
  }  
  
  trait CompoundConstr[+T] extends Constr[T] {
    val constraints: Seq[Constr[T]]
    lazy val variables: Seq[Var[T]]  = constraints.flatMap(_.variables).distinct
    override def toScala = prefix + constraints.map(_.toScala).mkString("(",",",")")
  }
  
  trait CompoundConstr1[+T] extends CompoundConstr[T] {
    val c1: Constr[T]
    val constraints = Seq(c1)
  }
  
  trait CompoundConstr2[+T] extends CompoundConstr[T] {
    val c1: Constr[T]
    val c2: Constr[T]
    val constraints = Seq(c1, c2)
  }
   
  case class Bounds[+T](seq1: Seq[Var[T]], domain: Seq[Interval]) 
  extends ConstrSeq1[T] with BoundingConstr {
    def addDomainOf[B >:T](that: Bounds[B]): Bounds[B] = Bounds[B](seq1, domain ++ that.domain)
    def seqToScala[A <: CanGenerateScala](xs: Seq[A]) = xs.size match {
        case 0 => "Seq()"
        case 1 => xs.head.toScala
        case _ => xs.map(_.toScala).mkString("Seq(",",",")")
    }
    override def toScala: String =  seqToScala(seq1) + " :: " + seqToScala(domain)
  }
  object Bounds {
    def apply[T](v: Var[T], ivls: Interval *) = new Bounds(Seq(v), ivls) 
    def apply[T](vs: Var[T] *) = new Bounds(vs, Seq()) 
  }
  
  case class AbsXeqY[+T](x: Var[T], y: Var[T]) extends Constr2[T]
  
  case class AllDifferent[+T](seq1: Seq[Var[T]]) extends ConstrSeq1[T]
  
  case class IndexOfEquals[T](index: Var[T], varSeq: Seq[Var[T]], value: Var[T]) extends Constr2Seq1[T] {
    val x = index
    val y = value
    val seq1 = varSeq
  }
  case class Sum[+T](seq1: Seq[Var[T]], x: Var[T]) extends Constr1Seq1[T] 
  case class XeqC[+T](x: Var[T], c: Int) extends Constr1IntConst[T] {
    override def toScala = x.toScala + " #== " + c
  }
  case class XeqY[+T](x: Var[T], y: Var[T]) extends Constr2[T] {
    override def toScala = x.toScala + " #== " + y.toScala
  }
  
  case class XdivYeqZ[+T](x: Var[T], y: Var[T], z: Var[T]) extends Constr3[T]
  case class XexpYeqZ[+T](x: Var[T], y: Var[T], z: Var[T]) extends Constr3[T] 
  case class XmulYeqZ[+T](x: Var[T], y: Var[T], z: Var[T]) extends Constr3[T] 
  case class XplusYeqZ[+T](x: Var[T], y: Var[T], z: Var[T]) extends Constr3[T] 
  case class XplusYlteqZ[+T](x: Var[T], y: Var[T], z: Var[T]) extends Constr3[T] 
  case class Distance[+T](x: Var[T], y: Var[T], z: Var[T]) extends Constr3[T] 
  
  case class XgtC[+T](x: Var[T], c: Int) extends Constr1IntConst[T] {
    override def toScala = x.toScala + " #> " + c
  }
  case class XgteqC[+T](x: Var[T], c: Int) extends Constr1IntConst[T] {
    override def toScala = x.toScala + " #>= " + c
  }
  case class XgteqY[+T](x: Var[T], y: Var[T]) extends Constr2[T] {
    override def toScala = x.toScala + " #>= " + y.toScala
  }
  case class XgtY[+T](x: Var[T], y: Var[T]) extends Constr2[T] {
    override def toScala = x.toScala + " #> " + y.toScala
  }
  case class XltC[+T](x: Var[T], c: Int) extends Constr1IntConst[T] {
    override def toScala = x.toScala + " #< " + c
  }
  case class XlteqC[+T](x: Var[T], c: Int) extends Constr1IntConst[T] {
    override def toScala = x.toScala + " #<= " + c
  }
  case class XlteqY[+T](x: Var[T], y: Var[T]) extends Constr2[T] {
    override def toScala = x.toScala + " #<= " + y.toScala
  }
  case class XltY[+T](x: Var[T], y: Var[T]) extends Constr2[T] {
    override def toScala = x.toScala + " #< " + y.toScala
  }
  case class XneqC[+T](x: Var[T], c: Int) extends Constr1IntConst[T] {
    override def toScala = x.toScala + " #!= " + c
  }
  case class XneqY[+T](x: Var[T], y: Var[T]) extends Constr2[T] {
    override def toScala = x.toScala + " #!= " + y.toScala
  }
  case class XeqBool[+T](x: Var[T], c: Boolean) extends Constr1BoolConst[T]
  
  case class IfThen[+T](c1: Constr[T], c2: Constr[T]) extends CompoundConstr2[T]
  
  case class Rectangle[+T](x: Var[T], y: Var[T], dx: Var[T], dy: Var[T]) extends Variables[T] {
    lazy val toVector: Vector[Var[T]] = Vector(x, y, dx, dy)
    lazy val variables: Seq[Var[T]] = toVector
  }
  
  case class Diff2[+T](rectangles: Vector[Vector[Var[T]]]) extends ConstrMatrix[T] {
    lazy val matrix = rectangles     
    assert(rectangles.map(_.size).distinct == Vector(4), "size of all rectangle vectors must be 4")
    override def toString = "Diff2" + rectangles.map(_.mkString("Rectangle(",",",")")).mkString("(",",",")")
  }
  object Diff2 {
    def apply[T](rectangles: Rectangle[T] *) = new Diff2[T](rectangles.toVector.map(_.toVector))
  }
  
} //end package reqt
