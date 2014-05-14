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

package reqT
import language.implicitConversions
import JaCoP. { search => jsearch, core => jcore, constraints => jcon }  

/**  http://www.jacop.eu/  */

trait Solutions[T] {
  def nVariables: Int
  def variables: Array[Var[T]]
  def varMap: Map[String, Var[T]]
  def indexOf: Map[Var[T], Int]
  def nSolutions: Int
  def lastSolution: Map[Var[T], Int]
  def solution(solutionIndex: Int): Array[Int]
  def solutionMap(solutionIndex: Int): Map[Var[T], Int]
  def value(solutionIndex: Int, variableIndex: Int): Int
  def valueVector(v: Var[T]): Vector[Int]
  def coreDomains: Array[Array[JaCoP.core.Domain]] 
  def coreVariables: Array[_ <: JaCoP.core.Var]
  def domain(solutionIndex: Int, variableIndex: Int): JaCoP.core.Domain
  def printSolutions: Unit
}

case class CSP(m: Model, cs: Constraints) {
  //Any propagates because of Map invariance in first Type argument:
  def updateModel(vmap: Map[Var[Any], Int]): Model = { 
    var newModel = m
    val modelVariables = vmap collect { 
      case (Var(ar @ AttrRef(_,t)), i) if t.isInt => ar(i)  
    }
    
    modelVariables foreach { attrVal => newModel += attrVal  } 
    newModel
  }

  def solve(
      searchType: SearchType = jacop.Settings.defaultSearchType,
      timeOutOption: Option[Long] = None,
      solutionLimitOption: Option[Int] = None,
      valueSelection: jacop.ValueSelection = jacop.Settings.defaultValueSelection,
      variableSelection: jacop.VariableSelection = jacop.Settings.defaultVariableSelection,
      assignOption: Option[Seq[Var[Any]]] = None
   ): (Model, Result[Any]) =  {
    val r = jacop.Solver(cs, searchType, timeOutOption, solutionLimitOption, valueSelection, variableSelection, assignOption).solve
    if (r.conclusion == SolutionFound) (updateModel(r.lastSolution), r) 
    else { 
      jacop.Settings.warningPrinter(r.conclusion.toString)
      (Model(), r) 
    } 
  }
  
  def satisfy: Model = solve(Satisfy)._1
  private def optimum(mr: (Model, Result[Any]), v: Var[Any]): (Model, Option[Int]) = 
    (mr._1, mr._2.lastSolution.get(v))
  def maximize(v: Var[Any]): (Model, Option[Int]) = optimum(solve(Maximize(v)), v)
  def minimize(v: Var[Any]): (Model, Option[Int]) = optimum(solve(Minimize(v)), v) 
}
  
class JacopSolutions[T]( 
        val coreDomains: Array[Array[jcore.Domain]], 
        val coreVariables: Array[_ <: jcore.Var],
        val nSolutions: Int,
        val lastSolution: Map[Var[T], Int]) extends Solutions[T] {
  lazy val nVariables = coreVariables.length
  lazy val varMap: Map[String, Var[T]] = lastSolution.keys.toSeq.map(v => (v.ref.toString, v)).toMap
  lazy val variables: Array[Var[T]] = coreVariables.map(v => varMap(v.id))
  lazy val indexOf: Map[Var[T], Int] = variables.zipWithIndex.toMap
  private def toInt(d: jcore.Domain): Int = d.asInstanceOf[jcore.IntDomain].value
  def domain(solutionIndex: Int, variableIndex: Int): jcore.Domain = coreDomains(solutionIndex)(variableIndex)
  def value(s: Int, v: Int): Int = toInt(domain(s,v))
  def solution(s: Int): Array[Int] = coreDomains(s).map(toInt)
  def solutionMap(s: Int): Map[Var[T], Int] = 
    ( for (i <- 0 until nVariables) yield (variables(i), value(s, i)) ) .toMap
  def valueVector(v: Var[T]): Vector[Int] = 
    ( for (s <- 0 until nSolutions) yield value(s, indexOf(v)) ) .toVector   
  def printSolutions: Unit = for (i <- 0 until nSolutions) println(s"*** Solution $i:\n" + solutionMap(i))
  override def toString = s"Solutions([nSolutions=$nSolutions][nVariables=$nVariables])" 
}

 
object jacop {  

  object Settings {
    var defaultInterval: Interval = Interval(-1000, 1000)
    var defaultSearchType: SearchType = Satisfy
    var defaultValueSelection: ValueSelection = IndomainRandom
    var defaultVariableSelection: VariableSelection = InputOrder
    var verbose: Boolean = true
    var debug: Boolean = false
    var warningPrinter: String => Unit = (s: String) => println("WARNING: " + s)
  }
 
  type JIntVar = jcore.IntVar
  type JVar = jcore.Var
  type JBooleanVar = jcore.BooleanVar
  
  sealed trait ValueSelection { def toJacop: jsearch.Indomain[JIntVar] }
  case object IndomainMax  extends ValueSelection { def toJacop = new jsearch.IndomainMax[JIntVar] }
  case object IndomainMedian  extends ValueSelection { def toJacop = new jsearch.IndomainMedian[JIntVar] }
  case object IndomainMiddle  extends ValueSelection { def toJacop = new jsearch.IndomainMiddle[JIntVar] }
  case object IndomainMin  extends ValueSelection { def toJacop = new jsearch.IndomainMin[JIntVar] }
  case object IndomainRandom  extends ValueSelection { def toJacop = new jsearch.IndomainRandom[JIntVar] }
  case object IndomainSimpleRandom  extends ValueSelection { def toJacop = new jsearch.IndomainSimpleRandom[JIntVar] }

/* 
value selection methods not yet implemented    
 case object IndomainSetMax  extends ValueSelection { def toJacop = new jsearch.IndomainMax[JSetVar] }
 case object IndomainSetMin  extends ValueSelection { def toJacop = new jsearch.IndomainMax[JSetVar] }
 case object IndomainSetRandom  extends ValueSelection { def toJacop = new jsearch.IndomainMax[JSetVar] }
  case object IndomainHierarchical  extends ValueSelection 
  case object IndomainList  extends ValueSelection  
*/
  sealed trait VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection): 
      jsearch.SelectChoicePoint[JIntVar] 
  }
  case object InputOrder extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.InputOrderSelect[JIntVar](s, jvs, valSelect.toJacop) 
  }
  case object LargestDomain extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.LargestDomain(), valSelect.toJacop) 
  }
  case object SmallestDomain extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.SmallestDomain(), valSelect.toJacop) 
  }
  case object LargestMax extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.LargestMax(), valSelect.toJacop) 
  }
  case object LargestMin extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.LargestMin(), valSelect.toJacop) 
  }
  case object SmallestMax extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.SmallestMax(), valSelect.toJacop) 
  }
  case object SmallestMin extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.SmallestMin(), valSelect.toJacop) 
  }
  case object MaxRegret extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.MaxRegret(), valSelect.toJacop) 
  }
  case object MostConstrainedDynamic extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.MostConstrainedDynamic(), valSelect.toJacop) 
  }
  case object MostConstrainedStatic extends VariableSelection { 
    def toJacop(s: jcore.Store, jvs: Array[JIntVar], valSelect: ValueSelection) =
        new jsearch.SimpleSelect[JIntVar](jvs, new jsearch.MostConstrainedStatic(), valSelect.toJacop) 
  }
  
/* variable selection methods not yet implemented:
   MaxCardDiff    
   MaxGlbCard    
   MaxLubCard    
   MinCardDiff    
   MinDomainOverDegree    
   MinGlbCard    
   MinLubCard    
   WeightedDegree    
   */

  trait SolverUtils {
    type Ivls[T] = Map[Var[T], Seq[Interval]]
    def distinctVars[T](cs: Seq[Constr[T]]): Seq[Var[T]] = cs.map { _.variables } .flatten.distinct
    def collectBounds[T](cs: Seq[Constr[T]]): Seq[Bounds[T]] = cs collect { case b: Bounds[T] => b }
    def collectConstr[T](cs: Seq[Constr[T]]): Seq[Constr[T]] = cs filter { case b: BoundingConstr => false ; case _ => true }
    def intervals[T](b: Bounds[T]): Map[Var[T], Seq[Interval]] = b.variables.map(v => (v, b.domain)).toMap
    def mergeIntervals[T](ivls1: Ivls[T], ivls2: Ivls[T]): Ivls[T] = {
      var result = ivls1
      for ((v, ivls) <- ivls2) { 
        if (result.isDefinedAt(v)) result += v -> (result(v) ++ ivls) 
        else result += v -> ivls 
      }
      result
    }
    def buildDomainMap[T](cs: Seq[Constr[T]]): Ivls[T] = {
      var result = collectBounds(cs).map(intervals(_)).foldLeft(Map(): Ivls[T])(mergeIntervals(_,_))
      for (v <- distinctVars(cs)) {
        if (!result.isDefinedAt(v)) result += v -> Seq(jacop. Settings.defaultInterval)
      }
      result
    }
    def nameToVarMap[T](vs: Seq[Var[T]]): Map[String, Var[T]] = vs.map(v => (v.ref.toString, v)).toMap
    def checkUniqueToString[T](vs: Seq[Var[T]]): Set[String] = {
      val strings = vs.map(_.ref.toString)
      strings.diff(strings.distinct).toSet
    }
    def checkIfNameExists[T](name: String, vs: Seq[Var[T]]): Boolean = 
      vs.exists { case Var(ref) => ref.toString == name }
    
  def flattenAllConstraints(cs: Seq[Constr[Any]]): Seq[Constr[Any]] = {
      def flatten(xs: Seq[Constr[Any]]): Seq[Constr[Any]] = 
        if (xs.isEmpty) xs 
        else (xs.head match {
          case cs: Constraints => flatten(cs.value)
          case c => Seq(c)
        } ) ++ flatten(xs.tail)
      flatten(cs)
    }
 }         
   
  case class Solver[T](
      constraints: Seq[Constr[T]] , 
      searchType: SearchType = jacop.Settings.defaultSearchType,
      timeOutOption: Option[Long] = None,
      solutionLimitOption: Option[Int] = None,
      valueSelection: ValueSelection = jacop.Settings.defaultValueSelection,
      variableSelection: VariableSelection = jacop.Settings.defaultVariableSelection,
      assignOption: Option[Seq[Var[T]]] = None
    ) extends SolverUtils {

    def toJCon[T](constr: Constr[T], store: jcore.Store, jIntVar: Map[Var[T], JIntVar]): jcon.Constraint = {
      def jVarArray(vs: Seq[Var[T]]) = vs.map(v => jIntVar(v)).toArray
      constr match {
        case AbsXeqY(x, y) => new jcon.AbsXeqY(jIntVar(x), jIntVar(y))
        case AllDifferent(vs) => new jcon.Alldiff(jVarArray(vs))
        case And(cs) => 
          new jcon.And(cs.map(c => 
              toJCon(c, store, jIntVar).asInstanceOf[jcon.PrimitiveConstraint]
            ).toArray
          )
        case IndexValue(ix, vs, v) => new jcon.Element(jIntVar(ix), jVarArray(vs), jIntVar(v))
        case SumEq(vs, x) => new jcon.Sum(vs.map(v => jIntVar(v)).toArray, jIntVar(x))
        case Count(vs, x, c) => new jcon.Count(vs.map(v => jIntVar(v)).toArray, jIntVar(x),  c)
        case XeqC(x, c) => new jcon.XeqC(jIntVar(x), c)
        case XeqY(x, y) => new jcon.XeqY(jIntVar(x), jIntVar(y))
        case XdivYeqZ(x, y, z) => new jcon.XdivYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case XexpYeqZ(x, y, z) => new jcon.XexpYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case XmulYeqZ(x, y, z) => new jcon.XmulYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case XplusYeqZ(x, y, z) => new jcon.XplusYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case XplusYlteqZ(x, y, z) => new jcon.XplusYlteqZ(jIntVar(x), jIntVar(y), jIntVar(z))
        case Distance(x, y, z) => new jcon.Distance(jIntVar(x), jIntVar(y), jIntVar(z))
        case XgtC(x, c) => new jcon.XgtC(jIntVar(x), c)
        case XgteqC(x, c) => new jcon.XgteqC(jIntVar(x), c)
        case XgteqY(x, y) => new jcon.XgteqY(jIntVar(x), jIntVar(y))
        case XgtY(x, y) => new jcon.XgtY(jIntVar(x), jIntVar(y))
        case XltC(x, c) => new jcon.XltC(jIntVar(x), c)
        case XlteqC(x, c) => new jcon.XlteqC(jIntVar(x), c)
        case XlteqY(x, y) => new jcon.XlteqY(jIntVar(x), jIntVar(y))
        case XltY(x, y) => new jcon.XltY(jIntVar(x), jIntVar(y))
        case XneqC(x, c) => new jcon.XneqC(jIntVar(x), c)
        case XneqY(x, y) => new jcon.XneqY(jIntVar(x), jIntVar(y))
        case XeqBool(x, b) => new jcon.XeqC(jIntVar(x), if (b) 1 else 0)
        case IfThen(c1, c2) =>
          val jc = (toJCon(c1, store, jIntVar), toJCon(c2, store, jIntVar)) 
          new jcon.IfThen(jc._1.asInstanceOf[jcon.PrimitiveConstraint],   jc._2.asInstanceOf[jcon.PrimitiveConstraint])
        case IfThenElse(c1, c2, c3) =>
          val vs = Vector(c1, c2, c3)
          val jc = vs.map(toJCon(_, store, jIntVar).asInstanceOf[jcon.PrimitiveConstraint]) 
          new jcon.IfThenElse(jc(0), jc(1), jc(2))
        case IfThenBool(x,y,z) => new jcon.IfThenBool(jIntVar(x), jIntVar(y), jIntVar(z))
        case Reified(c1, x) =>
          val jc = toJCon(c1, store, jIntVar).asInstanceOf[jcon.PrimitiveConstraint]
          new jcon.Reified(jc, jIntVar(x))
        case Diff2(rectangles) => 
          def matrix: Array[Array[JIntVar]] = rectangles.map(jVarArray(_)).toArray
          new jcon.Diff2(matrix)
        case Binpacking(item, load, size) =>
          new jcon.binpacking.Binpacking(jVarArray(item), jVarArray(load), size.toArray)
        case c => println("Constr to JaCoP match error: " + c); ???
      }
    }
    
    lazy val flatConstr = flattenAllConstraints(constraints)
    lazy val domainOf: Map[Var[Any], Seq[Interval]] = buildDomainMap(flatConstr)
 
    def varToJIntVar[T](v: Var[T], s: jcore.Store): JIntVar = {
      val intDom = new jcore.IntervalDomain()
      domainOf(v).foreach(ivl => intDom.addDom( new jcore.IntervalDomain(ivl.min, ivl.max)))
      new JIntVar(s, v.ref.toString, intDom) 
    }

    val minimizeHelpVarName = "_$minimize0"

    def collectIntVars(s: jcore.Store): Array[JIntVar] = 
      s.vars.collect { case x: JIntVar if (x.id != minimizeHelpVarName) => x } .toArray
            
    def solutionMap[T](s: jcore.Store, nameToVar: Map[String, Var[T]] ): Map[Var[T], Int] = 
          collectIntVars(s).filter(_.singleton).map(iv => (nameToVar(iv.id), iv.value) ).toMap

    def solve: Result[Any] = {
      val store = new jcore.Store
      val vs = distinctVars(flatConstr)
      val cs = collectConstr(flatConstr)
      if (Settings.verbose) println("*** VARIABLES:   " + vs.mkString(","))
      if (Settings.verbose) println("*** CONSTRAINTS: " + cs.mkString(","))
      val duplicates = checkUniqueToString(vs)
      if (!duplicates.isEmpty) 
        return Result(SearchFailed("Duplicate toString values of variables:" + 
          duplicates.mkString(", "))        )
      if (checkIfNameExists(minimizeHelpVarName, vs)) 
        return Result(SearchFailed("Reserved varable name not allowed:" + minimizeHelpVarName))
      val intVarMap: Map[Var[Any], JIntVar] = vs.map { v => (v, varToJIntVar(v, store)) } .toMap
      Some(searchType).collect { //abort if cost variable is not defined
        case opt: Optimize[Any] if (!intVarMap.isDefinedAt(opt.cost)) => 
          return Result(SearchFailed("Cost variable not defined:" + opt.cost)) 
      }
      cs foreach { c => store.impose(toJCon(c, store, intVarMap)) }
      if (Settings.debug) println(store)
      if (!store.consistency) return Result(InconsistencyFound)
      val label = new jsearch.DepthFirstSearch[JIntVar]
      def listener = label.getSolutionListener().asInstanceOf[jsearch.SimpleSolutionListener[JIntVar]]
      def setup(searchAll: Boolean, recordSolutions: Boolean): Unit =  {
        //this must be done in the right order as JaCoP may overwrite solutionLimit if
        // searchAll is set after setting solutionLimit as explained by Kris
        listener.searchAll(searchAll)
        listener.recordSolutions(recordSolutions)
        timeOutOption.map { timeOut => label.setTimeOut(timeOut) } 
        solutionLimitOption.map { limit =>  listener.setSolutionLimit(limit) }
      }
      val variablesToAssign: Array[JIntVar] = 
        if (!assignOption.isDefined) collectIntVars(store) //assign all in store
        else assignOption.get.map(intVarMap(_)).toArray
      val selectChoicePoint = variableSelection.toJacop(store, variablesToAssign, valueSelection)
      def solutionNotFound = Result[Any](SolutionNotFound)
      def solutionInStore = solutionMap(store, nameToVarMap(vs))
      def interruptOpt: Option[SearchInterrupt] = 
        if (label.timeOutOccured) Some(SearchTimeOut) 
        else if (listener.solutionLimitReached && solutionLimitOption.isDefined) 
          Some(SolutionLimitReached)
        else None
      def oneResult(ok: Boolean) = 
        if (ok) Result[Any](SolutionFound, 1, solutionInStore, interruptOpt)  
        else solutionNotFound
      def countResult(ok: Boolean, i: Int) = 
        if (ok) Result[Any](SolutionFound, i, solutionInStore, interruptOpt) 
        else solutionNotFound
      val conclusion = searchType match {
        case Satisfy => 
          setup(searchAll = false , recordSolutions = true )
          oneResult(label.labeling(store, selectChoicePoint)) 
        case CountAll => //count solutions but don't record any solution to save memory
          setup(searchAll = true , recordSolutions = false )
          countResult(label.labeling(store, selectChoicePoint), listener.solutionsNo) 
        case FindAll => 
          setup(searchAll = true , recordSolutions = true )
          val found = label.labeling(store, selectChoicePoint)
          if (!found) solutionNotFound else {
            val solutions: Solutions[Any] = new JacopSolutions(
                  listener.getSolutions(), 
                  listener.getVariables(), 
                  listener.solutionsNo(), 
                  solutionInStore)
            Result[Any](SolutionFound, solutions.nSolutions, solutionInStore, interruptOpt, Some(solutions))
          }
        case minimize: Minimize[Any] => 
          listener.searchAll(false)
          listener.recordSolutions(true)
          oneResult(label.labeling(store, selectChoicePoint, intVarMap(minimize.cost)))
        case m: Maximize[Any] =>  
          listener.searchAll(false)
          listener.recordSolutions(true)
          val intDom = new jcore.IntervalDomain()
          domainOf(m.cost) foreach (ivl => intDom.addDom( new jcore.IntervalDomain(-ivl.max, -ivl.min)))
          val negCost = new JIntVar(store, minimizeHelpVarName, intDom)
          store.impose( new jcon.XmulCeqZ(intVarMap(m.cost), -1, negCost) )
          oneResult(label.labeling(store, selectChoicePoint, negCost))
        case other => 
          println("This search type is not yet implemented: " + other)
          ???
          solutionNotFound
      }
      if (Settings.verbose) println(store)
      conclusion
    } //end def solve
    
  } //end class Solver
} // end object jacop

