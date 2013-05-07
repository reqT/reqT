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
  import language.implicitConversions
  
  /**  An interface module that wraps http://www.jacop.eu/
  *    Dependends on JaCoP-3.2.jar: JaCoP.core._,JaCoP.search._,JaCoP.constraints._
  */
  
  class Solutions[T]( 
          val jacopDomains: Array[Array[JaCoP.core.Domain]], 
          val jacopVariables: Array[_ <: JaCoP.core.Var],
          val nSolutions: Int,
          val lastSolution: Map[Var[T], Int]) {
    lazy val nVariables = jacopVariables.length
    lazy val varMap: Map[String, Var[T]] = lastSolution.keys.toSeq.map(v => (v.ref.toString, v)).toMap
    lazy val variables: Array[Var[T]] = jacopVariables.map(v => varMap(v.id))
    lazy val indexOf: Map[Var[T], Int] = variables.zipWithIndex.toMap
    private def toInt(d: JaCoP.core.Domain): Int = d.asInstanceOf[JaCoP.core.IntDomain].value
    def domain(solutionIndex: Int, variableIndex: Int) = jacopDomains(solutionIndex)(variableIndex)
    def value(s: Int, v: Int): Int = toInt(domain(s,v))
    def solution(s: Int): Array[Int] = jacopDomains(s).map(toInt)
    def solutionMap(s: Int): Map[Var[T], Int] = 
      ( for (i <- 0 until nVariables) yield (variables(i), value(s, i)) ) .toMap
    def valueVector(v: Var[T]): Vector[Int] = 
      ( for (s <- 0 until nSolutions) yield value(s, indexOf(v)) ) .toVector   
    override def toString = s"Solutions([nSolutions=$nSolutions][nVariables=$nVariables])" 
  }
  
  object jacop {  

    object Settings {
      var defaultInterval: Interval = Interval(-1000, 1000)
      var defaultObjective: Objective = Satisfy
      var defaultSelect: Indomain = IndomainRandom
      var verbose: Boolean = true
      var debug: Boolean = false
      var warningPrinter: String => Unit = (s: String) => println("WARNING: " + s)
    }
   
    type JIntVar = JaCoP.core.IntVar
    type JVar = JaCoP.core.Var
    type JBooleanVar = JaCoP.core.BooleanVar
    
    sealed trait Indomain { def toJacop: JaCoP.search.Indomain[JIntVar] }
    case object IndomainMax  extends Indomain { def toJacop = new JaCoP.search.IndomainMax[JIntVar] }
    case object IndomainMedian  extends Indomain { def toJacop = new JaCoP.search.IndomainMedian[JIntVar] }
    case object IndomainMiddle  extends Indomain { def toJacop = new JaCoP.search.IndomainMiddle[JIntVar] }
    case object IndomainMin  extends Indomain { def toJacop = new JaCoP.search.IndomainMin[JIntVar] }
    case object IndomainRandom  extends Indomain { def toJacop = new JaCoP.search.IndomainRandom[JIntVar] }
    case object IndomainSimpleRandom  extends Indomain { def toJacop = new JaCoP.search.IndomainSimpleRandom[JIntVar] }
    
  //  case object IndomainSetMax  extends Indomain { def toJacop = new JaCoP.search.IndomainMax[JSetVar] }
  //  case object IndomainSetMin  extends Indomain { def toJacop = new JaCoP.search.IndomainMax[JSetVar] }
  //  case object IndomainSetRandom  extends Indomain { def toJacop = new JaCoP.search.IndomainMax[JSetVar] }
    //case object IndomainHierarchical  extends Indomain 
    //case object IndomainList  extends Indomain  
  /*   
    sealed trait Comparator   
    case object LargestDomain extends Comparator { def toJacop = new JaCoP.search.IndomainMax[JIntVar] }
    case object LargestMax extends Comparator
    case object LargestMin extends Comparator
    case object MaxCardDiff extends Comparator
    case object MaxGlbCard extends Comparator
    case object MaxLubCard extends Comparator
    case object MaxRegret extends Comparator
    case object MinCardDiff extends Comparator
    case object MinDomainOverDegree extends Comparator
    case object MinGlbCard extends Comparator
    case object MinLubCard extends Comparator
    case object MostConstrainedDynamic extends Comparator
    case object MostConstrainedStatic extends Comparator
    case object SmallestDomain extends Comparator
    case object SmallestMax extends Comparator
    case object SmallestMin extends Comparator
    case object WeightedDegree extends Comparator
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
    }         
     
    case class Solver[T](
        constraints: Seq[Constr[T]] , 
        objective: Objective = jacop.Settings.defaultObjective,
        timeOutOption: Option[Long] = None,
        solutionLimitOption: Option[Int] = None,
        indomain: Indomain = jacop.Settings.defaultSelect,
        assignOption: Option[Seq[Var[T]]] = None
      ) extends SolverUtils {
 
      import JaCoP. { search => jsearch, core => jcore, constraints => jcon }  
      
      def toJCon[T](constr: Constr[T], store: jcore.Store, jIntVar: Map[Var[T], JIntVar]): jcon.Constraint = {
        def jVarArray(vs: Seq[Var[T]]) = vs.map(v => jIntVar(v)).toArray
        constr match {
          case AbsXeqY(x, y) => new jcon.AbsXeqY(jIntVar(x), jIntVar(y))
          case AllDifferent(vs) => new jcon.Alldiff(jVarArray(vs))
          case IndexValue(ix, vs, v) => new jcon.Element(jIntVar(ix), jVarArray(vs), jIntVar(v))
          case Sum(vs, x) => new jcon.Sum(vs.map(v => jIntVar(v)).toArray, jIntVar(x))
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
          case c => println("Constr to jacop match error: " + c); ???
        }
      }
      
      lazy val domainOf: Map[Var[T], Seq[Interval]] = buildDomainMap(constraints)
   
      def varToJIntVar(v: Var[T], s: jcore.Store): JIntVar = {
        val intDom = new jcore.IntervalDomain()
        domainOf(v).foreach(ivl => intDom.addDom( new jcore.IntervalDomain(ivl.min, ivl.max)))
        new JIntVar(s, v.ref.toString, intDom) 
      }

      val minimizeHelpVarName = "_$minimize0"

      def collectIntVars(s: jcore.Store): Array[JIntVar] = 
        s.vars.collect { case x: JIntVar if (x.id != minimizeHelpVarName) => x } .toArray
              
      def solutionMap(s: jcore.Store, nameToVar: Map[String, Var[T]] ): Map[Var[T], Int] = 
            collectIntVars(s).filter(_.singleton).map(iv => (nameToVar(iv.id), iv.value) ).toMap
 
      def solve: Result[T] = {
        val store = new jcore.Store
        val vs = distinctVars(constraints)
        val cs = collectConstr(constraints)
        if (Settings.verbose) println("*** VARIABLES:   " + vs.mkString(","))
        if (Settings.verbose) println("*** CONSTRAINTS: " + cs.mkString(","))
        val duplicates = checkUniqueToString(vs)
        if (!duplicates.isEmpty) 
          return Result(SearchFailed("Duplicate toString values of variables:" + 
            duplicates.mkString(", "))        )
        if (checkIfNameExists(minimizeHelpVarName, vs)) 
          return Result(SearchFailed("Reserved varable name not allowed:" + minimizeHelpVarName))
        val intVarMap: Map[Var[T], JIntVar] = vs.map { v => (v, varToJIntVar(v, store)) } .toMap
        Some(objective).collect { //abort if cost variable is not defined
          case opt: Optimize[T] if (!intVarMap.isDefinedAt(opt.cost)) => 
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
        val select = new jsearch.InputOrderSelect[JIntVar](store, variablesToAssign, indomain.toJacop) 
        def solutionNotFound = Result[T](SolutionNotFound)
        def solutionInStore = solutionMap(store, nameToVarMap(vs))
        def interuptOpt: Option[SearchInterupt] = 
          if (label.timeOutOccured) Some(SearchTimeOut) 
          else if (listener.solutionLimitReached && solutionLimitOption.isDefined) 
            Some(SolutionLimitReached)
          else None
        def oneResult(ok: Boolean) = 
          if (ok) Result(SolutionFound, 1, solutionInStore, interuptOpt)  
          else solutionNotFound
        def countResult(ok: Boolean, i: Int) = 
          if (ok) Result(SolutionFound, i, solutionInStore, interuptOpt) 
          else solutionNotFound
        val conclusion = objective match {
          case Satisfy => 
            setup(searchAll = false , recordSolutions = true )
            oneResult(label.labeling(store, select)) 
          case Count => //count soultions but don't record any solution to save memory
            setup(searchAll = true , recordSolutions = false )
            countResult(label.labeling(store, select), listener.solutionsNo) 
          case FindAll => 
            setup(searchAll = true , recordSolutions = true )
            val found = label.labeling(store, select)
            if (!found) solutionNotFound else {
              //val vmap = nameToVarMap(vs)
              //AARGH! getSolutions() array of array may include null after last solution...
              // val aargh = listener.getSolutions()
              // for (i <- 0 until aargh.size) 
                // if (aargh(i) == null) println(s"*** DEBUG aargh($i) == null")
              // val values: Array[Array[Option[Int]]] = 
                // listener.getSolutions().filterNot(_ == null).map( domains => 
                  // domains.collect { 
                    // case intDom: jcore.IntDomain if intDom.singleton() => Some(intDom.value)
                    // case _ => None
                  // } 
                // )
              // val v: Array[_ <: JVar] = listener.getVariables()
              // val solutions: Vector[Map[Var[T], Int]] = values.map { d => 
                // ( for (i <- 0 until d.size if d(i).isDefined) 
                  // yield (vmap(v(i).id), d(i).get) ).toMap
              // } .toVector
              val solutions = new Solutions(
                    listener.getSolutions(), 
                    listener.getVariables(), 
                    listener.solutionsNo(), 
                    solutionInStore)
              Result(SolutionFound, solutions.nSolutions, solutionInStore, interuptOpt, Some(solutions))
            }
          case minimize: Minimize[T] => 
            listener.searchAll(false)
            listener.recordSolutions(true)
            oneResult(label.labeling(store, select, intVarMap(minimize.cost)))
          case m: Maximize[T] =>  
            listener.searchAll(false)
            listener.recordSolutions(true)
            val intDom = new jcore.IntervalDomain()
            domainOf(m.cost) foreach (ivl => intDom.addDom( new jcore.IntervalDomain(-ivl.max, -ivl.min)))
            val negCost = new JIntVar(store, minimizeHelpVarName, intDom)
            store.impose( new jcon.XmulCeqZ(intVarMap(m.cost), -1, negCost) )
            oneResult(label.labeling(store, select, negCost))
          case other => 
            println("Search objective not yet implemented: " + other)
            ???
            solutionNotFound
        }
        if (Settings.verbose) println(store)
        conclusion
      } //end def solve
      
    } //end class Solver
  } // end object jacop
} //end package reqT
