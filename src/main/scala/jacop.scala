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
  object jacop {  

    object Settings {
      var defaultInterval = Interval(-10000, 10000)
      var defaultObjective = Satisfy
      var defaultSelect = IndomainRandom
      var verbose = true
      var warningPrinter: String => Unit = (s: String) => println("WARNING: " + s)
    }

    implicit def constraintsToSeq[T](cs: Constraints[T]): Seq[Constr[T]] = cs.cs
    implicit def seqToConstraints[T](cs: Seq[Constr[T]]): Constraints[T] = Constraints(cs:_*)
    implicit def rangeToInterval(r: Range): Interval = Interval(r.min, r.max)

    implicit class RangeSeqOps(rs: Seq[Range]) { //to enable > Var("x")::Seq(1 to 10, 12 to 15)
      def ::[T](v: Var[T]): Bounds[T] = Bounds(Seq(v), rs.map(rangeToInterval(_)))
    }
        
    def vars[T](vs: T *): Seq[Var[T]] = vs.map(Var(_)).toIndexedSeq
 
    case class Constraints[+T](cs: Constr[T] *) {
      def solve[B >: T](
          objective: Objective = Settings.defaultObjective,
          select: jacop.Indomain = Settings.defaultSelect
        ): Result[B] = jacop.Solver[B](cs, objective, select).solve
    }

    case class Interval(min: Int, max: Int) {
      if (min > max) Settings.warningPrinter("Negative interval min > max: " + this )
      def ::[T](v: Var[T]): Bounds[T] = Bounds(Seq(v), Seq(this))
      def ::[T](vs: Seq[Var[T]]): Bounds[T] = Bounds(vs, Seq(this))
      def ::[T](b: Bounds[T]): Bounds[T] = Bounds(b.seq1, b.domain ++ Seq(this))
    }  
    
    case class Result[T](conclusion: Conclusion, solutions: Seq[Map[Var[T], Int]])  
   
    sealed trait Conclusion
    case object SolutionFound       extends Conclusion
    case object SolutionNotFound    extends Conclusion
    case object InconsistencyFound  extends Conclusion
    case class SearchFailed(msg: String)   extends Conclusion  
     
    sealed trait Objective   
    case object Satisfy extends Objective
    case class Minimize[+T](cost: Var[T]) extends Objective
    case class Maximize[+T](cost: Var[T]) extends Objective
    case object CountAll extends Objective
    case object RecordAll extends Objective //TODO check jacop example gates.java to see how listener is built

    //implicit def anyToVar[T](ref: T): Var[T] = Var(ref)  //too dangerous??
    case class Var[+T](ref: T) { 
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
    }
    // object Var { 
      // private var nextAnonymousVariableNumber = 0
      // private def nextVarNum() = { nextAnonymousVariableNumber += 1; nextAnonymousVariableNumber }
      // def anonVarName() = "v"+nextVarNum
      // def apply() = new Var(anonVarName()) 
    // }
    

    trait BoundingConstr //marker trait to enable Bounds filter
    
    trait Constr[+T]{ 
      val variables: Seq[Var[T]] 
    }
    trait Constr1[+T] extends Constr[T] { 
      val x: Var[T]
      val variables: Seq[Var[T]] = Seq(x) 
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
    }
    trait Constr1Seq1[+T] extends Constr[T] { 
      val x: Var[T]
      val seq1: Seq[Var[T]]
      val variables: Seq[Var[T]] = Seq(x) ++ seq1 
    }
    trait Constr2Seq1[+T] extends Constr[T] { 
      val x: Var[T]; val y: Var[T]
      val seq1: Seq[Var[T]]
      val variables: Seq[Var[T]] = Seq(x, y) ++ seq1
    }
    
    case class Bounds[+T](seq1: Seq[Var[T]], domain: Seq[Interval]) 
    extends ConstrSeq1[T] with BoundingConstr {
      def addDomainOf[B >:T](that: Bounds[B]): Bounds[B] = Bounds[B](seq1, domain ++ that.domain)
    }
    object Bounds {
      def apply[T](v: Var[T], ivls: Interval *) = new Bounds(Seq(v), ivls) 
      def apply[T](vs: Var[T] *) = new Bounds(vs, Seq()) 
    }
    
    case class AbsXeqY[+T](x: Var[T], y: Var[T]) extends Constr2[T]
    case class AllDifferent[+T](seq1: Seq[Var[T]]) extends ConstrSeq1[T]
    case class Element[T](index: Var[T], varSeq: Seq[Var[T]], value: Var[T]) extends Constr2Seq1[T] {
      val x = index
      val y = value
      val seq1 = varSeq
    }
    case class Sum[+T](seq1: Seq[Var[T]], x: Var[T]) extends Constr1Seq1[T] 
    case class XdivYeqZ[+T](x: Var[T], y: Var[T], z: Var[T]) extends Constr3[T]
    case class XeqC[+T](x: Var[T], c: Int) extends Constr1[T]
    case class XeqY[+T](x: Var[T], y: Var[T]) extends Constr2[T]
    case class XexpYeqZ[+T](x: Var[T], y: Var[T], z: Var[T]) extends Constr3[T]
    case class XgtC[+T](x: Var[T], c: Int) extends Constr1[T]
    case class XgteqC[+T](x: Var[T], c: Int) extends Constr1[T]
    case class XgteqY[+T](x: Var[T], y: Var[T]) extends Constr2[T]
    case class XgtY[+T](x: Var[T], y: Var[T]) extends Constr2[T]
    case class XltC[+T](x: Var[T], c: Int) extends Constr1[T]
    case class XlteqC[+T](x: Var[T], c: Int) extends Constr1[T]
    case class XlteqY[+T](x: Var[T], y: Var[T]) extends Constr2[T]
    case class XltY[+T](x: Var[T], y: Var[T]) extends Constr2[T]
    case class XneqC[+T](x: Var[T], c: Int) extends Constr1[T]
    case class XneqY[+T](x: Var[T], y: Var[T]) extends Constr2[T]
    case class XeqBool[+T](x: Var[T], c: Boolean) extends Constr1[T]
    
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
          if (!result.isDefinedAt(v)) result += v -> Seq(Settings.defaultInterval)
        }
        result
      }
      def nameToVarMap[T](vs: Seq[Var[T]]): Map[String, Var[T]] = vs.map(v => (v.ref.toString, v)).toMap
      //def varToName[T](vs: Seq[Var[T]]): Map[Var[T], String] = vs map (v => (v, v.ref.toString)) toMap
      def checkUniqueToString[T](vs: Seq[Var[T]]): Set[String] = {
        val strings = vs.map(_.ref.toString)
        strings.diff(strings.distinct).toSet
      }
      def checkIfNameExists[T](name: String, vs: Seq[Var[T]]): Boolean = 
        vs.exists { case Var(ref) => ref.toString == name }
    }    
    
    
    //------ jacop interfacing
    
    type JIntVar = JaCoP.core.IntVar
    type JBooleanVar = JaCoP.core.BooleanVar
   
    sealed trait Indomain { def toJacop: JaCoP.search.Indomain[JIntVar] }
    case object IndomainMax  extends Indomain { def toJacop = new JaCoP.search.IndomainMax[JIntVar] }
    case object IndomainMedian  extends Indomain { def toJacop = new JaCoP.search.IndomainMedian[JIntVar] }
    case object IndomainMiddle  extends Indomain { def toJacop = new JaCoP.search.IndomainMiddle[JIntVar] }
    case object IndomainMin  extends Indomain { def toJacop = new JaCoP.search.IndomainMin[JIntVar] }
    case object IndomainRandom  extends Indomain { def toJacop = new JaCoP.search.IndomainRandom[JIntVar] }
    case object IndomainSimpleRandom  extends Indomain { def toJacop = new JaCoP.search.IndomainMax[JIntVar] }
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
    
    case class Solver[T](
        constraints: Seq[Constr[T]], 
        objective: Objective,
        indomain: Indomain
      ) extends SolverUtils {
 
      import JaCoP. { search => jsearch, core => jcore, constraints => jcon }  
      
      def toJCon[T](constr: Constr[T], store: jcore.Store, jIntVar: Map[Var[T], JIntVar]): jcon.Constraint = {
        def jVarArray(vs: Seq[Var[T]]) = vs.map(v => jIntVar(v)).toArray
        constr match {
          case AbsXeqY(x, y) => new jcon.AbsXeqY(jIntVar(x), jIntVar(y))
          case AllDifferent(vs) => new jcon.Alldiff(jVarArray(vs))
          case Element(ix, vs, v) => new jcon.Element(jIntVar(ix), jVarArray(vs), jIntVar(v))
          case Sum(vs, x) => new jcon.Sum(vs.map(v => jIntVar(v)).toArray, jIntVar(x))
          case XdivYeqZ(x, y, z) => new jcon.XdivYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
          case XeqC(x, c) => new jcon.XeqC(jIntVar(x), c)
          case XeqY(x, y) => new jcon.XeqY(jIntVar(x), jIntVar(y))
          case XexpYeqZ(x, y, z) => new jcon.XexpYeqZ(jIntVar(x), jIntVar(y), jIntVar(z))
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
        if (Settings.verbose) println("*** Solver VARIABLES:   " + vs.mkString(","))
        if (Settings.verbose) println("*** Solver CONSTRAINTS: " + cs.mkString(","))
        val duplicates = checkUniqueToString(vs)
        if (!duplicates.isEmpty) return Result(
            SearchFailed("Duplicate toString values of variables:" + duplicates.mkString(", ")), 
            Seq(Map())
        )
        if (checkIfNameExists(minimizeHelpVarName, vs)) return Result(
            SearchFailed("Reserved varable name not allowed:" + minimizeHelpVarName), 
            Seq(Map())
        )
        val intVarMap: Map[Var[T], JIntVar] = vs.map { v => (v, varToJIntVar(v, store)) } .toMap
        (objective match {
          case m: Maximize[T] => Some(m.cost) 
          case m: Minimize[T] => Some(m.cost)
          case _ => None
        } ). foreach { v => if (!intVarMap.isDefinedAt(v)) 
          return Result(SearchFailed("Cost variable not defined:" + v), Seq(Map())) 
        }
        cs foreach { c => store.impose(toJCon(c, store, intVarMap)) }
        if (!store.consistency) return Result(InconsistencyFound, Seq(Map()))
        val dfs = new jsearch.DepthFirstSearch[JIntVar]
        dfs.getSolutionListener.searchAll(false)
        dfs.getSolutionListener.recordSolutions(true)
        val select = new jsearch.InputOrderSelect[JIntVar](store, collectIntVars(store), indomain.toJacop) 
        val found = objective match {
          case Satisfy => dfs.labeling(store, select)
          case minimize: Minimize[T] => dfs.labeling(store, select, intVarMap(minimize.cost))
          case m: Maximize[T] =>  
            val intDom = new jcore.IntervalDomain()
            domainOf(m.cost) foreach (ivl => intDom.addDom( new jcore.IntervalDomain(-ivl.max, -ivl.min)))
            val negCost = new JIntVar(store, minimizeHelpVarName, intDom)
            store.impose( new jcon.XmulCeqZ(intVarMap(m.cost), -1, negCost) )
            dfs.labeling(store, select, negCost)
          case so => println("Search objective not yet implemented: " + so); ??? ; false
        }
        if (Settings.verbose) println(store)
        if (found) Result(SolutionFound, Seq(solutionMap(store, nameToVarMap(vs))))
        else Result(SolutionNotFound, Seq(Map()))
      } //end def solve
      
    } //end class Solver
  } // end object jacop
} //end package reqT
