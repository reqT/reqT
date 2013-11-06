// a constraint problem
Seq(
  Var("x") #>= Var("y"), 
  Var("x")::Interval(1,10),
  Var("y")::Interval(2,7)
).solve(Satisfy)

// constraints as a model element
val cs = Constraints(
  (Feature("x")!Prio) #> 0,
  (Feature("y")!Prio) #> 0,
  (Feature("x")!Prio) #>= (Feature("y")!Prio)
)

val Result(conclusion, solutionCount, lastSolution, interuptOption, solutionsOption)   
  = cs.satisfy

// constraints in a requirements model
val m = Model(Stakeholder("user") has cs)
val result = m.constraints.satisfy
val newModel = m.constraints.toModel
val (extendedModel, _) = m.impose(m.constraints).solve(Satisfy)

// use of assignOption to only assign solutions to only some vars
val v = vars("x","y","z")
Seq(XplusYeqZ(v(0),v(1),v(2)), v::{1 to 10}).
  solve(Satisfy,assignOption = Some(Seq(v(2))))

//a bigger example
val priorities = Vector(Feature("x")!Prio, Feature("y")!Prio)
var m2 = Model(
  Subdomain("frame") has Constraints(
    priorities::{1 to 100},
    AllDifferent(priorities),
    SumEq(priorities,Var("a")),
    Var("a") #== 100,
    XmulYeqZ(Var("a"),Var("b"),Var("c"))
  )
)
val Result(_, n, _, interuptOption, _) = 
  m2.constraints.solve(CountAll, timeOutOption = Some(1))
val Result(_, n, _, interuptOption, Some(solutions)) =
  m2.constraints.solve(FindAll,solutionLimitOption = Some(10))
println("solutions.variables.toVector == " + solutions.variables.toVector)
println("solutions.nSolutions == " + solutions.nSolutions)
println("solutions.solutionMap(0) == " + solutions.solutionMap(0))
val valueOfFirstVariableOfFirstSolution = solutions.solutionMap(0)(solutions.variables(0)) 