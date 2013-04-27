// a simple constraint problem
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

val Result(conlusion, solutionList) = cs.satisfy

// constraints in a requirements model
val m = Model(Stakeholder("user") has cs)
val Result(c, ss) = m.constraints.satisfy
val newModel = m.constraints.toModel
val (extendedModel, _) = m.impose(m.constraints).solve(Satisfy)

//a bigger example
val priorities = Vector(Feature("x")!Prio, Feature("y")!Prio)
var m2 = Model(
  Subdomain("frame") has Constraints(
    priorities::{1 to 100},
    AllDifferent(priorities),
    Sum(priorities,Var("a")),
    Var("a") #== 100,
    XmulYeqZ(Var("a"),Var("b"),Var("c"))
  )
)
