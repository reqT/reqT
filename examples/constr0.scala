// a simple constraint problem
Seq(
  Var("x") #>= Var("y"), 
  Var("x")::Interval(1,10),
  Var("y")::Interval(2,7)
).solve(Satisfy)

// constraints as a model element
val cs = Constraints(Feature("x").Prio #>= Feature("y").Prio)
val Result(conlusion, solutionList) = cs.satisfy

// constraints in a requirements model
val m = Model(Stakeholder("user") has cs)
m.constraints.satisfy
