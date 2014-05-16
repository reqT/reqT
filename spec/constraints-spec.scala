val m = Model(
  Stakeholder("Anna") requires (
      Feature("x")/Prio > 3,
      Feature("y")/Prio > 1,
      Feature("z")/Prio < 10),
  Constraints(Vector("x","y","z").map(Feature(_)/Prio :: {1 to 100}):_*),
  Constraints(Sum(Vector("x","y","z").map(s => Var(Feature(s)/Prio))) := Var("prioSum")  ))
      
"Priority constraints". test {
  m.satisfy == Model(
    Stakeholder("Anna"),
    Feature("x") has Prio(91),
    Feature("y") has Prio(4),
    Feature("z") has Prio(1),
    Constr(Var("prioSum") := 96))
}