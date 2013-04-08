var m = Model(
  Feature("x") has Spec("A feature specification"),
  Feature("y") has Spec("Another feature specification"),
  Stakeholder("s") has Constraints(
    Feature("x").Prio #< Feature("y").Prio,
    Feature("y").Prio #> 5,
    Feature("x").Prio :: Interval(1,100),
    Feature("y").Prio :: Interval(1,10)  
  )
)

val (maximized, priorities) = m.impose(m.constraints).solve(Maximize(Feature("x").Prio))

println( 
  maximized == m ++ Model(Feature("x") has Prio(9), Feature("y") has Prio(10))
)  // will print true
  



