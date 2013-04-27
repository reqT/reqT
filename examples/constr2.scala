val m = Model(Subdomain("fr1") has Constraints(
  (Feature("x")!Prio) #== 42, 
  (Feature("y")!Prio) #> 3, 
  (Feature("y")!Prio) #< (Feature("x")!Prio) ))
 
val m2 = m.impose(m / Subdomain("fr1") !! Constraints) satisfy
  
val s: String = m.toScala  
val m3 = Model.interpret(s)
if (m3 == m) println("It works!")  