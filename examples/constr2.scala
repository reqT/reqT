val m = Model(Subdomain("fr1") has Constraints(
  (Feature("x")!Prio) #== 42, 
  (Feature("y")!Prio) #> 3, 
  (Feature("y")!Prio) #< (Feature("x")!Prio) ))
 
val m2 = m.impose(m / Subdomain("fr1") !! Constraints) satisfy
  
val s: String = m.toScala  
val m3 = Model.interpret(s)
if (m3 == m) println("It works!")  

def rect(n: Int) = Rectangle(Var("x"+n),Var("y"+n),Var("dx"+n),Var("dy"+n))
val bounds = (rect(0).variables ++ rect(1).variables)::Interval(10,100)
val diff = Diff2(rect(0), rect(1))  
val distX = Distance(rect(0).x, rect(1).x, Var("distX"))
val distY = Distance(rect(0).y, rect(1).y, Var("distY"))
val sum = XplusYeqZ(Var("distX"), Var("distY"), Var("sum"))
val Result(c,s) = Constraints(bounds,diff,distX,distY,sum).minimize(Var("sum"))