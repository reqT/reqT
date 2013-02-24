val m1 = Model(
  Release("r1") precedes Release("r2"),
  Feature("y") precedes Feature("y"))
  
val m2 = Model(
  Release("r1") has Order(1),
  Release("r2") has Order(2),
  Feature("y") has Order(1),
  Feature("y") has Order(2)) 
  
val m3 = Model(
  Release("r1") precedes Release("r2"),
  Feature("y") precedes Feature("y"),
  Release("r1") requires Feature("y"),  //requires => implements/releases/includes??
  Release("r2") requires Feature("x")) 
  
