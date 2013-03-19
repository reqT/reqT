val m1 = Model( 
  Issue("svPhys") has Gist("Physics examples lacks Swedish translation"),
  Issue("hiPhys") has Gist ("Physics examples lacks Hindi translation"),
  Resource("Bjorn") implements Issue("svPhys"), 
  Resource("Lalit") implements Issue("hiPhys"), 
  Ticket("translPhys") implements (Issue("svPhys"), Issue("hiPhys")),
  TestCase("hello") has (Code(""" def hello = "hello" ; hello"""), Expectation("hello")),
  TestCase("hello2") has (Code(""" def hello = "hello" ; hello"""), Expectation("hello2"))
) 


val m = Model(
  F(7) has Order(3), 
  F(4) has Order(2), 
  F(3) has Order(8),
  F(6) has Order(1), 
  F(5) has Order(2), 
  F(8) has Order(3),  
  F(1), F(2),
  Release("r1") implements (F(7), F(4), F(3), F(1)),
  Release("r2") implements (F(6), F(2), F(8), F(5), F(4))
)

val m = ModelVector(m1,m2)