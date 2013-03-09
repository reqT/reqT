val m = Model( 
  Issue("svPhys") has Gist("Physics examples lacks Swedish translation"),
  Issue("hiPhys") has Gist ("Physics examples lacks Hindi translation"),
  Resource("Bjorn") implements Issue("svPhys"), 
  Resource("Lalit") implements Issue("hiPhys"), 
  Ticket("translPhys") implements (Issue("svPhys"), Issue("hiPhys")),
  TestCase("hello") has (Code(""" def hello = "hello" ; hello"""), Expectation("hello")),
  TestCase("hello2") has (Code(""" def hello = "hello" ; hello"""), Expectation("hello2"))
) 
