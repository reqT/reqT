val m1 = Model( 
  Issue("greetSwedish") has Gist("Greeting msg lacks Swedish translation"),
  Issue("greetGerman") has Gist ("Greeting msg lacks German translation"),
  Resource("Bjorn") implements Issue("greetSwedish"), 
  Resource("Berbel") implements Issue("greetGerman"), 
  Ticket("translateGreetings") implements (Issue("greetSwedish"), Issue("greetGerman")),
  TestCase("helloSe") has (Code(""" def helloSe = "hejsan" ; helloSe"""), Expectation("hejsan")),
  TestCase("helloGe") has (Code(""" def helloGe = "hallo" ; helloGe"""), Expectation("hola"))
) 

