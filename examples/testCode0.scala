val m = Model(
  Feature("helloWorld") has Gist("demonstrate basic availability"),
  TestCase("hello1") has (Code(""" "hel" + "lo" """), Expectation("hello")),
  TestCase("hello2") has (Code(""" "hell" + "lo" """), Expectation("hello")),
  TestCase("hello1") verifies Feature("hello"),
  TestCase("hello2") verifies Feature("hello")
)
m.tested.testFailed.foreach { 
  case (t, (o, e)) => println(s"*** Failed:$t|output:$o|expected:$e|") 
}
  
m.isTestOk 

println("number of failed test cases: " + m.tested.testFailed.size)

