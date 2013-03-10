Model(
  Feature("helloWorld") has Gist("demonstrate basic availability"),
  TestCase("hello1") has (Code(""" "hel" + "lo" """), Expectation("hello")),
  TestCase("hello2") has (Code(""" "hell" + "lo" """), Expectation("hello")),
  TestCase("hello1") verifies Feature("hello"),
  TestCase("hello2") verifies Feature("hello")
).tested.testFailed.size