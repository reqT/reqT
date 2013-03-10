Model(
  TestCase("pass") has (Code(""" "hel" + "lo" """), Expectation("hello")),
  TestCase("fail") has (Code(""" "hell" + "lo" """), Expectation("hello"))
).tested.testFailed.size