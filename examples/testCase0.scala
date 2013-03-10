val m1 = Model(
  TestCase("testTested") has (Code("""
    Model(
      TestCase("pass") has (Code("\"hel\" + \"lo\" "), Expectation("hello")),
      TestCase("fail") has (Code("\"hell\" + \"lo\" "), Expectation("hello"))
    ).tested.testFailed.size
  """), Expectation("1"))
)

val m2 = Model(
  TestCase("testTested") has (External[Code]("examples/testCode0.scala"), Expectation("1"))
)