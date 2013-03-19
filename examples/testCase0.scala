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

val m3 = Model(
  TestCase("addRemoveEmpty") has (
    Input("val m = Model()"),
    Code("(m + F(1) - F(1)) == m"), Expectation("true"))
)

val hello = Model(
  Goal("demo") has Gist("Demonstrate basic availability."),  
  Feature("hello") has Spec("The system shall concat 'hello' with 'world'."),
  UserStory("init") has Spec("As a newbee I want to execute hello world."),
  Feature("hello") helps Goal("demo"),
  UserStory("init") helps Goal("demo"),
  TestCase("concat") verifies (Feature("hello"), UserStory("init")),
  TestCase("concat") has (
    Input("""val (input1, input2) = ("hello","world") """),
    Code("input1 + input2"),
    Expectation("hello world")
  )
)