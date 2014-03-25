Model(
  TestCase("aggregate") has Code(load("spec/aggregate-spec.scala")),
  TestCase("internalDSL") has Code(load("spec/DSL-spec.scala")),
  TestCase("hasEmpty") has Code("""
    "An entity that has nothing is an entity" .test { Model(Req("x").has()) == Model(Req("x")) }
  """)
)