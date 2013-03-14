val m = Model(
	Feature("a") has (Gist("hello a"), Spec("world a")),
	Feature("b") has (Gist("hello b"), Spec("world b"), Status(DROPPED)),
	Feature("c") has (Gist("hello c"), Spec("world c")),
	Feature("d") has (Gist("hello d"), Spec("world d"), Status(SPECIFIED)),
	Feature("e") has (Gist("hello e"), Spec("world e")),
  Feature("sm") has Submodel(Model(
    Feature("x") has Submodel(Model(Feature("a") has Order(7)))
  )),
	Feature("a") requires Feature("b"),
	Feature("b") requires Feature("q"),
	Feature("a") excludes Feature("d"),
	Feature("c") requires Feature("x"),
	Feature("x") requires Feature("e"),
	Feature("z") requires Feature("a"),
	Feature("q") requires Feature("a"),
	Feature("b") requires Feature("a"),
  Feature("x") assigns(Prio(3)) to Feature("p")
)

val m1 = Model(
	Feature("e") has (Gist("hello e"), Spec("world e"), Order(3), Prio(42)),
  Feature("sm") has Submodel(Model(
    Feature("smsm") has Submodel(Model(Feature("e") has Order(7)))
  ))
)

val deep = Model(
	Feature("top") has (Gist("hello e"), Spec("world e"), Order(3), Prio(42)),
  Feature("sub") has Submodel(Model(
    Feature("subsub") has Submodel(Model(Feature("subsubsub") has Order(7)))
  ))
)

val d2 = Model( F(1) has Prio(1), F(1) has Submodel(Model(
  F(2) has (Prio(2), Submodel(Model(F(3) has Prio(3))))
)))