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

val duality = Model(
  F(1) has Prio(1), 
  F(2) owns F(21), 
  F(3) has Submodel(F(31) has Prio(31), F(32) has Prio(32)),
  F(5) has Submodel(F(51) has Prio(51), F(52) has Prio(52)),
  F(4) owns (F(41), F(42)),
  F(4) has Submodel(F(43), F(44)),
  F(41) has Gist("hej"),
  F(41) owns F(411)
  )

  val circular = Model(
    F(1) owns F(2), F(2) owns F(3), F(3) owns F(1), 
    F(4) owns F(5), F(5) owns F(4),
    F(6) owns (F(61), F(62)),
    F(61) owns (F(611), F(612))
  )

val sm = Model(
  Stakeholder("a") has Submodel(
    F(1) has Prio(1),
    F(2) has Prio(2),
    F(3) has Prio(3)
   ),
  Stakeholder("b") has Submodel(
    F(1) has Prio(4),
    F(2) has Prio(5),
    F(3) has Prio(6)
   ),
  Stakeholder("c") has Submodel(
    F(1) has Prio(7),
    F(2) has Prio(8),
    F(3) has Prio(9)
   ),
  Stakeholder("d") has Submodel(
    F(1) has Prio(10),
    F(2) has Prio(11),
    F(3) has Prio(12)
   )
)
  