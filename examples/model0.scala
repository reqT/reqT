val m = Model(
	Feature("a") has (Gist("hello a"), Spec("world a")),
	Feature("b") has (Gist("hello b"), Spec("world b"), Status(DROPPED)),
	Feature("c") has (Gist("hello c"), Spec("world c")),
	Feature("d") has (Gist("hello d"), Spec("world d"), Status(SPECIFIED)),
	Feature("e") has (Gist("hello e"), Spec("world e")),
	Feature("a") requires Feature("b"),
	Feature("a") excludes Feature("d"),
	Feature("c") requires Feature("x"),
	Feature("x") requires Feature("e")
)