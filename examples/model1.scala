val m = Model(
  Feature("x") has (Gist("x"), Status(DROPPED)),
  Feature("y") has (Gist("x"), Status(DROPPED)),
  Feature("z") has (Gist("x"), Status(SPECIFIED)),
  Feature("x") requires Feature("y"),
  Feature("z") requires Feature("x")
)

val m2 = m - has.Status(DROPPED) 

val m3 = Model(
  Feature("x") has Gist("x"),
  Feature("y") has Gist("x"),
  Feature("z") has (Gist("x"), Status(SPECIFIED)),
  Feature("x") requires Feature("y"),
  Feature("z") requires Feature("x")
)

assert(m3 == m2)

val m4 = m + Feature("x").has(Prio(3)) 
val m5 = m4 - Feature("x").has(Gist("y"),Status, Prio(3))

val m6 = Model(
  Feature("x") has Gist("x"),
  Feature("y") has (
    Gist("x"),
    Status(DROPPED)
  ),
  Feature("z") has (
    Gist("x"),
    Status(SPECIFIED)
  ),
  Feature("x") requires Feature("y"),
  Feature("z") requires Feature("x")
)

assert(m6 == m5)

val m7 = m - Set[Entity](Feature("x"))
val m8 = m - Feature("x")

assert(m7 == m8)

m - ((m / Feature / Status(DROPPED)).entities)

