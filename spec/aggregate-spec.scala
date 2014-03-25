"Aggregation of empty models".test { 
  Model() ++ Model() == Model()
} +
"Aggregation should be idempotent over entity".test {
  Model(Req("x")) ++ Model(Req("x")) == Model(Req("x")) 
} + 
"Aggregation should be idempotent over attribute".test {
  Model(Spec("s")) ++ Model(Spec("s")) == Model(Spec("s")) 
} + 
"Aggregation should be idempotent over relation".test {
  Model(Req("x") has Spec("s")) ++ Model(Req("x") has Spec("s")) == Model(Req("x") has Spec("s")) 
} + 
"Aggregation should join tails".test {
  Model(Req("a") has Spec("s")) ++ Model(Req("a") has Prio(2)) == Model(Req("a") has (Spec("s"),Prio(2))) 
} +
"Aggregation should overwrite same attribute type".test {
  Model(Req("x") has Spec("a")) ++ Model(Req("x") has Spec("b")) == Model(Req("x") has Spec("b")) 
} +
"Aggregation should note overwrite same entity type".test {
  Model(Req("x")) ++ Model(Req("y")) == Model(Req("x"), Req("y")) 
}











