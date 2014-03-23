//script for reqT metaspecification
"AGREGATION should be idempotent over empty model".test { 

Model() ++ Model() == Model()

} +"Aggregation should be idempotent over entity".test {

Model(Req("r")) ++ Model(Req("r")) == Model(Req("r")) 

} + "Aggregation should be idempotent over attribute".test {

Model(Spec("s")) ++ Model(Spec("s")) == Model(Spec("s")) 

} + "Aggregation should be idempotent over relation".test {

Model(Req("r") has Spec("s")) ++ Model(Req("r") has Spec("s")) == Model(Req("r") has Spec("s")) 

} + "Aggregation should join tails".test {

Model(Req("a") has Spec("s")) ++ Model(Req("a") has Prio(2)) == Model(Req("a") has (Spec("s"),Prio(2))) 

} + "Add entity to empty model".test { 

Model() + Req("x") == Model(Req("x"))

} + "Add attribute to empty model".test { 

Model() + Spec("x") == Model(Spec("x"))

} +
"Add relation to empty model".test { 

Model() + (Req("x") has Spec("s")) == Model(Req("x") has Spec("s"))

} +
"Add of existing attribute type should overwrite" .test { 

Model(Spec("a")) + Spec("b") == Model(Spec("b")) 

} +
"Add of existing entity type should not overwrite".test { 

Model(Req("a")) + Req("b") == Model(Req("a"), Req("b"))

} +
"Add to same relation should join submodels".test { 

Model(Req("a") has Spec("s")) + (Req("a") has Prio(2)) == Model(Req("a") has (Spec("s"),Prio(2)))

} +
"Enter entity.has should give submodel".test {

Model(Req("a") has Spec("s")) / Req("a").has == Model(Spec("s")) 

} +
"Enter entity should be same as enter entity.has".test {

Model(Req("a") has Spec("s")) / Req("a") == Model(Req("a") has Spec("s")) / Req("a").has

} +
"Restrict on entity type should exclude other entity types". test {

Model(Req("a"), Feature("b"), Req("b")) * Req == Model(Req("a"),Req("b"))

} +
"Restrict on entity type should include tail entities of that types". test {

Model(Feature("a") requires (Req("b"), Feature("a"))) * Req == Model(Req("a"),Req("b"))

} 












