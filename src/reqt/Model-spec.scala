//script for reqT metaspecification
false &&
{ Model() ++ Model() == Model() } &&
{ Model(Spec("a")) ++ Model(Spec("b")) == Model(Spec("b")) }  &&
{ Model(Ent("a")) ++ Model(Ent("b")) == Model(Ent("a"),Ent("b")) }