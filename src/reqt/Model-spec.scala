//script for reqT metaspecification

  "Add empty"          .test {   
    Model() ++ Model() <==> Model()
  } +

  "Add same attribute" .test {   
    Model(Spec("a")) ++ Model(Spec("b")) <==> Model(Spec("b")) 
  } +

  "Add same entity"    .test { 
    Model(Ent("a")) ++ Model(Ent("b")) <==> Model(Ent("a"),Ent("b")) 
  }  