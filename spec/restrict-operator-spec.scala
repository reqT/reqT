{
  val m = Model(
    Attr("a"),
    Ent("x"),
    Ent("y") has Attr("a"), 
    Ent("y") requires Ent("z"),
    Ent("z") has (Ent("x") has Attr("a")),
    Ent("w") has (Ent("y") has (Ent("x") has Attr("a"))))
    
  "Aggregation of empty models".test { 
    Model() * Ent("x") == Model()
  }
}