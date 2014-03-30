{
  val m = Model(
    Attr("a"),
    Ent("x"),
    Ent("y") has Attr("b"), 
    Ent("y") requires Ent("z"),
    Ent("z") has (Ent("x") has Attr("c")),
    Ent("w") has (Ent("y") has (Ent("x") has Attr("a"))))
    
  "Restrict to entity".test { 
     m * Ent("x") == Model(
      Ent("x"),
      Ent("z") has (Ent("x") has Attr("c")),
      Ent("w") has (Ent("y") has (Ent("x") has Attr("a"))))
  }

  "Restrict to attribute value".test { 
     m * Attr("a") == Model(
      Attr("a"), 
      Ent("w") has (Ent("y") has (Ent("x") has Attr("a"))))
  }
  
  "Restrict to entity type".test { 
    Model(Ent("x"), Req("r")) * Ent == Model(Ent("x"))
  }
  
  "Restrict to attribute type".test { 
    m * Attr == Model(
      Attr("a"),
      Ent("y") has Attr("b"), 
      Ent("z") has (Ent("x") has Attr("c")),
      Ent("w") has (Ent("y") has (Ent("x") has Attr("a"))))
  }
}