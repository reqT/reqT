//reqT metaspecification: DSL representation
"Map-Vector duality: Entity to Mapping".test { 
  Req("x").toMapping == Req("x"). has -> Model() 
} +
"Map-Vector duality: Mapping to Entity".test { 
  (Req("x"). has -> Model()).toElem == Req("x")
} +
"Map-Vector duality: Attribute to Mapping".test { 
  Spec("a").toMapping == Spec -> Spec("a") 
} +
"Map-Vector duality: Mapping to Attribute".test { 
  (Spec -> Spec("a")).toElem == Spec("a")
} 













