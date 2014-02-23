//script for reqT metaspecification

case class TestOk(ok: Boolean)
implicit class AnyShouldEqual(a: Any) { 
  def <==> (that: Any) = TestOk(a == that) 
}
implicit class StringTestIf(s: String) { 
  def testIf(t: TestOk):String = if (t.ok) "" else s"TEST FAILED: $s \n" 
}

"Add empty" .testIf {   
    Model() ++ Model() <==> Model()
  } +
"Add same attribute" .testIf {   
    Model(Spec("a")) ++ Model(Spec("b")) <==> Model(Spec("b")) 
  } +
"Add same entity"    .testIf { 
    Model(Ent("a")) ++ Model(Ent("b")) <==> Model(Ent("a"),Ent("c")) 
  }  