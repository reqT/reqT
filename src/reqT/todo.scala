package reqT

object todo {
  def list = Model(
    Req("metaprog") has Spec("generate metamodel automagically"),
    Req("check all operators") has Spec("esp. * *~ *^ "),
    Req("path"),
    Req("path visit"),
    Req("UTF-8")
  )
}