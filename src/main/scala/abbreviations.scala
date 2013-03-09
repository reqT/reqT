package reqt {
  object abbrev { //TODO check against metamodel for missing entities
    def P(id: String) = reqt.Product(id)   
    def P(id: Int) = reqt.Product(id.toString)   
    def Rel(id: String) = reqt.Release(id) 
    def Rel(id: Int) = reqt.Release(id.toString) 
    def S(id: String) = reqt.Stakeholder(id) 
    def S(id: Int) = reqt.Stakeholder(id.toString) 
    def G(id: String) = reqt.Goal(id) 
    def G(id: Int) = reqt.Goal(id.toString) 
    def R(id: String) = reqt.Req(id)
    def R(id: Int) = reqt.Req(id.toString)
    def F(id: String) = reqt.Feature(id)   
    def F(id: Int) = reqt.Feature(id.toString)   
    def Fun(id: String) = reqt.Function(id)   
    def Fun(id: Int) = reqt.Function(id.toString)   
    def Q(id: String) = reqt.Quality(id)   
    def Q(id: Int) = reqt.Quality(id.toString)   
  }
}