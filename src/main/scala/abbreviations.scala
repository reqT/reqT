package reqt {
  object abbrev {
    def S(id: String) = Stakeholder(id) 
    def S(id: Int) = Stakeholder(id.toString) 
    def G(id: String) = Goal(id) 
    def G(id: Int) = Goal(id.toString) 
    def F(id: String) = Feature(id)   
    def F(id: Int) = Feature(id.toString)   
    def Q(id: String) = Quality(id)   
    def Q(id: Int) = Quality(id.toString)   
  }
}