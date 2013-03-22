/****************************************************************     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2013, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/

package reqt {
  trait ToDo { 
    def todo: Model  
    def done: Model
    def halt: Model
    def drop: Model
    lazy val all = todo ++ done ++ halt ++ drop
  }
  object backlog extends ToDo {
    val todo = Model(
      Feature("ConstrToScala") has (
        Gist("Generate Scala code in toScala of Constraints"),
        Status(PLANNED),
        Prio(1)
      ),
      Feature("jacopConstr") has (
        Gist("Implement essential jacop constraints."),
        Status(PLANNED),
        Problem("Which constraints are essential?"),
        Prio(1)
      ),
      Feature("submodelConstr") has (
        Gist("Allow constraints on submodels."),
        Status(PLANNED),
        Prio(1)
      ),
      Feature("modelHistory") has (
        Gist("ModelHistory with edits Model => Model"),
        Spec("The system shall maintain a history of model edits in the form of functions Model => Model that can enable undo, replay, redo etc."),
        Status(SPECIFIED),
        Example("val mh = ModelHistory() + {m => m + (Feature(1))} \n mh.undo; mh.redo; mh.branch(name) "),
        Prio(2)
      ),
      Feature("smartNewline") has (
        Gist("Break lines better in m.toScala"),
        Status(PLANNED),
        Prio(2)
      ),
      Ticket("trippleQuotes") has (
        Gist("Investigate escape versus tripple quotes"),
        Status(PLANNED),
        Why("To better understande trade-offs between uggliness of backslash escape sequences and verbosity of tripple quotes. Investigate implementation issues."),
        Prio(2)
      ),
      Issue("toTableOfSubmodel") has (
        Gist("Current toTable does not work for submodels"),
        Status(PLANNED),
        Prio(2)
      ),
      Issue("toHtmlOfSubmodel") has (
        Gist("Current toTable does not work for submodels"),
        Status(PLANNED),
        Prio(2)
      ),
      Ticket("prioRange") has (
        Gist("Investigate all integer attributes if and how to allow range"),
        Status(ELICITED),
        Example("allow both Prio(3,9) and Prio(9) where Prio(9) == Prio(9,9) and Prio(Interval(3,4)) == Prio(3,4); or maybe add new attribute Estimate[Prio](3,9)"),
        Prio(3)
      ),
      Ticket("pandoc") has (
        Gist("Investigate integration with pandoc"),
        Status(PLANNED),
        Why("Maybe document generation can be improved and simplified with some kind of pandoc integration?"),
        Prio(3)
      ),
      Feature("toMarkdown") has (
        Gist("Generate markdown textual output from models"),
        Prio(3)
      ),
      Feature("setupTeardown") has (
        Gist("Add TestCase attributes Setup Teardown"),
        Status(ELICITED),
        Prio(5)
      ),
      Feature("toMarkdown") requires Ticket("pandoc")
    )
    val done = Model(
      Feature("collectConstraints") has (Gist("m.constraints, m.constraintsAll"), Status(IMPLEMENTED))
    )
    val halt = Model(
      Feature("javaId") has (Gist("Check that entity id is java identifier"), Problem("Is this good?"), Status.init),
      Feature("tagList") has (Gist("m / Tag(x) finds TagList(x,y)"), Problem("Is this good?"),Status.init)
    )
    val drop = Model()
  }
}
