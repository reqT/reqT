/***     
**                  _______        
**                 |__   __|   reqT - a requirements engineering tool  
**   _ __  ___   __ _ | |      (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
**************************************************************************/
package reqT
import scala.language.postfixOps

package object meta {
  def gen(m: Model = model): String = new MetaReqT(m).toScala
  lazy val model = Model(
    Meta("Entity") superOf (
      Meta("General") superOf ( 
        Meta("Item"), Meta("Label"), Meta("Meta"), Meta("Section"), Meta("Term")),
      Meta("Context") superOf (
          Meta("Actor"), Meta("App"), Meta("Component"), Meta("Domain"), Meta("Module"),
          Meta("Product"), Meta("Release"), Meta("Resource"), Meta("Risk"), Meta("Service"), 
          Meta("Stakeholder"), Meta("System"), Meta("User")),
      Meta("Requirement") superOf (
        Meta("DataReq") superOf (
          Meta("Class"), Meta("Data"), Meta("Input"), Meta("Member"), Meta("Output"), Meta("Relationship")),
        Meta("DesignReq") superOf (Meta("Design"), Meta("Screen"), Meta("MockUp")),
        Meta("FunctionalReq") superOf (
          Meta("Function"), Meta("Interface"), Meta("State"), Meta("Event")), 
        Meta("GeneralReq") superOf (
          Meta("Epic"), Meta("Feature"), Meta("Goal"), Meta("Idea"), Meta("Issue"), 
          Meta("Req"), Meta("Ticket"), Meta("WorkPackage")), 
        Meta("QualityReq") superOf (
          Meta("Breakpoint"), Meta("Barrier"), Meta("Quality"), Meta("Target")), 
        Meta("ScenarioReq") superOf (
          Meta("Scenario"), Meta("Task"), Meta("Test"), Meta("Story"), Meta("UseCase")),
        Meta("VariabilityReq") superOf (Meta("VariationPoint"), Meta("Variant")))),
    Meta("RelationType") superOf (
      Meta("binds"), Meta("deprecates"), Meta("excludes"), 
      Meta("has"), Meta("helps"), Meta("hurts"), Meta("impacts"),
      Meta("implements"), Meta("interactsWith"), Meta("is"), Meta("precedes"), 
      Meta("requires"), Meta("relatesTo"), Meta("superOf"), Meta("verifies")),
    Meta("Attribute") superOf (
      Meta("StringAttribute") superOf (
        Meta("Code"), Meta("Comment"), Meta("Deprecated"),  
        Meta("Example"), Meta("Expectation"), Meta("FileName"), Meta("Gist"), 
        Meta("Image"), Meta("Spec"), Meta("Text"), Meta("Title"), Meta("Why")),
      Meta("IntAttribute") superOf (
        Meta("Benefit"), Meta("Capacity"), Meta("Cost"), Meta("Damage"), 
        Meta("Frequency"), Meta("Min"), Meta("Max"),        
        Meta("Order"), Meta("Prio"), Meta("Probability"), Meta("Profit"), Meta("Value")),
      Meta("StatusValueAttribute") superOf (Meta("Status")),   
      Meta("VectorAttribute") superOf (Meta("Constraints"))),      
    Meta("enums") has (
      Meta("StatusValue") has (
        Meta("ELICITED"), Meta("SPECIFIED"), Meta("VALIDATED"), 
        Meta("PLANNED"), Meta("IMPLEMENTED"), Meta("TESTED"), Meta("RELEASED"), Meta("FAILED"), Meta("POSTPONED"), Meta("DROPPED"))),
    Meta("enumDefaults") has (
      Meta("StatusValue") has Meta("ELICITED")))  
  
  val statusUp = Map[StatusValue, StatusValue](
    ELICITED -> SPECIFIED, SPECIFIED -> VALIDATED, VALIDATED -> PLANNED, PLANNED -> IMPLEMENTED, IMPLEMENTED -> TESTED, FAILED -> IMPLEMENTED, POSTPONED -> PLANNED, DROPPED -> ELICITED). withDefaultValue(RELEASED)
  
  val statusDown = Map[StatusValue, StatusValue](
    VALIDATED -> SPECIFIED, PLANNED -> POSTPONED, IMPLEMENTED -> FAILED, TESTED -> FAILED, RELEASED -> FAILED). withDefaultValue(DROPPED)
  
  val (statusStart, statusEnd, statusDead) = (ELICITED, RELEASED, DROPPED)
  
  lazy val entDef = Map[String,String](
    "Actor" -> "A human or machine that communicates with a system." ,
    "App" -> "A computer program, or group of programs designed for end users, normally with a graphical user interface. Short for application." ,
    "Barrier" -> "Something that makes it difficult to achieve a goal or a higher quality level." ,
    "Breakpoint" -> "A point of change. An important aspect of a (non-linear) relation between quality and benefit.",
    "Class" -> "An extensible template for creating objects. A set of objects with certain attributes in common. A category.",
    "Component" -> "A composable part of a system. A reusable, interchangeable system unit or functionality." ,
    "Configuration" -> "A specific combination of variants." ,
    "Data" -> "Information stored in a system." ,
    "Design" -> "A specific realization or high-level implementation description (of a system part).",
    "Domain" -> "The application area of a product with its surrounding entities.",
    "Epic" -> "A large user story or a collection of stories." ,
    "Event" -> "Something that can happen in the domain and/or in the system.",
    "Feature" -> "A releasable characteristic of a product. A (high-level, coherent) bundle of requirements.",
    "Function" -> "A description of how input data is mapped to output data. A capability of a system to do something specific." ,
    "Goal" -> "An intention of a stakeholder or desired system property." ,
    "Idea" -> "A concept or thought (potentially interesting)." ,
    "Interface" -> "A defined way to interact with a system." ,
    "Item" -> " An article in a collection, enumeration, or series.",
    "Issue" -> "Something needed to be fixed." ,
    "Label" -> "A descriptive name used to identify something.",
    "Meta" -> "A prefix used on a concept to mean beyond or about its own concept, e.g. metadata is data about data.",
    "Member" -> "An entity that is part of another entity, eg. a field in a in a class." ,
    "Module" -> "A collection of coherent functions and interfaces.",
    "MockUp" -> "A prototype with limited functionality used to demonstrate a design idea.",
    "Product" -> "Something offered to a market." ,
    "Quality" -> "A distinguishing characteristic or degree of goodness.",
    "Relationship" -> "A specific way that entities are connected." ,
    "Release" -> "A specific version of a system offered at a specific time to end users." ,
    "Req" -> "Something needed or wanted. An abstract term denoting any type of information relevant to the (specification of) intentions behind system development. Short for requirement.",
    "Resource" -> "A capability of, or support for development." ,
    "Risk" -> "Something negative that may happen." ,
    "Scenario" -> "A (vivid) description of a (possible future) system usage." ,
    "Screen" -> "A design of (a part of) a user interface." ,
    "Section" -> "A part of a (requirements) document." ,
    "Service" -> "Actions performed by systems and/or humans to provide results to stakeholders." ,
    "Stakeholder" -> "Someone with a stake in the system development or usage." ,
    "State" -> "A mode or condition of something in the domain and/or in the system. A configuration of data.",
    "Story" -> "A short description of what a user does or needs. Short for user story." ,
    "System" -> "A set of interacting software and/or hardware components." ,
    "Target" -> "A desired quality level or goal ." ,
    "Task" -> "A piece of work (that users do, maybe supported by a system)." ,
    "Term" -> "A word or group of words having a particular meaning.",
    "Test" -> "A procedure to check if requirements are met." ,
    "Ticket" -> "(Development) work awaiting to be completed." ,
    "UseCase" -> "A list of steps defining interactions between actors and a system to achieve a goal." ,
    "User" -> "A human interacting with a system." ,
    "Variant" -> "An object or system property that can be chosen from a set of options." ,
    "VariationPoint" -> "An opportunity of choice among variants.",
    "WorkPackage" -> "A collection of (development) work tasks.")
    
  lazy val attrDef = Map[String,String](
    "Benefit" -> "A characterisation of a good or helpful result or effect (e.g. of a feature)." ,
    "Capacity" -> "The largest amount that can be held or contained (e.g. by a resource)." ,
    "Code" -> "A collection of (textual) computer instructions in some programming language, e.g. Scala. Short for source code.",
    "Comment" -> "A note that explains or discusses some entity." ,
    "Constraints" -> "A collection of propositions that restrict the possible values of a set of variables.",
    "Cost" -> "The expenditure of something, such as time or effort, necessary for the implementation of an entity." ,
    "Damage" -> "A characterisation of the negative consequences if some entity (e.g. a risk) occurs." ,
    "Deprecated" -> "A description of why an entity should be avoided, often because it is superseded by another entity, as indicated by a 'deprecates' relation." ,
    "Example" -> "A note that illustrates some entity by a  typical instance." ,
    "Expectation" -> "The required output of a test in order to be counted as passed." ,
    "FileName" -> "The name of a storage of serialized, persistent data." ,
    "Frequency" -> "The rate of occurrence of some entity. " ,
    "Gist" -> "A short and simple description of an entity, e.g. a function or a test." ,
    "Image" -> "(The name of) a picture of an entity." ,
    "Input" -> "Data consumed by an entity, " ,
    "Max" -> "The maximum estimated or assigned (relative) value." ,
    "Min" -> "The minimum estimated or assigned (relative) value." ,
    "Order" -> "The ordinal number of an entity (1st, 2nd, ...)." ,
    "Output" -> "Data produced by an entity, e.g. a function or a test." ,
    "Prio" -> "The level of importance of an entity. Short for priority." ,
    "Probability" -> "The likelihood that something (e.g. a risk) occurs." ,
    "Profit" -> "The gain or return of some entity, e.g. in monetary terms." ,
    "Spec" -> "A (detailed) definition of an entity. Short for specification" ,
    "Status" -> "A level of refinement of an entity (e.g. a feature) in the development process. ", 
    "Text" -> "A sequence of words (in natural language).", 
    "Title" -> "A general or descriptive heading.", 
    "Value" -> "An amount. An estimate of worth.",
    "Why" -> "A description of intention. Rationale.")
    
  lazy val relDef = Map[String,String](
    "binds" -> "Ties a value to an option. A configuration binds a variation point." ,
    "deprecates" -> "Makes outdated. An entity deprecates (supersedes) another entity." ,
    "excludes" -> "Prevents a combination. An entity excludes another entity." ,
    "has" -> "Expresses containment, substructure. An entity contains another entity." ,
    "helps" -> "Positive influence. A goal helps to fulfil another goal." ,
    "hurts" -> "Negative influence. A goal hinders another goal." ,
    "impacts" -> "Some influence. A new feature impacts an existing component." ,
    "implements" -> "Realisation of. A module implements a feature." ,
    "interactsWith" -> "Communication. A user interacts with an interface." ,
    "is" -> "Sub-typing, specialization, part of another, more general entity." ,
    "precedes" -> "Temporal ordering. A feature precedes (is implemented before) another feature." ,
    "relatesTo" -> "General relation. An entity is related to another entity." ,
    "requires" -> "Requested combination. An entity is required (or wished) by another entity." ,
    "superOf" -> "Super-typing, generalization, includes another, more specific entity." ,
    "verifies" -> "Gives evidence of correctness. A test verifies the implementation of a feature." )
    
  lazy val definitions = (entDef++attrDef++relDef).map{case (k,v) => (k.toLowerCase,v)}.withDefaultValue("???")
  
  def define(any: Any): String = definitions(any.toString.toLowerCase)
  
  lazy val cheatSheetLatex: String = {
    def visitEntity[T](m: Model, level: Int = 0)(visit: (Entity, Int, Boolean) => T):Vector[T] = 
      m.elems.flatMap {
        case Relation(e,l,m2) => Vector(visit(e, level, false)) ++ visitEntity(m2, level + 1)(visit)
        case e: Entity => Vector(visit(e, level, true))
        case _ => Vector()
      }
    def visitor(e: Entity, level: Int, isLeaf: Boolean): String = 
      if (isLeaf) { 
        s"""\\hangindent=1em\\lstinline+${e.id}+ ${define(e.id)}\n""" 
      } else s"""\\${"sub"*level}section*{${e.id}}\n"""
    def sortLowCase(t1: (String, String), t2: (String, String)): Boolean = t1._1.toLowerCase < t2._1.toLowerCase
    val m = reqT.meta.model - Meta("enums") - Meta("enumDefaults")
    val defBody = visitEntity(m)(visitor).mkString("\n")
    val ending = "\\end{multicols*}\n\\end{document}"
    val preamble = s"""%compile with XeLaTeX
      |\\documentclass[9pt,a4paper,oneside]{report}
      |\\usepackage[margin=18mm,landscape]{geometry}
      |\\usepackage{xltxtra,fontspec,xunicode} %requires XeLaTeX
      |  \\setromanfont{Source Sans Pro}
      |  \\setsansfont{Source Sans Pro}
      |  \\setmonofont{DejaVu Sans Mono}
      |\\usepackage{fancyhdr}
      |\\pagestyle{fancy}
      |\\chead{\\url{http://reqT.org/reqT-cheat-sheet.pdf}}
      |\\usepackage{hyperref}
      |\\hypersetup{colorlinks=true, linkcolor=blue, urlcolor=blue}
      |\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
      |\\definecolor{entityColor}{RGB}{0,100,200}
      |\\definecolor{attributeColor}{RGB}{0,100,50}
      |\\definecolor{relationColor}{RGB}{160,0,30}
      |\\usepackage{listings}
      |\\lstdefinestyle{reqT}{
      |  belowcaptionskip=1\\baselineskip,
      |  breaklines=true,
      |  showstringspaces=false,
      |  basicstyle=\\sffamily,
      |  emph={${reqT.metamodel.entityTypes.mkString(",")}},
      |  emphstyle=\\bfseries\\color{entityColor},
      |  emph={[2]${reqT.metamodel.relationTypes.mkString(",")}},
      |  emphstyle={[2]\\bfseries\\color{relationColor}},
      |  emph={[3]${reqT.metamodel.attributeTypes.mkString(",")}},
      |  emphstyle={[3]\\bfseries\\color{attributeColor}},  
      |}
      |\\lstset{style=reqT}
      |\\usepackage{multicol}
      |
      |\\setlength\\parindent{0em}
      |\\usepackage{titlesec}
      |  \\titlespacing{\\section}{0pt}{5pt}{2pt}
      |  \\titlespacing{\\subsection}{0pt}{5pt}{2pt}
      |  \\titlespacing{\\subsubsection}{0pt}{5pt}{2pt}
      |\\begin{document}
      |\\begin{multicols*}{4}
    """.stripMargin 
    preamble + defBody + ending
  }
  
  lazy val toGraphViz: String = {
    val m = model * superOf inverse(superOf, is)
    val body = m.elems .
      collect { case Relation(Meta(id1), is, Model(Meta(id2))) => s"$id1 -> $id2" } .
      mkString(";\n")
    val preamble = """
      // reqT metamodel in dot language for input to GraphViz 
      // compile with dot -T pdf -o filename.pdf filename.dot 
      compound=true;overlap=false;rankdir=BT;clusterrank=local;
      ordering=out;nojustify=true;
      node [fontname=Sans, fontsize=9, shape=record];
      edge [fontname=Sans, fontsize=9, arrowhead = empty];
      { rank = same; Elem;  Model; RelationType; }
      { rank = same; Node_; Relation; }
      { rank = same;  Entity; Attribute;  }

      Node_ [label = "Node"]
      Attribute [label = "{Attribute[T]|val value: T}"]
      Entity [label = "{Entity|val id: String}"]
      Relation [label = "{Relation|val entity: Entity\lval link: RelationType\lval tail: Model\l }"]
      Model [label = "{Model|def toVector: Vector[Elem]}"]

      Node_ -> Elem
      Relation -> Elem
      Attribute -> Node_
      Entity -> Node_
    """
    s"digraph G {\n $preamble\n $body\n}" 
  }
}

package meta {
  class MetaReqT(val model: Model) extends MetaGen {
    import scala.collection.immutable.ListMap

    override val defaultEntities = Vector(Ent, Meta)
    override val defaultAttributes = Vector(Attr, Code)
    override val defaultInterpretedAttributes = Vector(Constraints)
    override val defaultRelations = Vector(has, is, superOf)
    val defRels  = defaultRelations.map(r => Meta(r.toString)).toModel
    val defEnts  = defaultEntities.map(r => Meta(r.toString)).toModel
    
    override val enums = 
      ListMap((model / "enums").tipIds.map(id=> (id, (model / "enums" / id).tipIds)) :_*)

    val attr = ((model/Meta("Attribute").superOf - Meta("StringAttribute").superOf/Meta("Attr")) - Meta("StringAttribute").superOf/Meta("Code")) - Meta("VectorAttribute").superOf/Meta("Constraints")
      
    override val attributes = 
      ListMap(attr.tipIds.map(id => 
        (id.replace("Attribute",""), attr / Meta(id).superOf tipIds)):_*) 
        
    val enumDefaults = model / "enumDefaults"
    override val attributeDefaultValues = 
      ListMap(enumDefaults.tipIds.map(id => (id, (enumDefaults / id).tipIds.head)):_*)
    
    val ents = model/Meta("Entity").superOf
    override val generalEntities = (ents/Meta("General").superOf -- defEnts) tipIds

    override val contextEntities = ents/Meta("Context").superOf tipIds
    
    val reqs = ents/Meta("Requirement").superOf  

    override val requriementEntities = 
      ListMap(reqs.tipIds.map(r => (r,(reqs/Meta(r).superOf).tipIds)):_*)
      
    override val relations = (model / Meta("RelationType").superOf -- defRels)  tipIds
  }
}

