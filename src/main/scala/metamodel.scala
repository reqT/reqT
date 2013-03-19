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
/*
in v2.3.0:
INPROGRESS: csp
INPROGRESS: AttrRef integrated with csp
DONE: Added Integer attributes: Order, Cost, Benefit, Capacity, Urgency
DONE: Bugfix: in ++ so that it merges attributes of enitites
DONE: m.ids gives a Vector of strings of ids
DONE: don't warn if updating attribute with same value
DONE: idGen: generates sequence of ids; idGen.next(), idGen.set("start","1"), idGen.reset
DONE: entityToKeyNodeSetPair: Model(Feature("x"), Feature("y"), Feature("z"))
DONE: Depth First Search: m.depthFirstSearch(Set(Feature("x"), Feature("y")))
DONE: Restrict on DFS gives separate with traversed relations: m /--> Feature("x")
DONE: type parameter for External: Feature("hello") has External[Spec]("specfile.txt")
DONE: type trait implicits for External[T] where T can be Spec, Why, Example, Comment
DONE: load attributes from all external files: m.loadExternals
DONE: Model.interpret("Model()") returns Model()
DONE: Model.load("file-with-my-model.scala") returns Model
DONE: ModelVector(m1,m2) 
DONE: ModelVector(m1,m2).merge
DONE: ModelVector(m1,m2).split(m) == m.split(ModelVector(m1,m2))
DONE?? ModelFiles("mf1.scala", "mf2.scala").load ****** ModelBuilder ???
DONE: m1.intersect(m2) == m1 & m2
DONE: m1.diff(m2) == m1 -- m2 == m1 &~ m2
DONE: meta-link in HTML generation, <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
DONE: preserving add order of Model contents by changing underlying implementation to LinkedHashMap 
DONE: m.sorted to sort on keyOrdering
DONE: indexed access of model elements
DONE: pretty printing of model with index numbers of each key: m.pp
DONE: Allowing Submodel[Model] as attributes - is this a good idea??

TODO: ??m.updateIds(id => "prefix."+id) 
TODO: ??ensure that ids are unique
TODO: ??flatten Model to insert its submodels??

TODO: ?? Model(Part("name") has Submodel(Model("XX")))  //Submodel extends Attribute[Model]
TODO: ?? or is Subdomain better than Part??: Model(Subdomain("a1") has Submodel(Model(...))) 
TODO: ?? add Term entity for generating glossaries
TODO: ?? case class Paragraph instead of Text with object § with apply that generates Paragraph
TODO: Clean up code duplication in attribute trait structure
TODO: Break up long lines to follow scala coding conventions a bit better
 
TODO: m.terms // gives all Model words in strings and prefix names
TODO: m.strings //gives all strings in attributes or ids
TODO: m.stringTerms 
TODO: m.prefixTerms

from v2.2
DONE: Escape sequence convertion in toScala
DONE: Newline \n in Attribute values is replaced with <br> in HTML-generation
DONE: Removed semantic check causing long execution times for large owns-structures 
DONE: m up Feature("x") == (m / Feature("x").up ++ (m \ Feature("x"))
DONE: implement new semantic check of owns-relations: m.hasMultiOwners, m.multiOwners
DONE: implement semantic check missing Specs: m.hasMissingSpecs, m.missingSpecs
DONE: Ordering of Status and Level: Status(DROPPED) < STATUS(ELICITED)
DONE: implement check to run all available checks: m.check
DONE: restrict on sets of entities, can be use eg. to get also relations if an entity has a label: m / (m / Label("aa")).sources

TODO?? new relation "exceeds" for greater than on Prio ??

TODO?? add Database as a Data requirement entity ??? or should DataType be the abstract and Data the concrete type

TODO?? NodeSet use SortedSet/TreeSet instead of Set //started... 

TODO?? Model(State("x") has Spec(""), State("x") transition(Event("a")) State("y")

TODO?? restrict on Reqex; match only on id? 

TODO?? restrict on attributes in assigns??
 how should this work ??
var m = Model( 
  Stakeholder("s") assigns(Prio(5)) to Feature("x"),
  Stakeholder("s") assigns(Prio(1)) to Feature("y"),
  Stakeholder("s") assigns(Prio(9)) to Feature("z")
) 
m / Prio
m /+ Prio

*/
package reqt {

  import scala.language.postfixOps
  import scala.collection.immutable.{SortedSet, SortedMap, MapLike}
  import scala.collection.IndexedSeqLike
  import scala.collection.mutable.LinkedHashMap

  trait CanGenerateScala { def toScala: String = toString }  //override with string of code that generates this object
  trait Default[T] { def default : T }
  trait Value[T] { def value: T }
  trait Prefixed { 
    def productPrefix: String //every case class in Scala has this string member with its class name
    def prefix = productPrefix //alternative name to avoid confusion with the case class Product
    def hasEqualPrefix(that: Prefixed): Boolean = this.prefix == that.prefix 
    def <==>(that: Prefixed): Boolean = hasEqualPrefix(that)
  } 

  trait StringValueToScala extends CanGenerateScala with Value[String] with Prefixed {
    override def toScala: String = prefix + "(" + value.toScala + ")"
  }
  trait SubmodelValueToScala extends CanGenerateScala with Value[Model] with Prefixed {
    override def toScala: String = "Submodel" + value.bodyToScala.indentNewline()
  }
  trait ConstrVectorValueToScala extends CanGenerateScala with Value[Vector[Constr[Any]]] with Prefixed {
    override def toScala: String = prefix + "(" + value.map(_.toScala).mkString(", ") + ")"
  }  
  
  trait NodeKind extends Prefixed  
  trait EntityKind extends NodeKind { val value = "" }
  trait AttributeKind[T] extends NodeKind with Value[T] with Default[T] with CanGenerateScala { 
    override def value = default
    override def toScala = prefix + "(" + default + ")"
    def apply(v: T): Attribute[T] 
   //TRIAL*** def make(v: T): Attribute[T] 
  }
  trait EdgeKind extends Prefixed 

  abstract class Element extends CanGenerateScala with Prefixed
  abstract class Concept extends Element 
  abstract class Structure extends Element 
  abstract class Node[T] extends Concept with Value[T] {
    def isAttribute: Boolean = this.isInstanceOf[Attribute[_]]
    def isEntity: Boolean = this.isInstanceOf[Entity]
  }
  abstract class Entity extends Node[String] with StringValueToScala {
    def id: String = value
    def has() = Key(this, reqt.has())
    def has(as:Attribute[_] *) = (Key(this, reqt.has()), NodeSet(as: _*)) 
    def owns() = Key(this, reqt.owns())
    def owns(es:Entity *) = if (!es.contains(this)) (Key(this, reqt.owns()), NodeSet(es: _*)) else {
      warn("Entity can not own itself: " + this)
      (Key(this, reqt.owns()), NodeSet())
    }
    //construct Key
    def requires() = Key(this, reqt.requires())
    def requires(es:Entity *) = (Key(this, reqt.requires()), NodeSet(es: _*))
    def excludes() = Key(this, reqt.excludes())
    def excludes(es:Entity *) = (Key(this, reqt.excludes()), NodeSet(es: _*))
    def releases() = Key(this, reqt.releases())
    def releases(es:Entity *) = (Key(this, reqt.releases()), NodeSet(es: _*))
    def helps() = Key(this, reqt.helps())
    def helps(es:Entity *) = (Key(this, reqt.helps()), NodeSet(es: _*))
    def hurts() = Key(this, reqt.hurts())
    def hurts(es:Entity *) = (Key(this, reqt.hurts()), NodeSet(es: _*))
    def precedes() = Key(this, reqt.precedes())
    def precedes(es:Entity *) = (Key(this, reqt.precedes()), NodeSet(es: _*))
    def inherits() = Key(this, reqt.inherits())
    def inherits(es:Entity *) = (Key(this, reqt.inherits()), NodeSet(es: _*))
    def implements() = Key(this, reqt.implements())
    def implements(es:Entity *) = (Key(this, reqt.implements()), NodeSet(es: _*))
    def verifies() = Key(this, reqt.verifies())
    def verifies(es:Entity *) = (Key(this, reqt.verifies()), NodeSet(es: _*))
    def deprecates() = Key(this, reqt.deprecates())
    def deprecates(es:Entity *) = (Key(this, reqt.deprecates()), NodeSet(es: _*))
    def assigns[T](a:Attribute[T]) = Key(this, reqt.assigns(a))
    //construct AttrRef
    //def ![T](ak: AttributeKind[T]) = AttrRef[T](this, ak) 
    def Gist = AttrRef(this, reqt.Gist)
    def Spec = AttrRef(this, reqt.Spec)
    def Status = AttrRef(this, reqt.Status)
    def Why = AttrRef(this, reqt.Why)
    def Example = AttrRef(this, reqt.Example)
    def Expectation = AttrRef(this, reqt.Expectation)
    def Input = AttrRef(this, reqt.Input)
    def Output = AttrRef(this, reqt.Output)
    def Trigger = AttrRef(this, reqt.Trigger)
    def Precond = AttrRef(this, reqt.Precond)
    def Frequency = AttrRef(this, reqt.Frequency)
    def Critical = AttrRef(this, reqt.Critical)
    def Problem = AttrRef(this, reqt.Problem)
    def Prio = AttrRef(this, reqt.Prio)
    def Order = AttrRef(this, reqt.Order)
    def Cost = AttrRef(this, reqt.Cost)
    def Benefit = AttrRef(this, reqt.Benefit)
    def Capacity = AttrRef(this, reqt.Capacity)
    def Urgency = AttrRef(this, reqt.Urgency)
    def Code = AttrRef(this, reqt.Code)
    def Constraints = AttrRef(this, reqt.Constraints)
    def Label = AttrRef(this, reqt.Label)
    def Comment = AttrRef(this, reqt.Comment)
    def Image = AttrRef(this, reqt.Image)
    def Deprecated = AttrRef(this, reqt.Deprecated)

    def Submodel[T](r: Reference[T]): Reference[T] = r match {
      case AttrRef(e, ak) => SubRef[T](this, AttrRef[T](e,ak)) 
      case SubRef(e, r) => SubRef[T](this, e.Submodel[T](r))
    }
  }  

  sealed abstract class Reference[T] 
  trait ImplicitVar extends CanGenerateScala //marker trait to signal existence of implicit conversion to Var
  case class AttrRef[T](ent: Entity, attrKind: AttributeKind[T]) 
      extends Reference[T] with ImplicitVar with CanGenerateScala {
    def apply(m: Model) = AttrUpdater(m, this)
    def :=(v: T): (Key, NodeSet) =  (ent.has, NodeSet(attrKind(v)))
	override def toScala: String = ent.toScala + "." + attrKind 
  }
  
  case class AttrUpdater[T](m: Model, ar: AttrRef[T]) {
    def :=(v: T): Model =  m.updated(ar, v)
  }
  
  case class SubRef[T](ent: Entity, r: Reference[T]) 
      extends Reference[T] with ImplicitVar with CanGenerateScala { 
   //TODO !!!
    //def apply(m: Model) = SubUpdater(m, this)
    //def :=(v: T): (Key, NodeSet) =  (ent.has, NodeSet(attrKind(v)))
  }
  
  // case class SubUpdater[T](m: Model, ar: AttrRef[T]) {
    // def :=(v: T): Model =  m.updated(ar, v)
  // }
  
  abstract class Context extends Entity 
  case class Product(value: String) extends Context   
  case object Product extends Context with EntityKind    
  case class Release(value: String) extends Context 
  case object Release extends Context with EntityKind  
  case class Stakeholder(value: String) extends Context 
  case object Stakeholder extends Context with EntityKind 
  case class Actor(value: String) extends Context 
  case object Actor extends Context with EntityKind  
  case class Resource(value: String) extends Context 
  case object Resource extends Context with EntityKind  
  case class Subdomain(value: String) extends Context 
  case object Subdomain extends Context with EntityKind  
  
  abstract class Requirement extends Entity 
  case class Req(value: String) extends Requirement
  case object Req extends Requirement with EntityKind  
  case class Idea(value: String) extends Requirement 
  case object Idea extends Requirement with EntityKind 
  case class Goal(value: String) extends Requirement 
  case object Goal extends Requirement with EntityKind 
  case class Feature(value: String) extends Requirement   
  case object Feature extends Requirement with EntityKind   
  case class Function(value: String) extends Requirement   
  case object Function extends Requirement with EntityKind  
  case class Quality(value: String) extends Requirement   
  case object Quality extends Requirement with EntityKind   
  case class Interface(value: String) extends Requirement   
  case object Interface extends Requirement with EntityKind   
  case class Design(value: String) extends Requirement   
  case object Design extends Requirement with EntityKind   
  case class Issue(value: String) extends Requirement   
  case object Issue extends Requirement with EntityKind   
  case class Ticket(value: String) extends Requirement   
  case object Ticket extends Requirement with EntityKind   
  
  abstract class Data extends Requirement
  case class Class(value: String) extends Requirement   
  case object Class extends Requirement with EntityKind 
  case class Member(value: String) extends Requirement   
  case object Member extends Requirement with EntityKind    
  
  abstract class Scenario extends Requirement
  case class UserStory(value: String) extends Scenario  
  case object UserStory extends Scenario with EntityKind  
  case class UseCase(value: String) extends Scenario  
  case object UseCase extends Scenario with EntityKind   
  case class TestCase(value: String) extends Scenario  
  case object TestCase extends Scenario with EntityKind   
  case class Task(value: String) extends Scenario  
  case object Task extends Scenario with EntityKind  
  case class VividScenario(value: String) extends Scenario  
  case object VividScenario extends Scenario with EntityKind  
  
  //************** Attributes **************
  
  abstract class Attribute[T] extends Node[T] with Default[T] 
  
  trait StringAttr extends Attribute[String] with StringValueToScala { val default = "???" }
  trait LevelAttr extends Attribute[Level] { val default = ELICITED }
  trait IntAttr extends Attribute[Int] { val default = 0 }
  trait ConstrSeqAttr extends Attribute[Vector[Constr[Any]]] with ConstrVectorValueToScala { val default = Vector() }
  
  case class Gist(value: String) extends StringAttr
  case object Gist extends StringAttr with AttributeKind[String]  
  
  case class Spec(value: String) extends StringAttr
  case object Spec extends StringAttr with AttributeKind[String] 
  
  trait Level extends Ordered[Level] { 
    def up:Level
    def down:Level
    def compare(that: Level) = levelIndex(this) compare levelIndex(that)
  }
  case object ELICITED    extends Level { val (up,down) = (SPECIFIED, DROPPED) }
  case object SPECIFIED   extends Level { val (up,down) = (VALIDATED, DROPPED) } 
  case object VALIDATED   extends Level { val (up,down) = (PLANNED, SPECIFIED) } 
  case object PLANNED     extends Level { val (up,down) = (IMPLEMENTED, POSTPONED) }
  case object IMPLEMENTED extends Level { val (up,down) = (TESTED, FAILED) }
  case object TESTED      extends Level { val (up,down) = (RELEASED, FAILED) }
  case object RELEASED    extends Level { val (up,down) = (RELEASED, FAILED) }
  case object FAILED      extends Level { val (up,down) = (IMPLEMENTED, DROPPED) }
  case object POSTPONED   extends Level { val (up,down) = (PLANNED, POSTPONED) }
  case object DROPPED     extends Level { val (up,down) = (ELICITED, DROPPED) }
  
  trait CanUpDown extends Value[Level] with Default[Level] { 
    def up = Status(value.up)
    def down = Status(value.down)
    def init = Status(default)
  }
  
  case class Status(value: Level) extends LevelAttr with CanUpDown with Ordered[Status] { 
    def compare(that: Status) = levelIndex(this.value) compare levelIndex(that.value)
  }
  case object Status extends LevelAttr with AttributeKind[Level] with CanUpDown 
  
  case class Why(value: String) extends StringAttr   //TODO *** update all below with StringAttr and IntAttr etc.
  case object Why extends StringAttr with AttributeKind[String] 
  
  case class Example(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED EXAMPLE" }
  case object Example extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED EXAMPLE" }

  case class Expectation(value: String) extends StringAttr
  case object Expectation extends StringAttr with AttributeKind[String]
  
  case class Input(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED INPUT" }
  case object Input extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED INPUT" }
  
  case class Output(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED OUTPUT" }
  case object Output extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED OUTPUT" }
  
  case class Trigger(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED TRIGGER" }
  case object Trigger extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED TRIGGER" }
  
  case class Precond(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED PRECONDITION" }
  case object Precond extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED PRECONDITION" }
  
  case class Frequency(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED FREQUENCY" }
  case object Frequency extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED FREQUENCY" }
  
  case class Critical(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED CRITICAL" }
  case object Critical extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED CRITICAL" }
  
  case class Problem(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED PROBLEM" }
  case object Problem extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED PROBLEM" }
  
  case class Prio(value: Int) extends IntAttr 
  case object Prio extends IntAttr with AttributeKind[Int]  
  
  case class Order(value: Int) extends IntAttr 
  case object Order extends IntAttr with AttributeKind[Int]  

  case class Cost(value: Int) extends IntAttr 
  case object Cost extends IntAttr with AttributeKind[Int]  
  
  case class Benefit(value: Int) extends IntAttr 
  case object Benefit extends IntAttr with AttributeKind[Int]  

  case class Capacity(value: Int) extends IntAttr 
  case object Capacity extends IntAttr with AttributeKind[Int]  
  
  case class Urgency(value: Int) extends IntAttr 
  case object Urgency extends IntAttr with AttributeKind[Int]  

  case class Label(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED LABEL" }
  case object Label extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED LABEL" }

  case class Comment(value: String) extends Attribute[String] with StringValueToScala { val default = "NO COMMENT" }
  case object Comment extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "NO COMMENT" }
  
  case class Image(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED-FILENAME.jpg" }
  case object Image extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED-FILENAME.jpg" }
  
  case class Deprecated(value: String) extends Attribute[String] with StringValueToScala { val default = "UNDEFINED LABEL" }
  case object Deprecated extends Attribute[String] with StringValueToScala with AttributeKind[String] { val default = "UNDEFINED LABEL" }

  case class Submodel(value: Model) extends Attribute[Model] with SubmodelValueToScala { val default = Model() }
  case object Submodel extends Attribute[Model] with AttributeKind[Model] { 
    val default = Model() 
    def apply(kvs: (Key,NodeSet) * ): Submodel = Submodel(Model(kvs: _*))
  }  

  case class Code(value: String) extends StringAttr {
    def run(prefix: String = ""): String = {
      Model.interpreter match {
        case None => Model.interpreterWarning() ; ""
        case Some(i) => 
          val result = Array[String]("")
          i.beQuietDuring(i.bind("result", "Array[String]", result))
          i.quietRun(s"result(0) = {$prefix ; $value}.toString")
          result(0)          
      } 
    }
  }
  case object Code extends StringAttr with AttributeKind[String]   
  
  case class Constraints(value: Vector[Constr[Any]]) extends ConstrSeqAttr {
    def satisfy = value.solve(Satisfy)
    def toModel = (Model() impose this) satisfy
  }
  case object Constraints extends ConstrSeqAttr with AttributeKind[Vector[Constr[Any]]] {
    def apply(cs: Constr[Any] * ): Constraints = Constraints(cs.toVector)
  }
  
  case class External[T <: Attribute[_]](fileName:String)( implicit makeAttr: AttrFromString[T]) 
  extends Attribute[String] with StringValueToScala { 
    val default = "NONAME.scala"
    val value = fileName
    val emptyAttr: T = makeAttr("")
    def fromFile: T = makeAttr(load(fileName))
    override def prefix = emptyAttr.prefix
    override def toScala = "External[" + emptyAttr.prefix + "](\"" + fileName + "\")" 
  }
  case object External extends Attribute[String] with StringValueToScala with AttributeKind[String] { 
    val default = "UNDEFINED EXTERNAL" 
    def apply(s: String) = Spec(default)
  }
  
  trait AttrFromString[T <: Attribute[_]] {
    def apply(s: String): T 
  }
  
  case object NoAttribute extends Attribute[Unit] { val value = (); val default = () }

  //************** Relations **************
  
  abstract class Edge extends Concept 
  abstract class Relation extends Edge {
    def kind: Relation
    def to(es: Entity *): EdgeToNodes = EdgeToNodes(this, NodeSet(es.toSet.asInstanceOf[Set[Node[_]]]))
    def Product(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Product(id)))
    def Release(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Release(id)))
    def Stakeholder(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Stakeholder(id)))
    def Actor(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Actor(id)))
    def Resource(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Resource(id)))
    def Subdomain(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Subdomain(id)))
    def Req(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Req(id)))
    def Idea(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Idea(id)))
    def Goal(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Goal(id)))
    def Feature(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Feature(id)))
    def Function(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Function(id)))
    def Class(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Class(id)))
    def Member(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Member(id)))
    def Quality(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Quality(id)))
    def Interface(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Interface(id)))
    def Design(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Design(id)))
    def Issue(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Issue(id)))
    def Ticket(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Ticket(id)))
    def UserStory(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.UserStory(id)))
    def UseCase(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.UseCase(id)))
    def TestCase(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.TestCase(id)))
    def Task(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.Task(id)))
    def VividScenario(id: String): EdgeToNodes = EdgeToNodes(this, NodeSet(reqt.VividScenario(id)))
  }
  
  abstract class RelationWithoutAttribute extends Relation {
    override def toScala = prefix
  }
  case class owns() extends RelationWithoutAttribute { def kind: Relation = owns } 
  case object owns extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  case class requires() extends RelationWithoutAttribute { def kind: Relation = requires }
  case object requires extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  case class excludes() extends RelationWithoutAttribute { def kind: Relation = excludes }
  case object excludes extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  case class releases() extends RelationWithoutAttribute { def kind: Relation = releases }
  case object releases extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  case class helps() extends RelationWithoutAttribute { def kind: Relation = helps }
  case object helps extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  case class hurts() extends RelationWithoutAttribute { def kind: Relation = hurts }
  case object hurts extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  case class precedes() extends RelationWithoutAttribute { def kind: Relation = precedes }
  case object precedes extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  case class inherits() extends RelationWithoutAttribute { def kind: Relation = inherits }
  case object inherits extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  case class implements() extends RelationWithoutAttribute { def kind: Relation = implements }
  case object implements extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }  
  case class verifies() extends RelationWithoutAttribute { def kind: Relation = verifies }
  case object verifies extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }  
  case class deprecates() extends RelationWithoutAttribute { def kind: Relation = deprecates }
  case object deprecates extends RelationWithoutAttribute with EdgeKind { def kind: Relation = this }
  abstract class RelationWithAttribute[T] extends Relation { 
    def attribute: Attribute[T]
    override def toScala: String = prefix + "(" + attribute.toScala + ")"
  } 
  case class assigns[T](attribute: Attribute[T]) extends RelationWithAttribute[T] { def kind: Relation = assigns }
  case object assigns extends RelationWithAttribute[Unit] with EdgeKind { val attribute = NoAttribute; def kind: Relation = this }
  
  abstract class AttributeEdge extends Edge {
    def Gist(value: String) = EdgeToNodes(has(), NodeSet(reqt.Gist(value)))
    def Spec(value: String) = EdgeToNodes(has(), NodeSet(reqt.Spec(value)))
    def Status(value: Level) = EdgeToNodes(has(), NodeSet(reqt.Status(value)))
    def Why(value: String) = EdgeToNodes(has(), NodeSet(reqt.Why(value)))
    def Example(value: String) = EdgeToNodes(has(), NodeSet(reqt.Example(value)))
    def Expectation(value: String) = EdgeToNodes(has(), NodeSet(reqt.Expectation(value)))
    def Input(value: String) = EdgeToNodes(has(), NodeSet(reqt.Input(value)))
    def Output(value: String) = EdgeToNodes(has(), NodeSet(reqt.Output(value)))
    def Trigger(value: String) = EdgeToNodes(has(), NodeSet(reqt.Trigger(value)))
    def Precond(value: String) = EdgeToNodes(has(), NodeSet(reqt.Precond(value)))
    def Frequency(value: String) = EdgeToNodes(has(), NodeSet(reqt.Frequency(value)))
    def Critical(value: String) = EdgeToNodes(has(), NodeSet(reqt.Critical(value)))
    def Problem(value: String) = EdgeToNodes(has(), NodeSet(reqt.Problem(value)))
    def Prio(value: Int) = EdgeToNodes(has(), NodeSet(reqt.Prio(value)))
    def Order(value: Int) = EdgeToNodes(has(), NodeSet(reqt.Order(value)))
    def Cost(value: Int) = EdgeToNodes(has(), NodeSet(reqt.Cost(value)))
    def Benefit(value: Int) = EdgeToNodes(has(), NodeSet(reqt.Benefit(value)))
    def Capacity(value: Int) = EdgeToNodes(has(), NodeSet(reqt.Capacity(value)))
    def Urgency(value: Int) = EdgeToNodes(has(), NodeSet(reqt.Urgency(value)))
    def Submodel(value: Model) = EdgeToNodes(has(), NodeSet(reqt.Submodel(value)))
    def Code(value: String) = EdgeToNodes(has(), NodeSet(reqt.Code(value)))
    def Constraints[T](value: Vector[Constr[T]]) = EdgeToNodes(has(), NodeSet(reqt.Constraints(value)))
    def Constraints[T](value: Constr[T] *) = EdgeToNodes(has(), NodeSet(reqt.Constraints(value.toVector)))
    def Label(value: String) = EdgeToNodes(has(), NodeSet(reqt.Label(value)))
    def Comment(value: String) = EdgeToNodes(has(), NodeSet(reqt.Comment(value)))
    def Image(value: String) = EdgeToNodes(has(), NodeSet(reqt.Image(value)))
    def Deprecated(value: String) = EdgeToNodes(has(), NodeSet(reqt.Deprecated(value)))
    override def toScala = prefix
 }
  case class has() extends AttributeEdge 
  case object has extends AttributeEdge with EdgeKind {
    def apply(as:Attribute[_] *): EdgeToNodes = EdgeToNodes(has(), NodeSet(as.toSet.asInstanceOf[Set[Node[_]]])) 
  }
  
  case class EdgeToNodes(edge: Edge, nodes: NodeSet) extends Structure //used as argument for addEdgeToNodes
  
  //values below used for abstract concept arguments to the restriction method in Model:
  case object Context extends Context { val value = "" }
  case object Requirement extends Requirement { val value = "" }
  case object Scenario extends Scenario { val value = "" }
  case object Relation extends Relation { def kind: Relation = owns } // TODO check if kind=owns is ok here
  
  //******* Structural elements *******
  
  case class Key(entity: Entity, edge: Edge) extends Structure {
    def to(es: Entity *) = edge match {
      case e: RelationWithAttribute[_] => (this, NodeSet(es: _*))
      case _ => warn("to-construct only allowed for relation with attribute value" + 
        "\nNodeSet.empty generated for " + entity + " " + edge)
        (this, NodeSet.empty)
    }     
    override def toScala = entity.toScala + " " + edge.toScala + " "
  }
  sealed abstract class SetStructure[T <: Node[_]] extends Structure {
    def nodes: Set[T]
    def hasAttribute = nodes.exists(_.isInstanceOf[Attribute[_]])
    def hasEntity = nodes.exists(_.isInstanceOf[Entity])
    override def toScala = {
      val skipNl = (hasEntity) || nodes.map(_.toScala).size < 68
      val nl = if (skipNl) "" else "\n  "
      val nl2 = if (skipNl) "" else "\n    "
      val (leftPar, rightPar) = nodes.size match {
        case 0 => ("(",")")
        case 1 => ("", "") 
        case _ => ("(" + nl2, nl +")")
      }
      nodes.map(_.toScala).mkString(leftPar, ", " + nl2, rightPar)
    }
    override def toString = prefix + nodes.map(_.toString).mkString("(", ", ", ")")
  }
  case class NodeSet(nodes: Set[Node[_]]) extends SetStructure[Node[_]] {  
    assert(!(hasAttribute && hasEntity), 
      "Both Entity and Attribute nodes in the same NodeSet is not allowed. This is a bug. Please report.")
    def keyStr(keyOpt: Option[Key] = None) = (keyOpt collect { case k => " of " + k.entity } orElse (Some("")) get)
    def removeDuplicatePrefixes(keyOpt: Option[Key] = None) = {
      def removeDup(l:List[Node[_]]):List[Node[_]] = l match {
        case Nil => Nil
        case x::Nil => x::Nil
        case x::xs => 
          if (xs.exists(_.hasEqualPrefix(x))) 
            { warn("Duplicate" + keyStr(keyOpt) + " overwritten: "+x); removeDup(xs) } 
          else x::removeDup(xs) 
      }
      NodeSet(removeDup(nodes.toList).toSet)
    }
    def concatNodes(ns: NodeSet, keyOpt: Option[Key] = None)  = {
      if (!ns.hasAttribute) NodeSet(nodes ++ ns.nodes)
      else { //remove duplicates and replace existing attributes
        val moreNodes = ns.removeDuplicatePrefixes(keyOpt) 
        val existingAttrNodesRemoved = nodes filterNot { n =>
          if (n.isAttribute) moreNodes.nodes.exists { n2 => 
            if (n2.hasEqualPrefix(n) && (n2.value != n.value))
              { warn("Overwriting attribute " + n + " with " + n2 + keyStr(keyOpt))
              true }
            else false
          }
          else false
        }
        NodeSet(existingAttrNodesRemoved ++ moreNodes.nodes)
      } 
    }
  }
  object NodeSet {
    def apply(ns:Node[_]*): NodeSet = empty.concatNodes(NodeSet(ns.toSet))
    def empty: NodeSet = new NodeSet(SortedSet.empty[Node[_]](reqt.nodeOrdering)) 
      //???why must nodeOrdering above be explicitly passed (or else compile error)??? 
      //(implicit lookup does not seem to work... why??)
  }

  //**** ModelVector and ModelFiles for splitting and merging models ***
  
  final class ModelVector( val models: IndexedSeq[Model]) 
      extends IndexedSeq[Model] with IndexedSeqLike[Model, ModelVector] {
    import scala.collection.mutable.Builder
    override protected[this] def newBuilder: Builder[Model, ModelVector] = ModelVector.newBuilder 
    lazy val length: Int = models.length

    def apply(idx: Int): Model = models(idx)

    // override def toString = if (isEmpty) "ModelVector()" 
      // else "ModelVector(\n" + models.map(
        // m => "Model(... " + 
          // m.size + " keys; " + 
          // m.entities.size + " entities, " + 
          // m.relations.size + " relations ...)"
      // ).mkString(" ",",\n ","\n") + ")"
      
    lazy val merge: Model = reduce(_ ++ _)
    lazy val overlap: Model = reduce(_ & _)
    def split(pivot: Model): ModelVector = {
      val keySeq = models map (_.keySet)
      val splitSeq = keySeq map (pivot.restrictKeys(_))
      val allMeregdKeys = merge.keySet
      val notInSplitSeq = pivot.excludeKeys(allMeregdKeys)
      if (notInSplitSeq.isEmpty) ModelVector.fromSeq(splitSeq) 
      else {
        warn("ModelVector with extra model of overflow keys appended!")
        ModelVector.fromSeq(splitSeq) ++ ModelVector(notInSplitSeq)
      }
    }
    lazy val checkEmpty = if (models.exists { _ == Model() } ) { 
      warn("Empty Model in ModelVector."); false 
    } else true
    lazy val checkOverlap = if (!overlap.isEmpty) { 
      warn("Overlapping keys in ModelVector. Try method overlap on ModelVector."); false 
    } else true
    def check: Boolean = (models.map(_.check) ++ Seq(checkEmpty, checkOverlap)) reduce(_ & _)       
    def save(mf: ModelFiles) = mf save this   
  }

  object ModelVector {
    import scala.collection.mutable.{Builder, ArrayBuffer}
    import scala.collection.generic.CanBuildFrom
    def fromSeq(models: Seq[Model]): ModelVector = new ModelVector(models.toIndexedSeq)
    def apply(models: Model*) = new ModelVector(models.toIndexedSeq)
    def newBuilder: Builder[Model, ModelVector] = new ArrayBuffer mapResult fromSeq
    implicit def canBuildFrom: CanBuildFrom[ModelVector, Model, ModelVector] = new CanBuildFrom[ModelVector, Model, ModelVector] {
        def apply(): Builder[Model, ModelVector] = newBuilder
        def apply(from: ModelVector): Builder[Model, ModelVector] = newBuilder
      }
  }
  
  final class ModelFiles(fileNames: IndexedSeq[String]) extends IndexedSeq[String] with IndexedSeqLike[String, ModelFiles] {
    import scala.collection.mutable.Builder
    override protected[this] def newBuilder: Builder[String, ModelFiles] = ModelFiles.newBuilder 
    lazy val length: Int = fileNames.length

    def apply(idx: Int): String = fileNames(idx)
    def +(s: String): ModelFiles = this :+ s
    def load = ModelVector(fileNames map (s => Model.load(s)):_*)
    def save(mv: ModelVector) {
      if (mv.size != fileNames.size) 
        warn("Size of ModelVector == " + mv.size + " is not same as size of ModelFiles == " + fileNames.size)
      val n = mv.size min fileNames.size
      for (i <- 0 until n) mv(i).toScala.save(fileNames(i))
    }
    override def toString = "ModelFiles(" + fileNames.map(_.toScala).mkString(", ") + ")"
  }
  object ModelFiles {
    import scala.collection.mutable.{Builder, ArrayBuffer}
    import scala.collection.generic.CanBuildFrom
    def fromSeq(models: Seq[String]): ModelFiles = new ModelFiles(models.toIndexedSeq)
    def newBuilder: Builder[String, ModelFiles] = new ArrayBuffer mapResult fromSeq
    implicit def canBuildFrom: CanBuildFrom[ModelFiles, String, ModelFiles] = new CanBuildFrom[ModelFiles, String, ModelFiles] {
        def apply(): Builder[String, ModelFiles] = newBuilder
        def apply(from: ModelFiles): Builder[String, ModelFiles] = newBuilder
      }  
    def apply(names: String*): ModelFiles = new ModelFiles(names.toIndexedSeq)
  }
  
  
  //***************************************
  
  case class RichString(s: String) {
    def toScala: String = "" + '\"' + convertEscape + '\"'
    def toModel: Model = Model.interpret(s)
    def decapitalize: String = strUtil.decapitalize(s)
    def indentNewline(n: Int = 2) = strUtil.indentNewline(s, n)
    def filterEscape: String = strUtil.filterEscapeChar(s)
    def convertEscape: String = strUtil.escape(s)
    def save(fileName:String) = saveString(s, fileName) 
  }
  
  object strUtil { //utilities for strings
    def decapitalize(s:String) = s.take(1).toLowerCase + s.drop(1)
    def indentNewline(s: String, n: Int) = s.replace("\n","\n"+ (" " * n))
    def quoteIfString(a:Any):String = a match {
      case s:String => "\"" + s + "\""
      case _ => a.toString
    }
    def escapeSeq(s:String):String = (for (c <- s) yield c match {
      case '\b' => '\\'+"b"
      case '\t' => '\\'+"t"
      case '\n' => '\\'+"n"
      case '\f' => '\\'+"f"
      case '\r' => '\\'+"r"
      case '\"' => ""+'\\'+'\"'
      case '\'' => ""+'\\'+ """'"""
      case '\\' => ""+'\\'+'\\'	
      case _ => c.toString
    }).mkString("")
    def charToUnicodeSeq(c:Char):String = if (c >= ' ') c.toString else {
      val h = Integer.toHexString(c)
      val zeroes = ( for (i <- 1 to (4-h.length)) yield "0").mkString("")
      "\\u" + zeroes + h
    }
    def unicodeSeq(s:String):String = 
      (for (c <- s) yield charToUnicodeSeq(c)).mkString("")
    def escape(s:String):String = unicodeSeq(escapeSeq(s)) 
    def filterEscapeChar(s:String) = s.toList.filterNot(_ < ' ').mkString
    //def lineBreaks(s:String):String =  
    //	if (!s.contains("//")) s else 
    //		"(" + s.replaceAll("//","\" +\n      \"//") + "\n    )"
    def valueToString(v:Any):String = v match {
      case s:String =>  "\"" + escape(s) + "\""    //lineBreaks(escape(s)) //removed as collide with latex
      case _ => v.toString
    }
    def valueToRawString(v:Any) :String = v match {
      case s:String =>  "\"\"\"" + s + "\"\"\""
      case _ => v.toString
    }
    def scalaSuffix(s:String):String = if (!s.contains(".")) s + ".scala" else s
    def latexSuffix(s:String):String = if (!s.contains(".")) s + ".tex" else s
    def txtSuffix(s:String):String = if (!s.contains(".")) s + ".txt" else s
    def varPrefix(s:String):String = if (s.contains(".")) "" else  "var " + s + " = "
    
  } // end strUtil  
  
  object warn {
    private var warnMe = true
    private var savedState = List[Boolean]()
    def on() = {warnMe = true } ; def off() = {warnMe = false }
    def isOn = warnMe
    def save() {savedState = warnMe :: savedState} // to enable local change of warn state
    def restore() {warnMe = savedState.headOption.getOrElse(warnMe); savedState = savedState.drop(1)}
    def apply(w:String) = if (warnMe) println("--- Warning: " + w)
  }

  object idGen {
    var prefix = "id"
    var n = 1
    def next() = { val res = prefix + n ; n += 1 ; res }
    def set(newPrefix: String = prefix, newN: Int = n) { n = newN; prefix = newPrefix } 
    def set(newN: Int) { n = newN } 
    def reset() { set("id",1) }
  }   
  
} //end package reqt


