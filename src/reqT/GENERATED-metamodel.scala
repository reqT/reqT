// *** THIS IS GENERATED SOURCE  
// *** Generated by reqT> reqT.meta.gen()
// *** Generation date: Sun Sep 07 17:14:40 CEST 2014 
// *** reqT version 3.0.0-snapshot build date Sun Sep 07 17:14:40 CEST 2014
/*** 
**                  _______        
**                 |__   __|       
**   _ __  ___   __ _ | |      reqT - a requriements engineering tool
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |      (c) 2011-2014, Lund University
**  |_|   \___| \__  ||_|      Free Open Software BSD-2-clause licence
**                 | |      
**                 |_|      
***************************************************/
package reqT

object metamodel extends MetamodelTypes {
  override lazy val types: Vector[TypeObject] = entityTypes ++ attributeTypes ++ relationTypes
  lazy val entityTypes: Vector[EntityType] = Vector(Ent, Meta) ++ generalEntities ++ contextEntities ++ requirementEntities
  
  lazy val generalEntities: Vector[EntityType] = Vector(Item, Label, Section, Term) 
  lazy val contextEntities: Vector[EntityType] = Vector(Actor, App, Component, Domain, Module, Product, Release, Resource, Risk, Service, Stakeholder, System, User)   
  lazy val requirementEntities: Vector[EntityType] = dataReqs ++ designReqs ++ functionalReqs ++ generalReqs ++ qualityReqs ++ scenarioReqs ++ variabilityReqs
  lazy val dataReqs: Vector[EntityType] = Vector(Class, Data, Input, Member, Output, Relationship)
  lazy val designReqs: Vector[EntityType] = Vector(Design, Screen, MockUp)
  lazy val functionalReqs: Vector[EntityType] = Vector(Function, Interface)
  lazy val generalReqs: Vector[EntityType] = Vector(Epic, Feature, Goal, Idea, Issue, Req, Ticket, WorkPackage)
  lazy val qualityReqs: Vector[EntityType] = Vector(Breakpoint, Barrier, Quality, Target)
  lazy val scenarioReqs: Vector[EntityType] = Vector(Scenario, Task, Test, Story, UseCase)
  lazy val variabilityReqs: Vector[EntityType] = Vector(VariationPoint, Variant)

  lazy val attributeTypes: Vector[AttributeType[_]] = Vector(Attr, Code) ++ interpretedAttributes ++ stringAttributes ++ intAttributes ++ statusvalueAttributes
  lazy val stringAttributes: Vector[StringType] = Vector(Comment, Deprecated, Example, Expectation, FileName, Gist, Image, Spec, Text, Title, Why)
  lazy val intAttributes: Vector[IntType] = Vector(Benefit, Capacity, Cost, Damage, Frequency, Min, Max, Order, Prio, Probability, Profit, Value)
  lazy val statusvalueAttributes: Vector[StatusValueType] = Vector(Status) 
  lazy val relationTypes: Vector[RelationType] = Vector(has, is, superOf) ++ Vector(binds, deprecates, excludes, helps, hurts, impacts, implements, interactsWith, precedes, requires, relatesTo, verifies)
  lazy val interpretedAttributes: Vector[AttributeType[_]] = Vector(Constraints)
}

//Enum traits

trait StatusValue extends Enum[StatusValue] { val enumCompanion = StatusValue }
trait StatusValueCompanion extends EnumCompanion[StatusValue] { 
  val values = Vector(ELICITED, SPECIFIED, VALIDATED, PLANNED, IMPLEMENTED, TESTED, RELEASED, FAILED, POSTPONED, DROPPED)
  val default = ELICITED
}
trait StatusValueAttribute extends Attribute[StatusValue]
trait StatusValueType extends AttributeType[StatusValue] {
  val default = StatusValue.default
  override  def apply(value: StatusValue): StatusValueAttribute
}
case object StatusValue extends StatusValueCompanion
case object ELICITED extends StatusValue
case object SPECIFIED extends StatusValue
case object VALIDATED extends StatusValue
case object PLANNED extends StatusValue
case object IMPLEMENTED extends StatusValue
case object TESTED extends StatusValue
case object RELEASED extends StatusValue
case object FAILED extends StatusValue
case object POSTPONED extends StatusValue
case object DROPPED extends StatusValue
   
//Concrete attributes
case class Comment(value: String) extends StringAttribute { override val myType = Comment }
case object Comment extends StringType 

case class Deprecated(value: String) extends StringAttribute { override val myType = Deprecated }
case object Deprecated extends StringType 

case class Example(value: String) extends StringAttribute { override val myType = Example }
case object Example extends StringType 

case class Expectation(value: String) extends StringAttribute { override val myType = Expectation }
case object Expectation extends StringType 

case class FileName(value: String) extends StringAttribute { override val myType = FileName }
case object FileName extends StringType 

case class Gist(value: String) extends StringAttribute { override val myType = Gist }
case object Gist extends StringType 

case class Image(value: String) extends StringAttribute { override val myType = Image }
case object Image extends StringType 

case class Spec(value: String) extends StringAttribute { override val myType = Spec }
case object Spec extends StringType 

case class Text(value: String) extends StringAttribute { override val myType = Text }
case object Text extends StringType 

case class Title(value: String) extends StringAttribute { override val myType = Title }
case object Title extends StringType 

case class Why(value: String) extends StringAttribute { override val myType = Why }
case object Why extends StringType 

case class Benefit(value: Int) extends IntAttribute { override val myType = Benefit }
case object Benefit extends IntType 

case class Capacity(value: Int) extends IntAttribute { override val myType = Capacity }
case object Capacity extends IntType 

case class Cost(value: Int) extends IntAttribute { override val myType = Cost }
case object Cost extends IntType 

case class Damage(value: Int) extends IntAttribute { override val myType = Damage }
case object Damage extends IntType 

case class Frequency(value: Int) extends IntAttribute { override val myType = Frequency }
case object Frequency extends IntType 

case class Min(value: Int) extends IntAttribute { override val myType = Min }
case object Min extends IntType 

case class Max(value: Int) extends IntAttribute { override val myType = Max }
case object Max extends IntType 

case class Order(value: Int) extends IntAttribute { override val myType = Order }
case object Order extends IntType 

case class Prio(value: Int) extends IntAttribute { override val myType = Prio }
case object Prio extends IntType 

case class Probability(value: Int) extends IntAttribute { override val myType = Probability }
case object Probability extends IntType 

case class Profit(value: Int) extends IntAttribute { override val myType = Profit }
case object Profit extends IntType 

case class Value(value: Int) extends IntAttribute { override val myType = Value }
case object Value extends IntType 

case class Status(value: StatusValue) extends StatusValueAttribute { override val myType = Status }
case object Status extends StatusValueType 

//Abstract requirement traits
trait DataReq extends Requirement
case object DataReq extends AbstractSelector { type AbstractType = DataReq } 

trait DesignReq extends Requirement
case object DesignReq extends AbstractSelector { type AbstractType = DesignReq } 

trait FunctionalReq extends Requirement
case object FunctionalReq extends AbstractSelector { type AbstractType = FunctionalReq } 

trait GeneralReq extends Requirement
case object GeneralReq extends AbstractSelector { type AbstractType = GeneralReq } 

trait QualityReq extends Requirement
case object QualityReq extends AbstractSelector { type AbstractType = QualityReq } 

trait ScenarioReq extends Requirement
case object ScenarioReq extends AbstractSelector { type AbstractType = ScenarioReq } 

trait VariabilityReq extends Requirement
case object VariabilityReq extends AbstractSelector { type AbstractType = VariabilityReq } 

//Concrete entities
case class Item(id: String) extends General { override val myType: EntityType = Item }
case object Item extends EntityType

case class Label(id: String) extends General { override val myType: EntityType = Label }
case object Label extends EntityType

case class Section(id: String) extends General { override val myType: EntityType = Section }
case object Section extends EntityType

case class Term(id: String) extends General { override val myType: EntityType = Term }
case object Term extends EntityType

case class Actor(id: String) extends Context { override val myType: EntityType = Actor }
case object Actor extends EntityType

case class App(id: String) extends Context { override val myType: EntityType = App }
case object App extends EntityType

case class Component(id: String) extends Context { override val myType: EntityType = Component }
case object Component extends EntityType

case class Domain(id: String) extends Context { override val myType: EntityType = Domain }
case object Domain extends EntityType

case class Module(id: String) extends Context { override val myType: EntityType = Module }
case object Module extends EntityType

case class Product(id: String) extends Context { override val myType: EntityType = Product }
case object Product extends EntityType

case class Release(id: String) extends Context { override val myType: EntityType = Release }
case object Release extends EntityType

case class Resource(id: String) extends Context { override val myType: EntityType = Resource }
case object Resource extends EntityType

case class Risk(id: String) extends Context { override val myType: EntityType = Risk }
case object Risk extends EntityType

case class Service(id: String) extends Context { override val myType: EntityType = Service }
case object Service extends EntityType

case class Stakeholder(id: String) extends Context { override val myType: EntityType = Stakeholder }
case object Stakeholder extends EntityType

case class System(id: String) extends Context { override val myType: EntityType = System }
case object System extends EntityType

case class User(id: String) extends Context { override val myType: EntityType = User }
case object User extends EntityType

case class Class(id: String) extends DataReq { override val myType: EntityType = Class }
case object Class extends EntityType

case class Data(id: String) extends DataReq { override val myType: EntityType = Data }
case object Data extends EntityType

case class Input(id: String) extends DataReq { override val myType: EntityType = Input }
case object Input extends EntityType

case class Member(id: String) extends DataReq { override val myType: EntityType = Member }
case object Member extends EntityType

case class Output(id: String) extends DataReq { override val myType: EntityType = Output }
case object Output extends EntityType

case class Relationship(id: String) extends DataReq { override val myType: EntityType = Relationship }
case object Relationship extends EntityType

case class Design(id: String) extends DesignReq { override val myType: EntityType = Design }
case object Design extends EntityType

case class Screen(id: String) extends DesignReq { override val myType: EntityType = Screen }
case object Screen extends EntityType

case class MockUp(id: String) extends DesignReq { override val myType: EntityType = MockUp }
case object MockUp extends EntityType

case class Function(id: String) extends FunctionalReq { override val myType: EntityType = Function }
case object Function extends EntityType

case class Interface(id: String) extends FunctionalReq { override val myType: EntityType = Interface }
case object Interface extends EntityType

case class Epic(id: String) extends GeneralReq { override val myType: EntityType = Epic }
case object Epic extends EntityType

case class Feature(id: String) extends GeneralReq { override val myType: EntityType = Feature }
case object Feature extends EntityType

case class Goal(id: String) extends GeneralReq { override val myType: EntityType = Goal }
case object Goal extends EntityType

case class Idea(id: String) extends GeneralReq { override val myType: EntityType = Idea }
case object Idea extends EntityType

case class Issue(id: String) extends GeneralReq { override val myType: EntityType = Issue }
case object Issue extends EntityType

case class Req(id: String) extends GeneralReq { override val myType: EntityType = Req }
case object Req extends EntityType

case class Ticket(id: String) extends GeneralReq { override val myType: EntityType = Ticket }
case object Ticket extends EntityType

case class WorkPackage(id: String) extends GeneralReq { override val myType: EntityType = WorkPackage }
case object WorkPackage extends EntityType

case class Breakpoint(id: String) extends QualityReq { override val myType: EntityType = Breakpoint }
case object Breakpoint extends EntityType

case class Barrier(id: String) extends QualityReq { override val myType: EntityType = Barrier }
case object Barrier extends EntityType

case class Quality(id: String) extends QualityReq { override val myType: EntityType = Quality }
case object Quality extends EntityType

case class Target(id: String) extends QualityReq { override val myType: EntityType = Target }
case object Target extends EntityType

case class Scenario(id: String) extends ScenarioReq { override val myType: EntityType = Scenario }
case object Scenario extends EntityType

case class Task(id: String) extends ScenarioReq { override val myType: EntityType = Task }
case object Task extends EntityType

case class Test(id: String) extends ScenarioReq { override val myType: EntityType = Test }
case object Test extends EntityType

case class Story(id: String) extends ScenarioReq { override val myType: EntityType = Story }
case object Story extends EntityType

case class UseCase(id: String) extends ScenarioReq { override val myType: EntityType = UseCase }
case object UseCase extends EntityType

case class VariationPoint(id: String) extends VariabilityReq { override val myType: EntityType = VariationPoint }
case object VariationPoint extends EntityType

case class Variant(id: String) extends VariabilityReq { override val myType: EntityType = Variant }
case object Variant extends EntityType

//Concrete relations
case object binds extends RelationType
case object deprecates extends RelationType
case object excludes extends RelationType
case object helps extends RelationType
case object hurts extends RelationType
case object impacts extends RelationType
case object implements extends RelationType
case object interactsWith extends RelationType
case object precedes extends RelationType
case object requires extends RelationType
case object relatesTo extends RelationType
case object verifies extends RelationType

//Factory traits
trait RelationFactory {
  self: Entity =>
  def binds(elems: Elem*) = Relation(this, reqT.binds, Model(elems:_*))
  def binds(submodel: Model) = Relation(this, reqT.binds, submodel)
  def deprecates(elems: Elem*) = Relation(this, reqT.deprecates, Model(elems:_*))
  def deprecates(submodel: Model) = Relation(this, reqT.deprecates, submodel)
  def excludes(elems: Elem*) = Relation(this, reqT.excludes, Model(elems:_*))
  def excludes(submodel: Model) = Relation(this, reqT.excludes, submodel)
  def helps(elems: Elem*) = Relation(this, reqT.helps, Model(elems:_*))
  def helps(submodel: Model) = Relation(this, reqT.helps, submodel)
  def hurts(elems: Elem*) = Relation(this, reqT.hurts, Model(elems:_*))
  def hurts(submodel: Model) = Relation(this, reqT.hurts, submodel)
  def impacts(elems: Elem*) = Relation(this, reqT.impacts, Model(elems:_*))
  def impacts(submodel: Model) = Relation(this, reqT.impacts, submodel)
  def implements(elems: Elem*) = Relation(this, reqT.implements, Model(elems:_*))
  def implements(submodel: Model) = Relation(this, reqT.implements, submodel)
  def interactsWith(elems: Elem*) = Relation(this, reqT.interactsWith, Model(elems:_*))
  def interactsWith(submodel: Model) = Relation(this, reqT.interactsWith, submodel)
  def precedes(elems: Elem*) = Relation(this, reqT.precedes, Model(elems:_*))
  def precedes(submodel: Model) = Relation(this, reqT.precedes, submodel)
  def requires(elems: Elem*) = Relation(this, reqT.requires, Model(elems:_*))
  def requires(submodel: Model) = Relation(this, reqT.requires, submodel)
  def relatesTo(elems: Elem*) = Relation(this, reqT.relatesTo, Model(elems:_*))
  def relatesTo(submodel: Model) = Relation(this, reqT.relatesTo, submodel)
  def verifies(elems: Elem*) = Relation(this, reqT.verifies, Model(elems:_*))
  def verifies(submodel: Model) = Relation(this, reqT.verifies, submodel)
}

trait HeadFactory {
  self: Entity =>
  def binds = Head(this, reqT.binds)
  def deprecates = Head(this, reqT.deprecates)
  def excludes = Head(this, reqT.excludes)
  def helps = Head(this, reqT.helps)
  def hurts = Head(this, reqT.hurts)
  def impacts = Head(this, reqT.impacts)
  def implements = Head(this, reqT.implements)
  def interactsWith = Head(this, reqT.interactsWith)
  def precedes = Head(this, reqT.precedes)
  def requires = Head(this, reqT.requires)
  def relatesTo = Head(this, reqT.relatesTo)
  def verifies = Head(this, reqT.verifies)
}

trait HeadTypeFactory {
  self: EntityType =>
  def binds = HeadType(this, reqT.binds)
  def deprecates = HeadType(this, reqT.deprecates)
  def excludes = HeadType(this, reqT.excludes)
  def helps = HeadType(this, reqT.helps)
  def hurts = HeadType(this, reqT.hurts)
  def impacts = HeadType(this, reqT.impacts)
  def implements = HeadType(this, reqT.implements)
  def interactsWith = HeadType(this, reqT.interactsWith)
  def precedes = HeadType(this, reqT.precedes)
  def requires = HeadType(this, reqT.requires)
  def relatesTo = HeadType(this, reqT.relatesTo)
  def verifies = HeadType(this, reqT.verifies)
}

trait ImplicitFactoryObjects extends CanMakeAttr { //mixed in by package object reqT
  implicit object makeAttr extends AttrMaker[Attr] { def apply(s: String): Attr = Attr(s.toString) }
  implicit object makeCode extends AttrMaker[Code] { def apply(s: String): Code = Code(s.toString) }  
  implicit object makeConstraints extends AttrMaker[Constraints] { def apply(s: String): Constraints = Constraints(s.toString) }  

  implicit object makeComment extends AttrMaker[Comment] { def apply(s: String): Comment = Comment(s.toString) }
  implicit object makeDeprecated extends AttrMaker[Deprecated] { def apply(s: String): Deprecated = Deprecated(s.toString) }
  implicit object makeExample extends AttrMaker[Example] { def apply(s: String): Example = Example(s.toString) }
  implicit object makeExpectation extends AttrMaker[Expectation] { def apply(s: String): Expectation = Expectation(s.toString) }
  implicit object makeFileName extends AttrMaker[FileName] { def apply(s: String): FileName = FileName(s.toString) }
  implicit object makeGist extends AttrMaker[Gist] { def apply(s: String): Gist = Gist(s.toString) }
  implicit object makeImage extends AttrMaker[Image] { def apply(s: String): Image = Image(s.toString) }
  implicit object makeSpec extends AttrMaker[Spec] { def apply(s: String): Spec = Spec(s.toString) }
  implicit object makeText extends AttrMaker[Text] { def apply(s: String): Text = Text(s.toString) }
  implicit object makeTitle extends AttrMaker[Title] { def apply(s: String): Title = Title(s.toString) }
  implicit object makeWhy extends AttrMaker[Why] { def apply(s: String): Why = Why(s.toString) }
  implicit object makeBenefit extends AttrMaker[Benefit] { def apply(s: String): Benefit = Benefit(s.toInt) }
  implicit object makeCapacity extends AttrMaker[Capacity] { def apply(s: String): Capacity = Capacity(s.toInt) }
  implicit object makeCost extends AttrMaker[Cost] { def apply(s: String): Cost = Cost(s.toInt) }
  implicit object makeDamage extends AttrMaker[Damage] { def apply(s: String): Damage = Damage(s.toInt) }
  implicit object makeFrequency extends AttrMaker[Frequency] { def apply(s: String): Frequency = Frequency(s.toInt) }
  implicit object makeMin extends AttrMaker[Min] { def apply(s: String): Min = Min(s.toInt) }
  implicit object makeMax extends AttrMaker[Max] { def apply(s: String): Max = Max(s.toInt) }
  implicit object makeOrder extends AttrMaker[Order] { def apply(s: String): Order = Order(s.toInt) }
  implicit object makePrio extends AttrMaker[Prio] { def apply(s: String): Prio = Prio(s.toInt) }
  implicit object makeProbability extends AttrMaker[Probability] { def apply(s: String): Probability = Probability(s.toInt) }
  implicit object makeProfit extends AttrMaker[Profit] { def apply(s: String): Profit = Profit(s.toInt) }
  implicit object makeValue extends AttrMaker[Value] { def apply(s: String): Value = Value(s.toInt) }
  implicit object makeStatus extends AttrMaker[Status] { def apply(s: String): Status = Status(s.toStatusValue) }


  implicit class StringToStatusValue(s: String) { def toStatusValue = StatusValue.valueOf(s)}

  lazy val attributeFromString = Map[String, String => Attribute[_]](
    "Attr" -> makeAttribute[Attr] _ ,
    "Code" -> makeAttribute[Code] _ ,
    "Constraints" -> makeAttribute[Constraints] _ ,
    "Comment" -> makeAttribute[Comment] _ ,
    "Deprecated" -> makeAttribute[Deprecated] _ ,
    "Example" -> makeAttribute[Example] _ ,
    "Expectation" -> makeAttribute[Expectation] _ ,
    "FileName" -> makeAttribute[FileName] _ ,
    "Gist" -> makeAttribute[Gist] _ ,
    "Image" -> makeAttribute[Image] _ ,
    "Spec" -> makeAttribute[Spec] _ ,
    "Text" -> makeAttribute[Text] _ ,
    "Title" -> makeAttribute[Title] _ ,
    "Why" -> makeAttribute[Why] _ ,
    "Benefit" -> makeAttribute[Benefit] _ ,
    "Capacity" -> makeAttribute[Capacity] _ ,
    "Cost" -> makeAttribute[Cost] _ ,
    "Damage" -> makeAttribute[Damage] _ ,
    "Frequency" -> makeAttribute[Frequency] _ ,
    "Min" -> makeAttribute[Min] _ ,
    "Max" -> makeAttribute[Max] _ ,
    "Order" -> makeAttribute[Order] _ ,
    "Prio" -> makeAttribute[Prio] _ ,
    "Probability" -> makeAttribute[Probability] _ ,
    "Profit" -> makeAttribute[Profit] _ ,
    "Value" -> makeAttribute[Value] _ ,
    "Status" -> makeAttribute[Status] _  
  )
  lazy val entityFromString = Map[String, String => Entity](
    "Ent" -> Ent.apply _ ,
    "Meta" -> Meta.apply _ ,
    "Item" -> Item.apply _ ,
    "Label" -> Label.apply _ ,
    "Section" -> Section.apply _ ,
    "Term" -> Term.apply _ ,
    "Actor" -> Actor.apply _ ,
    "App" -> App.apply _ ,
    "Component" -> Component.apply _ ,
    "Domain" -> Domain.apply _ ,
    "Module" -> Module.apply _ ,
    "Product" -> Product.apply _ ,
    "Release" -> Release.apply _ ,
    "Resource" -> Resource.apply _ ,
    "Risk" -> Risk.apply _ ,
    "Service" -> Service.apply _ ,
    "Stakeholder" -> Stakeholder.apply _ ,
    "System" -> System.apply _ ,
    "User" -> User.apply _ ,
    "Class" -> Class.apply _ ,
    "Data" -> Data.apply _ ,
    "Input" -> Input.apply _ ,
    "Member" -> Member.apply _ ,
    "Output" -> Output.apply _ ,
    "Relationship" -> Relationship.apply _ ,
    "Design" -> Design.apply _ ,
    "Screen" -> Screen.apply _ ,
    "MockUp" -> MockUp.apply _ ,
    "Function" -> Function.apply _ ,
    "Interface" -> Interface.apply _ ,
    "Epic" -> Epic.apply _ ,
    "Feature" -> Feature.apply _ ,
    "Goal" -> Goal.apply _ ,
    "Idea" -> Idea.apply _ ,
    "Issue" -> Issue.apply _ ,
    "Req" -> Req.apply _ ,
    "Ticket" -> Ticket.apply _ ,
    "WorkPackage" -> WorkPackage.apply _ ,
    "Breakpoint" -> Breakpoint.apply _ ,
    "Barrier" -> Barrier.apply _ ,
    "Quality" -> Quality.apply _ ,
    "Target" -> Target.apply _ ,
    "Scenario" -> Scenario.apply _ ,
    "Task" -> Task.apply _ ,
    "Test" -> Test.apply _ ,
    "Story" -> Story.apply _ ,
    "UseCase" -> UseCase.apply _ ,
    "VariationPoint" -> VariationPoint.apply _ ,
    "Variant" -> Variant.apply _ 
  )
}
