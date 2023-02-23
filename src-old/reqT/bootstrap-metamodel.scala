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

// This is the bootstrap metamodel to be used when a fresh metamodel is needed
//1. copy this file to the name "GENERATED-metamodel.scala" (replacing the old if any)
//2. uncomment the code below
//3. compile reqT
//4. generate new metamodel using reqT on the command line the -m parameter:
//    $> scala -toolcp reqT.jar reqT.Main -m
//5. copy the generated file into src/main/scala/.

/* <- uncomment here and last line
package reqT

object metamodel extends MetamodelTypes {
  override lazy val types: Vector[MetaType] = entityTypes ++ attributeTypes ++ relationTypes
  lazy val entityTypes: Vector[EntityType] = Vector(Ent) ++ generalEntities ++ contextEntities ++ requirementEntities
  
  lazy val generalEntities: Vector[EntityType] = Vector(Item, Label, Section) 
  lazy val contextEntities: Vector[EntityType] = Vector(Actor, Product, Release, Resource, Stakeholder, Subdomain, System)   
  lazy val requirementEntities: Vector[EntityType] = generalReqs ++ intentionalReqs
  lazy val generalReqs: Vector[EntityType] = Vector(Req, Idea, Feature)
  lazy val intentionalReqs: Vector[EntityType] = Vector(Goal, Wish)
  lazy val functionalReqs: Vector[EntityType] = Vector(Function, Interface, Design)
  lazy val qualityReqs: Vector[EntityType] = Vector(Quality, Target, Barrier)
  lazy val scenarioReqs: Vector[EntityType] = Vector(Scenario, Task, TestCase, UserStory, UseCase)

  lazy val attributeTypes: Vector[AttributeType[_]] = Vector(Attr, Code) ++ interpretedAttributes ++ stringAttributes ++ intAttributes ++ cardinalityAttributes
  lazy val stringAttributes: Vector[StringType] = Vector(Text, Title, Spec, Gist, Why, Example, Input, Output, Expectation)
  lazy val intAttributes: Vector[IntType] = Vector(Prio, Cost)
  lazy val cardinalityAttributes: Vector[CardinalityType] = Vector(Opt) 
  lazy val relationTypes: Vector[RelationType] = Vector(has, is) ++ Vector(requires, relatesTo)
  lazy val interpretedAttributes: Vector[AttributeType[_]] = Vector(Constraints)
}

//Enum traits

trait Cardinality extends Enum[Cardinality] { val enumCompanion = Cardinality }
trait CardinalityCompanion extends EnumCompanion[Cardinality] { 
  val values = Vector(NoOption, Zero, One, ZeroOrOne, OneOrMany, ZeroOrMany)
  val default = NoOption
}
trait CardinalityAttribute extends Attribute[Cardinality]
trait CardinalityType extends AttributeType[Cardinality] {
  val default = Cardinality.default
  override  def apply(value: Cardinality): CardinalityAttribute
}
case object Cardinality extends CardinalityCompanion
case object NoOption extends Cardinality
case object Zero extends Cardinality
case object One extends Cardinality
case object ZeroOrOne extends Cardinality
case object OneOrMany extends Cardinality
case object ZeroOrMany extends Cardinality
   
//Concrete attributes
case class Text(value: String) extends StringAttribute { override val myType = Text }
case object Text extends StringType 

case class Title(value: String) extends StringAttribute { override val myType = Title }
case object Title extends StringType 

case class Spec(value: String) extends StringAttribute { override val myType = Spec }
case object Spec extends StringType 

case class Gist(value: String) extends StringAttribute { override val myType = Gist }
case object Gist extends StringType 

case class Why(value: String) extends StringAttribute { override val myType = Why }
case object Why extends StringType 

case class Example(value: String) extends StringAttribute { override val myType = Example }
case object Example extends StringType 

case class Input(value: String) extends StringAttribute { override val myType = Input }
case object Input extends StringType 

case class Output(value: String) extends StringAttribute { override val myType = Output }
case object Output extends StringType 

case class Expectation(value: String) extends StringAttribute { override val myType = Expectation }
case object Expectation extends StringType 

case class Prio(value: Int) extends IntAttribute { override val myType = Prio }
case object Prio extends IntType 

case class Cost(value: Int) extends IntAttribute { override val myType = Cost }
case object Cost extends IntType 

case class Opt(value: Cardinality) extends CardinalityAttribute { override val myType = Opt }
case object Opt extends CardinalityType 

//Abstract requirement traits
trait GeneralReq extends Requirement
case object GeneralReq extends AbstractSelector { type AbstractType = GeneralReq } 

trait IntentionalReq extends Requirement
case object IntentionalReq extends AbstractSelector { type AbstractType = IntentionalReq } 

trait FunctionalReq extends Requirement
case object FunctionalReq extends AbstractSelector { type AbstractType = FunctionalReq } 

trait QualityReq extends Requirement
case object QualityReq extends AbstractSelector { type AbstractType = QualityReq } 

trait ScenarioReq extends Requirement
case object ScenarioReq extends AbstractSelector { type AbstractType = ScenarioReq } 

//Concrete entities
case class Item(id: String) extends General { override val myType: EntityType = Item }
case object Item extends EntityType

case class Label(id: String) extends General { override val myType: EntityType = Label }
case object Label extends EntityType

case class Section(id: String) extends General { override val myType: EntityType = Section }
case object Section extends EntityType

case class Actor(id: String) extends Context { override val myType: EntityType = Actor }
case object Actor extends EntityType

case class Product(id: String) extends Context { override val myType: EntityType = Product }
case object Product extends EntityType

case class Release(id: String) extends Context { override val myType: EntityType = Release }
case object Release extends EntityType

case class Resource(id: String) extends Context { override val myType: EntityType = Resource }
case object Resource extends EntityType

case class Stakeholder(id: String) extends Context { override val myType: EntityType = Stakeholder }
case object Stakeholder extends EntityType

case class Subdomain(id: String) extends Context { override val myType: EntityType = Subdomain }
case object Subdomain extends EntityType

case class System(id: String) extends Context { override val myType: EntityType = System }
case object System extends EntityType

case class Req(id: String) extends GeneralReq { override val myType: EntityType = Req }
case object Req extends EntityType

case class Idea(id: String) extends GeneralReq { override val myType: EntityType = Idea }
case object Idea extends EntityType

case class Feature(id: String) extends GeneralReq { override val myType: EntityType = Feature }
case object Feature extends EntityType

case class Goal(id: String) extends IntentionalReq { override val myType: EntityType = Goal }
case object Goal extends EntityType

case class Wish(id: String) extends IntentionalReq { override val myType: EntityType = Wish }
case object Wish extends EntityType

case class Function(id: String) extends FunctionalReq { override val myType: EntityType = Function }
case object Function extends EntityType

case class Interface(id: String) extends FunctionalReq { override val myType: EntityType = Interface }
case object Interface extends EntityType

case class Design(id: String) extends FunctionalReq { override val myType: EntityType = Design }
case object Design extends EntityType

case class Quality(id: String) extends QualityReq { override val myType: EntityType = Quality }
case object Quality extends EntityType

case class Target(id: String) extends QualityReq { override val myType: EntityType = Target }
case object Target extends EntityType

case class Barrier(id: String) extends QualityReq { override val myType: EntityType = Barrier }
case object Barrier extends EntityType

case class Scenario(id: String) extends ScenarioReq { override val myType: EntityType = Scenario }
case object Scenario extends EntityType

case class Task(id: String) extends ScenarioReq { override val myType: EntityType = Task }
case object Task extends EntityType

case class TestCase(id: String) extends ScenarioReq { override val myType: EntityType = TestCase }
case object TestCase extends EntityType

case class UserStory(id: String) extends ScenarioReq { override val myType: EntityType = UserStory }
case object UserStory extends EntityType

case class UseCase(id: String) extends ScenarioReq { override val myType: EntityType = UseCase }
case object UseCase extends EntityType

//Concrete relations
case object requires extends RelationType
case object relatesTo extends RelationType

//Factory traits
trait RelationFactory {
  self: Entity =>
  def requires(elems: Elem*) = Relation(this, reqT.requires, Model(elems:_*))
  def requires(submodel: Model) = Relation(this, reqT.requires, submodel)
  def relatesTo(elems: Elem*) = Relation(this, reqT.relatesTo, Model(elems:_*))
  def relatesTo(submodel: Model) = Relation(this, reqT.relatesTo, submodel)
}

trait HeadFactory {
  self: Entity =>
  def requires = Head(this, reqT.requires)
  def relatesTo = Head(this, reqT.relatesTo)
}

trait HeadTypeFactory {
  self: EntityType =>
  def requires = HeadType(this, reqT.requires)
  def relatesTo = HeadType(this, reqT.relatesTo)
}

trait ImplicitFactoryObjects extends CanMakeAttr { //mixed in by package object reqT
  implicit object makeAttr extends AttrMaker[Attr] { def apply(s: String): Attr = Attr(s.toString) }
  implicit object makeCode extends AttrMaker[Code] { def apply(s: String): Code = Code(s.toString) }  
  implicit object makeConstraints extends AttrMaker[Constraints] { def apply(s: String): Constraints = Constraints(s.toString) }  

  implicit object makeText extends AttrMaker[Text] { def apply(s: String): Text = Text(s.toString) }
  implicit object makeTitle extends AttrMaker[Title] { def apply(s: String): Title = Title(s.toString) }
  implicit object makeSpec extends AttrMaker[Spec] { def apply(s: String): Spec = Spec(s.toString) }
  implicit object makeGist extends AttrMaker[Gist] { def apply(s: String): Gist = Gist(s.toString) }
  implicit object makeWhy extends AttrMaker[Why] { def apply(s: String): Why = Why(s.toString) }
  implicit object makeExample extends AttrMaker[Example] { def apply(s: String): Example = Example(s.toString) }
  implicit object makeInput extends AttrMaker[Input] { def apply(s: String): Input = Input(s.toString) }
  implicit object makeOutput extends AttrMaker[Output] { def apply(s: String): Output = Output(s.toString) }
  implicit object makeExpectation extends AttrMaker[Expectation] { def apply(s: String): Expectation = Expectation(s.toString) }
  implicit object makePrio extends AttrMaker[Prio] { def apply(s: String): Prio = Prio(s.toInt) }
  implicit object makeCost extends AttrMaker[Cost] { def apply(s: String): Cost = Cost(s.toInt) }
  implicit object makeOpt extends AttrMaker[Opt] { def apply(s: String): Opt = Opt(s.toCardinality) }


  implicit class StringToCardinality(s: String) { def toCardinality = Cardinality.valueOf(s)}

  lazy val attributeFromString = Map[String, String => Attribute[_]](
    "Attr" -> makeAttribute[Attr] _ ,
    "Code" -> makeAttribute[Code] _ ,
    "Constraints" -> makeAttribute[Constraints] _ ,
    "Text" -> makeAttribute[Text] _ ,
    "Title" -> makeAttribute[Title] _ ,
    "Spec" -> makeAttribute[Spec] _ ,
    "Gist" -> makeAttribute[Gist] _ ,
    "Why" -> makeAttribute[Why] _ ,
    "Example" -> makeAttribute[Example] _ ,
    "Input" -> makeAttribute[Input] _ ,
    "Output" -> makeAttribute[Output] _ ,
    "Expectation" -> makeAttribute[Expectation] _ ,
    "Prio" -> makeAttribute[Prio] _ ,
    "Cost" -> makeAttribute[Cost] _ ,
    "Opt" -> makeAttribute[Opt] _  
  )
  lazy val entityFromString = Map[String, String => Entity](
    "Ent" -> Ent.apply _ ,
    "Item" -> Item.apply _ ,
    "Label" -> Label.apply _ ,
    "Section" -> Section.apply _ ,
    "Actor" -> Actor.apply _ ,
    "Product" -> Product.apply _ ,
    "Release" -> Release.apply _ ,
    "Resource" -> Resource.apply _ ,
    "Stakeholder" -> Stakeholder.apply _ ,
    "Subdomain" -> Subdomain.apply _ ,
    "System" -> System.apply _ ,
    "Req" -> Req.apply _ ,
    "Idea" -> Idea.apply _ ,
    "Feature" -> Feature.apply _ ,
    "Goal" -> Goal.apply _ ,
    "Wish" -> Wish.apply _ ,
    "Function" -> Function.apply _ ,
    "Interface" -> Interface.apply _ ,
    "Design" -> Design.apply _ ,
    "Quality" -> Quality.apply _ ,
    "Target" -> Target.apply _ ,
    "Barrier" -> Barrier.apply _ ,
    "Scenario" -> Scenario.apply _ ,
    "Task" -> Task.apply _ ,
    "TestCase" -> TestCase.apply _ ,
    "UserStory" -> UserStory.apply _ ,
    "UseCase" -> UseCase.apply _ 
  )
}

*/ //<-- REMOVE THIS LINE