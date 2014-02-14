/***     
**                  _______        
**                 |__   __|   reqT - a requriements engineering tool  
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

object metamodel extends MetamodelTypes {
  override lazy val types: Vector[MetaType] = entityTypes ++ attributeTypes ++ relationTypes  
  lazy val entityTypes: Vector[EntityType] = generalEntities ++ contextEntities ++ requirementEntities 
  lazy val generalEntities = Vector(Section) 
  lazy val contextEntities = Vector(Stakeholder) 
  lazy val requirementEntities = generalReqs ++ intentionalReqs
  lazy val generalReqs = Vector(Req, Feature)
  lazy val intentionalReqs = Vector(Goal, Wish)
  lazy val attributeTypes: Vector[AttributeType[_]] = stringAttributes ++ intAttributes ++ cardinalityAttributes
  lazy val stringAttributes = Vector(Spec)
  lazy val intAttributes = Vector(Prio)
  lazy val cardinalityAttributes = Vector(Opt)
  lazy val relationTypes: Vector[RelationType] = Vector(has, requires, relatesTo)
}

//Enum traits
trait Cardinality extends Enum[Cardinality] { val myType = Cardinality }
trait CardinalityType extends EnumType[Cardinality] with AttributeType[Cardinality] { 
  val values = Vector(NoOption, Zero, One, ZeroOrOne, OneOrMany, ZeroOrMany)
  val default = NoOption
} 

trait CardinalityAttribute extends Attribute[Cardinality]
case object Cardinality extends CardinalityType
case object NoOption extends Cardinality
case object Zero extends Cardinality
case object One extends Cardinality
case object ZeroOrOne extends Cardinality
case object OneOrMany extends Cardinality
case object ZeroOrMany extends Cardinality

//Concrete attributes
case class Spec(value: String) extends StringAttribute { override val myType = Spec }
case object Spec extends StringType 

case class Prio(value: Int) extends IntAttribute { override val myType = Prio }
case object Prio extends IntType 

case class Opt(value: Cardinality) extends CardinalityAttribute { override val myType = Opt }
case object Opt extends CardinalityType 

//Abstract requirement traits
trait GeneralReq extends Requirement
case object GeneralReq extends AbstractSelector { type AbstractType = GeneralReq } 

trait IntentionalReq extends Requirement
case object IntentionalReq extends AbstractSelector { type AbstractType = IntentionalReq } 

//Concrete entities
case class Section(id: String) extends General { override val myType: EntityType = Section }
case object Section extends EntityType

case class Stakeholder(id: String) extends Context { override val myType: EntityType = Stakeholder }
case object Stakeholder extends EntityType

case class Req(id: String) extends GeneralReq { override val myType: EntityType = Req }
case object Req extends EntityType

case class Feature(id: String) extends GeneralReq { override val myType: EntityType = Feature }
case object Feature extends EntityType

case class Goal(id: String) extends IntentionalReq { override val myType: EntityType = Goal }
case object Goal extends EntityType

case class Wish(id: String) extends IntentionalReq { override val myType: EntityType = Wish }
case object Wish extends EntityType

//Concrete relations
case object requires extends RelationType
case object relatesTo extends RelationType

//factory traits
trait RelationFactory {
  self: Entity =>
  def has(elems: Elem*) = Relation(this, reqT.has, Model(elems:_*))
  def has(submodel: Model) = Relation(this, reqT.has, submodel)
  def requires(elems: Elem*) =  Relation(this, reqT.requires, Model(elems:_*))
  def requires(submodel: Model) =  Relation(this, reqT.requires, submodel)
  def relatesTo(elems: Elem*) =  Relation(this, reqT.relatesTo, Model(elems:_*))
  def relatesTo(submodel: Model) =  Relation(this, reqT.relatesTo, submodel)
}

trait HeadFactory {
  self: Entity =>
  def has = Head(this, reqT.has)
  def requires = Head(this, reqT.requires)
  def relatesTo = Head(this, reqT.relatesTo)
}

trait HeadTypeFactory {
  self: EntityType =>
  def has = HeadType(this, reqT.has)
  def requires =  HeadType(this, reqT.requires)
  def relatesTo =  HeadType(this, reqT.relatesTo)
}

trait ImplicitFactoryObjects extends CanMakeAttr { //mixed in by package object reqT
  implicit object makeSpec extends AttrMaker[Spec] { def apply(s: String): Spec = Spec(s.toString) }
  implicit object makePrio extends AttrMaker[Prio] { def apply(s: String): Prio = Prio(s.toInt) }
  implicit class StringToCardinality(s: String) { def toCardinality = Opt.valueOf(s)}
  implicit object makeOpt extends AttrMaker[Opt] { def apply(s: String): Opt = Opt(s.toCardinality) }
  lazy val attributeFromString = Map[String, String => Attribute[_]](
    "Spec" -> makeAttr[Spec] _ ,
    "Prio" -> makeAttr[Prio] _,
    "Opt" -> makeAttr[Opt] _)
  lazy val entityFromString = Map[String, String => Entity](
    "Req" -> Req.apply _,
    "Feature" -> Feature.apply _)
}
