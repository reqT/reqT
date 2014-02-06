/***     
**                  _______        
**                 |__   __|   reqT - a free requriements engineering tool  
**   _ __  ___   __ _ | |      (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
***************************************************************************/
package reqT 

object metamodel {
  lazy val indexOf: Map[String, Int] = names.zipWithIndex.toMap.withDefaultValue(-1)
  lazy val names: Vector[String] = types map (_.toString)
  lazy val types: Vector[Type] = entityTypes ++ attributeTypes ++ relationTypes  
  lazy val entityTypes: Vector[EntityType] = Vector(Req, Feature)
  lazy val attributeTypes: Vector[AttributeType[_]] = stringAttributes ++ intAttributes ++ cardinalityAttributes
  lazy val relationTypes: Vector[RelationType] = Vector(has, requires, relatesTo)
  lazy val stringAttributes = Vector(Spec)
  lazy val intAttributes = Vector(Prio)
  lazy val cardinalityAttributes = Vector(Opt)
}

trait StringAttribute extends Attribute[String]
trait StringType extends AttributeType[String] { val default = "???"}

trait IntAttribute    extends Attribute[Int]
trait IntType extends AttributeType[Int] { val default = -999999999} 

trait Cardinality extends Enum[Cardinality] { val myType = Cardinality }
trait CardinalityType extends EnumType[Cardinality] with AttributeType[Cardinality] { 
  val values = Vector(NoOption, Zero, One, ZeroOrOne, OneOrMany, ZeroOrMany)
  val default = values(0)
} 
trait CardinalityAttribute extends Attribute[Cardinality]
case object Cardinality extends CardinalityType
case object NoOption extends Cardinality
case object Zero extends Cardinality
case object One extends Cardinality
case object ZeroOrOne extends Cardinality
case object OneOrMany extends Cardinality
case object ZeroOrMany extends Cardinality

case class Spec(value: String) extends StringAttribute { override val myType = Spec }
case object Spec extends StringType 

case class Prio(value: Int) extends IntAttribute { override val myType = Prio }
case object Prio extends IntType 

case class Opt(value: Cardinality) extends CardinalityAttribute { override val myType = Opt }
case object Opt extends CardinalityType 

case class Req(id: String) extends Entity { override val myType: EntityType = Req }
case object Req extends EntityType

case class Feature(id: String) extends Entity { override val myType: EntityType = Feature }
case object Feature extends EntityType

case object has extends RelationType  
case object requires extends RelationType
case object relatesTo extends RelationType

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

trait AttrMaker[T <: Attribute[_]] { def apply(s: String): T }

trait CanMakeAttr {
  def makeAttr[T <: Attribute[_]](value: String)( implicit make: AttrMaker[T]): T = make(value)
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
