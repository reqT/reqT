/*     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/

package reqT 

trait Cardinality extends Enum[Cardinality] { val myType = Cardinality }
trait CardinalityType extends EnumType[Cardinality] with AttributeType[Cardinality] { 
  val values = Vector(Zero, One, ZeroOrOne, OneOrMany, ZeroOrMany)
  val default = values(0)
} 
trait CardinalityAttribute extends Attribute { type Value = Cardinality }
case object Cardinality extends CardinalityType
case object Zero extends Cardinality
case object One extends Cardinality
case object ZeroOrOne extends Cardinality
case object OneOrMany extends Cardinality
case object ZeroOrMany extends Cardinality

object all {
  lazy val typeNameIndex: Map[String, Int] = typeNames.zipWithIndex.toMap.withDefaultValue(-1)
  lazy val typeNames: Vector[String] = types map (_.toString)
  lazy val types: Vector[Type] = entityTypes ++ attributeTypes ++ linkTypes  
  lazy val entityTypes: Vector[EntityType] = Vector(Req, Feature)
  lazy val attributeTypes: Vector[AttributeType[_]] = stringAttributes ++ intAttributes
  lazy val linkTypes: Vector[Link] = Vector(has, requires, relatesTo)
  lazy val stringAttributes = Vector(Spec)
  lazy val intAttributes = Vector(Prio)
}

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

case object has extends Link  
case object requires extends Link
case object relatesTo extends Link

trait AttrMaker[T <: Attribute] { def apply(s: String): T }

trait CanMakeAttr {
  def makeAttr[T <: Attribute](value: String)(implicit make: AttrMaker[T]): T = make(value)
}

trait ImplicitFactoryObjects extends CanMakeAttr { //mixed in by package object reqT
  implicit object makeSpec extends AttrMaker[Spec] { def apply(s: String): Spec = Spec(s) }
  lazy val attributeFromString = Map[String, String => Attribute](
    "Spec" -> makeAttr[Spec] _ ,
    "Prio" -> makeAttr[Prio] _,
    "Opt" -> makeAttr[Opt] _)
  lazy val entityFromString = Map[String, String => Entity](
    "Req" -> Req.apply _,
    "Feature" -> Feature.apply _)
}
