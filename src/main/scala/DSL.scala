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

import scala.language.implicitConversions

/** The base trait for the reqT DSL (a requirement Tool Domain Specific Language). */

trait DSL { 
  /** Concrete DSL classes should have an executable string representation.
      If the default toString is not executable scala-embedded DSL syntax,
      then this method is overriden by an executable string.
  */
  def toScala: String = toString 
}
  
/** A marker trait for parameters to separation operators on Model. */
sealed trait Selector 

/** A marker trait for types of runtime typing using case objects. */
sealed trait MetaType extends Selector   

/** A mixin trait for runtime typing through case object values of type MetaType. */
trait HasType { def myType: MetaType } 

/** A mixin trait for generic attribute values. */
trait HasValue[T] { def value: T }

/** A mixin trait for generic default attribute values. */
trait HasDefault[T] { def default: T }

/** A mixin trait for types that can be converted to a Map key-value-pair. */
trait CanBeMapped {
  def toMapping: (Key, MapTo)  = (key, mapTo)
  def key: Key
  def mapTo: MapTo
}

sealed trait Elem extends DSL with HasType with CanBeMapped with Selector { 
  def isNode: Boolean
  def isAttribute: Boolean
  def isEntity: Boolean = !isAttribute
  def isRelation: Boolean = !isNode
}

case object NoElem extends Elem with MetaType { 
  override val isNode: Boolean = false
  override val isAttribute: Boolean = false
  override val isEntity: Boolean = false
  override val isRelation: Boolean = false
  override val myType = this
  override def key: Head = Head(NoEntity, NoLink)
  override def mapTo: Model  = Model.empty
  def apply(id: String) = NoElem
}

sealed trait Node extends Elem {
  override def isNode: Boolean = true
}

sealed trait MapTo extends DSL with HasType  

trait Attribute[T] extends Node with MapTo with HasValue[T] with CanBeMapped {
  override def myType: AttributeType[T]
  override def key: AttributeType[T] = myType
  override def mapTo: Attribute[T] = this
  override def isAttribute: Boolean = true
  def / = AttrVal(HeadPath(), this)
}

sealed trait Key extends DSL with Selector

trait Model extends MapTo with ModelImplementation 
  
object Model extends MetaType 
  with ModelCompanion 
  with ModelFromMap


trait AttributeType[T] extends Key with MetaType with HasDefault[T] { 
  def apply(value: T): Attribute[T] 
  val default: T 
  def / = AttrRef(HeadPath(), this)
}
sealed trait Entity extends Node with HeadFactory with RelationFactory  {
  def id: String
  override def key: Head = Head( this , reqT.has) 
  override def mapTo: Model = Model() 
  override def myType: EntityType
  override def isAttribute: Boolean = false
  def / = HeadPath(this.has)
  def /(h: Head) = HeadPath(this.has, h)
  def /(e: Entity) = HeadPath(this.has, e.has)
  def /[T](at: AttributeType[T]) = AttrRef[T](HeadPath(this.has), at)
  def /[T](a: Attribute[T]) = AttrVal[T](HeadPath(this.has), a)
  def has(elems: Elem*) = Relation(this, reqT.has, Model(elems:_*))
  def has(submodel: Model) = Relation(this, reqT.has, submodel)
  def has = Head(this, reqT.has)
  def is(elems: Elem*) = Relation(this, reqT.is, Model(elems:_*))
  def is(submodel: Model) = Relation(this, reqT.is, submodel)
  def is = Head(this, reqT.is)
}
case object NoEntity extends Entity {
  override val id = ""
  override val myType = NoEntityType
}

trait Context extends Entity
trait General extends Entity
trait Requirement extends Entity

trait EntityType extends MetaType with HeadTypeFactory  {
  def apply(id: String): Entity
  def apply(): Entity = apply(nextId)
  def apply(i: Int): Entity = apply(i.toString)
  def has = HeadType(this, reqT.has)
  def is =  HeadType(this, reqT.is)
}
case object NoEntityType extends EntityType {
  override def apply(id: String): Entity = NoEntity
}

trait AbstractSelector extends Selector {
  type AbstractType <: Elem
}

case class StringSelector(s: String) extends Selector

trait ImplicitStringSelector { //to be mixed in by package object reqT
  implicit def stringToStringSelector(s: String): StringSelector = StringSelector(s)
}

case object Context extends AbstractSelector { type AbstractType = Context } 
case object General extends AbstractSelector { type AbstractType = General } 
case object Requirement extends AbstractSelector { type AbstractType = Requirement } 

trait RelationType extends MetaType
case object NoLink extends RelationType

case class Head(entity: Entity, link: RelationType) extends Key {
  def / = HeadPath(this)
  def /(h: Head) = HeadPath(this, h)
  def /(e: Entity) = HeadPath(this, e.has)
  def /[T](at: AttributeType[T]) = AttrRef[T](HeadPath(this), at)
  def /[T](a: Attribute[T]) = AttrVal[T](HeadPath(this), a)
  override def toString = entity + "." + link
}

case class Relation(entity: Entity, link: RelationType, tail: Model) extends Elem {
  val myType = Relation
  override val key: Head = Head(entity, link)
  val head: Head = key
  override def mapTo: Model = tail
  override lazy val toString = s"$entity $link ${tail.toStringBody}"
  override def isNode: Boolean = false
  override def isAttribute: Boolean = false
}
case object Relation extends MetaType {
  def apply(h: Head, tail: Model): Relation = new Relation(h.entity, h.link, tail) 
}  

case class HeadType(entityType: EntityType, link: RelationType) extends MetaType    

trait AttrMaker[T <: Attribute[_]] { def apply(s: String): T }

trait CanMakeAttr {
  def makeAttribute[T <: Attribute[_]](value: String)( implicit make: AttrMaker[T]): T = make(value)
}

trait MetamodelTypes {
  def types: Vector[MetaType]
  lazy val names: Vector[String] = types map (_.toString)
  lazy val indexOf: Map[String, Int] = names.zipWithIndex.toMap.withDefaultValue(-1)
}

//Primitive Attributes traits
trait StringAttribute extends Attribute[String] 
trait StringType extends AttributeType[String] { 
  val default = "???"
  override  def apply(value: String): StringAttribute
}

trait IntAttribute extends Attribute[Int] 
trait IntType extends AttributeType[Int] { 
  val default = -999999999
  override  def apply(value: Int): IntAttribute
} 

trait Enum[T <: Ordered[T]] extends Ordered[T] {
  self : T =>
  val enumCompanion: EnumCompanion[T]
  import enumCompanion._
  def compare(that: T): Int = indexOf(this).compare(indexOf(that))
  def toInt: Int = indexOf(this)
}

trait EnumCompanion[T <: Ordered[T]]  {
  val values: Vector[T]
  lazy val names: Vector[String] = values.map(_.toString)
  lazy val valueOf: Map[String, T] = names.zip(values).toMap
  lazy val indexOf: Map[T, Int] = values.zipWithIndex.toMap 
  implicit val ordering = Ordering.fromLessThan[T](_ < _)
}

//Primitive metamodel classes
case class Ent(id: String) extends Entity { override val myType: EntityType = Ent }
case object Ent extends EntityType

case class Spec(value: String) extends StringAttribute { override val myType = Spec }
case object Spec extends StringType 

case class Code(value: String) extends StringAttribute { override val myType = Code }
case object Code extends StringType 

case object has extends RelationType  
case object is extends RelationType  

//Special metamodel attribute implicits
trait ImplicitAttributeEnrichments {
  implicit class CodeRunnable(code: Code) {
    def run: String = repl.interpretString(s"""
{
  ${code.value}
}.toString""").getOrElse("")
    def isTrue: Boolean = repl.interpretBoolean(code.value) == Some(true)
    def interpretBoolean: Option[Boolean] = repl.interpretBoolean(code.value)
    def interpretString: Option[String] = repl.interpretString(code.value)
    def interpretInt: Option[Int] = repl.interpretInt(code.value)
    def interpretModel: Option[Model] = repl.interpretModel(code.value)
  }
}
