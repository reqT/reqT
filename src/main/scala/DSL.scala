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

/** The base trait for the reqT DSL (requirements Domain Specific Language). */
trait DSL { 
  /** Concrete DSL classes should have an executable string representation.
      If the default toString is not executable scala-embedded DSL syntax,
      then this method is overriden by an executable string.
  */
  def toScala: String = toString 
}
  
/** A marker trait for parameters to separation operators on Model. */
sealed trait Selector 

/** A mixin trait for runtime typing through case object values of type Type. */
trait HasType { def myType: Type } 

/** A mixin trait for generic values. */
trait HasValue[T] { def value: T }

/** A mixin trait for generic default values. */
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

case object NoElem extends Elem with Type { 
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
}

sealed trait Type extends Selector   
sealed trait Key extends DSL with Selector

trait Model extends MapTo 
  with ModelImplementation 
  with ModelEquality
  with ModelAccess

object Model extends Type 
  with ModelCompanion 
  with ModelFromMap


trait AttributeType[T] extends Key with Type with HasDefault[T] { 
  val default: T 
}
sealed trait Entity extends Node with HeadFactory with RelationFactory {
  def id: String
  override def key: Head = Head( this , reqT.has) 
  override def mapTo: Model = Model() 
  override def myType: EntityType
  override def isAttribute: Boolean = false
  def /(h: Head) = HeadPath(this.has, h)
  def /(e: Entity) = HeadPath(this.has, e.has)
  def /[T](at: AttributeType[T]) = AttrRef[T](HeadPath(this.has), at)
  def /[T](a: Attribute[T]) = AttrVal[T](HeadPath(this.has), a)
}
case object NoEntity extends Entity {
  override val id = ""
  override val myType = NoEntityType
}

trait Context extends Entity
trait General extends Entity
trait Requirement extends Entity

trait EntityType extends Type with HeadTypeFactory {
  def apply(id: String): Entity
  def apply(): Entity = apply(nextId)
  def apply(i: Int): Entity = apply(i.toString)
}
case object NoEntityType extends EntityType {
  override def apply(id: String): Entity = NoEntity
}

trait AbstractSelector extends Selector {
  type AbstractType <: Elem
}

case object Context extends AbstractSelector { type AbstractType = Context } 
case object General extends AbstractSelector { type AbstractType = General } 
case object Requirement extends AbstractSelector { type AbstractType = Requirement } 

trait RelationType extends Type
case object NoLink extends RelationType

case class Head(entity: Entity, link: RelationType) extends Key {
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
case object Relation extends Type {
  def apply(h: Head, tail: Model): Relation = new Relation(h.entity, h.link, tail) 
}  

case class HeadType(entityType: EntityType, link: RelationType) extends Type    

trait AttrMaker[T <: Attribute[_]] { def apply(s: String): T }

trait CanMakeAttr {
  def makeAttr[T <: Attribute[_]](value: String)( implicit make: AttrMaker[T]): T = make(value)
}

trait MetamodelTypes {
  def types: Vector[Type]
  lazy val names: Vector[String] = types map (_.toString)
  lazy val indexOf: Map[String, Int] = names.zipWithIndex.toMap.withDefaultValue(-1)
}





