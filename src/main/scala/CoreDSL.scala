/****************************************************************     
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

/** A base trait for the reqT DSL.
*/
trait Base
  
/** A marker trait for parameters to separation operators on class Model.
*/
trait Selector 

trait HasType { 
  def myType: Type 
} 

trait HasValue[T] { def value: T }

trait HasDefault[T] { def default: T }

sealed trait Elem extends Base with HasType with Selector { 
  def toPair: (Key, MapTo)
  def key: Key
  def mapTo: MapTo
  def isNode: Boolean
  def isAttribute: Boolean
  def isEntity: Boolean = !isAttribute
  def isRelation: Boolean = !isNode
}

trait MapTo extends Base with HasType  
sealed trait Node extends MapTo with Elem {
  override def isNode: Boolean = true
}

trait Attribute[T] extends Node with HasValue[T] {
  override def myType: AttributeType[T]
  override def toPair: (AttributeType[T], Attribute[T]) = (myType, this )
  override def key: AttributeType[T] = myType
  override def mapTo: Attribute[T] = this
  override def isAttribute: Boolean = true
}

trait StringAttribute extends Attribute[String]
trait IntAttribute    extends Attribute[Int]

sealed trait Key extends Base with Selector
sealed trait NodeKey extends Key

trait RelationFactory {
  self: Entity =>
  def has(elems: Elem*) = Relation(this, reqT.has, Model(elems:_*))
  def has(submodel: Model) = Relation(this, reqT.has, submodel)
  def requires(elems: Elem*) =  Relation(this, reqT.requires, Model(elems:_*))
  def requires(submodel: Model) =  Relation(this, reqT.requires, submodel)
}

trait HeadFactory {
  self: Entity =>
  def has = Head(this, reqT.has)
  def requires = Head(this, reqT.requires)
}

trait Entity extends Node with NodeKey with HeadFactory with RelationFactory {
  def id: String
  override def toPair: (Entity, Entity) = ( this , this )
  override def key: Entity = this 
  override def mapTo: Entity = this 
  override def myType: EntityType
  override def isAttribute: Boolean = false
}

trait Type extends Selector   
trait AttributeType[T] extends Type with NodeKey with HasDefault[T] { 
  val default: T 
}
trait StringType extends AttributeType[String] { val default = ""}
trait IntType extends AttributeType[Int] { val default = 0} 

trait EntityType extends Type {
  def apply(id: String): Entity
  def apply(): Entity = apply(nextId)
  def apply(i: Int): Entity = apply(i.toString)
}

trait RelationType extends Type

case class Head(head: Entity, link: RelationType) extends Key 

case class Relation(head: Entity, link: RelationType, tail: Model) extends Elem {
  val myType = Relation
  override def toPair: (Head, Model) = (key, tail)
  override def key: Head = Head(head, link)
  override def mapTo: Model = tail
  override lazy val toString = s"$head $link ${tail.toStringBody}"
  override def isNode: Boolean = false
  override def isAttribute: Boolean = false
}
case object Relation extends Type {
  def apply(rk: Head, tail: Model): Relation = new Relation(rk.head, rk.link, tail) 
}  
  







