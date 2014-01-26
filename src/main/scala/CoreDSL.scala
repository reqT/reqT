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
trait CoreDSL
  
/** A marker trait for parameters to separation operators on class Model.
*/
trait Selector 

trait Typed extends CoreDSL with Selector { 
  def myType: Type 
} 

trait HasValue {
  type Value
  def value: Value
}

sealed trait Elem extends Typed { 
  def key: Key
  def node: Node
}

trait Node extends Typed  
trait Leaf extends Node with Elem

trait Attribute extends Leaf with HasValue {
  override def myType: AttributeType[Value]
  override def key: AttributeType[Value] = myType
  override val node: Attribute = this
}

trait StringAttribute extends Attribute { type Value = String }
trait IntAttribute    extends Attribute { type Value = Int }
trait ChoiceAttribute extends Attribute { type Value = Choice }

sealed trait Key extends Selector
sealed trait LeafKey extends Key

trait RelationFactory {
  self: Entity =>
  def has(nodes: Elem*) = Relation(Head( this , reqT.has), Model(nodes:_*))
  def requires(nodes: Elem*) =  Relation(Head( this , reqT.requires), Model(nodes:_*))
}

trait HeadFactory {
  self: Entity =>
  def has = Head(this, reqT.has)
  def requires = Head(this, reqT.requires)
}

trait Entity extends Leaf with LeafKey with HeadFactory with RelationFactory {
  def id: String
  override def key: Entity = this
  override def node: Entity = this
  override def myType: EntityType
}

trait Type extends Selector   
trait AttributeType[T] extends Type with LeafKey with HasValue { 
  type Value = T 
  def default = value
}
trait StringType extends AttributeType[String] { def value = ""}
trait IntType extends AttributeType[Int] { def value = 0} 
trait ChoiceType extends AttributeType[Choice] { def value = Zero} 

trait EntityType extends Type 

trait Link extends Type

case class Head(source: Entity, link: Link) extends Key 

case class Relation(source: Entity, link: Link, submodel: Model) extends Elem {
  val myType = Relation
  val head = Head(source, link)
  override def key: Head = head
  override def node: Model = submodel
  override lazy val toString = s"$source $link ${submodel.toStringBody}"
}
case object Relation extends Type {
  def apply(head: Head, submodel: Model): Relation = new Relation(head.source, head.link, submodel) 
}  
  







