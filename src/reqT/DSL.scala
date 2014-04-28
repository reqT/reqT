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
sealed trait Selector {
  /*** old to be removed
  def isTypeMatch(that: HasType): Boolean = this.isInstanceOf[TypeObject] && that.myType == this 
  def selects(that: Selector): Boolean = ( this == that ) || ( that match {
      case Head(e,l) if this.isInstanceOf[HeadType] => HeadType(e.myType, l) == this 
      case Relation(e,l,t) if this.isInstanceOf[HeadType] => HeadType(e.myType, l) == this 
      case ht: HasType => isTypeMatch(ht)
      case _ => false } )
  ***---*/
  
  def =*=(that: Elem): Boolean = isMatch(that)
  def isMatch(that: Elem): Boolean = this match {
    case NotSelector(ns)   => ! ( ns =*= that )
    case AndSelector(s1, s2) => ( s1 =*= that ) && ( s2 =*= that )  
    case OrSelector (s1, s2) => ( s1 =*= that ) || ( s2 =*= that )
    case Select(ifMatches) => ifMatches(that) //är detta egentligen användbart??
    case _ if this == that || 
      ( this.isInstanceOf[TypeObject] && this == that.myType) => true
    case HeadType(thisEntType, thisLink) => that match {
      case Relation(thatEnt, thatLink, _) if thisEntType == thatEnt.myType && 
        thisLink == thatLink => true   
      case Relation(_, _, thatModel) => thatModel.contains( this )
      case _ => false
    }
    case Head(thisEnt, thisLink) => that match {
      case Relation(thatEnt, thatLink, _) if thisEnt == thatEnt && thisLink == thatLink => true   
      case Relation(_, _, thatModel) => thatModel.contains( this )
      case _ => false
    }
    case Relation(thisEntity, thisLink, thisModel) => that match {
      case Relation(thatEnt, thatLink, thatModel) if thisEntity == thatEnt && thisLink == thatLink =>
        thisModel == thatModel ||
          thisModel.elems.map(e => thatModel.elems.contains(e)).fold(true)(_ && _) ||
            thatModel.contains( this ) 
      case Relation(_, _, thatModel) => thatModel.contains( this )
      case _ => false
    }
    case _ => that match {
      case Relation(thatEnt, thatLink, thatModel) => 
        this =*= thatEnt || this == thatLink || thatModel.contains( this )
      case _ => false
    }
  }
  
  def && (that: Selector): AndSelector = AndSelector(this, that)
  def || (that: Selector): OrSelector = OrSelector(this, that)
  def unary_!  = NotSelector(this)
  def restrict(that: Model): Model = that * this
  def * (that: Model): Model = that * this
}

case class AndSelector(left: Selector, right: Selector) extends Selector 
case class OrSelector (left: Selector, right: Selector) extends Selector
case class NotSelector(selector: Selector) extends Selector 
case class Select(predicate: Elem => Boolean) extends Selector

/** A marker trait for types of runtime typing using case objects. */
sealed trait TypeObject extends Selector   

/** A mixin trait for runtime typing through case object values of type TypeObject. */
trait HasType { def myType: TypeObject } 

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

case object NoElem extends Elem with TypeObject { 
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
  def toScalaBody: String
}

sealed trait MapTo extends DSL with HasType  

trait Attribute[T] extends Node with MapTo with HasValue[T] with CanBeMapped {
  override def myType: AttributeType[T]
  override def key: AttributeType[T] = myType
  override def mapTo: Attribute[T] = this
  override def isAttribute: Boolean = true
  override def toScalaBody: String = value.toString
  override def toScala: String =  myType + "(" + toScalaBody + ")"
  def / = AttrVal(HeadPath(), this)
}

sealed trait Key extends DSL with Selector

trait Model extends MapTo with ModelImplementation 
  
object Model extends TypeObject 
  with ModelCompanion 
  with ModelFromMap


trait AttributeType[T] extends Key with TypeObject with HasDefault[T] { 
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
  override def toScalaBody = id.toScala
  override def toScala: String = myType + "(" + toScalaBody + ")"
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
  def superOf(elems: Elem*) = Relation(this, reqT.superOf, Model(elems:_*))
  def superOf(submodel: Model) = Relation(this, reqT.superOf, submodel)
  def superOf = Head(this, reqT.superOf)
}
case object NoEntity extends Entity {
  override val id = ""
  override val myType = NoEntityType
}

trait Context extends Entity
trait General extends Entity
trait Requirement extends Entity

trait EntityType extends TypeObject with HeadTypeFactory  {
  def apply(id: String): Entity
  def apply(): Entity = apply(nextId)
  def apply(i: Int): Entity = apply(i.toString)
  def has = HeadType(this, reqT.has)
  def is =  HeadType(this, reqT.is)
  def superOf =  HeadType(this, reqT.superOf)
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

trait RelationType extends TypeObject with HasType { def myType = this }
case object NoLink extends RelationType

case class Head(entity: Entity, link: RelationType) extends Key {
  def / = HeadPath(this)
  def /(h: Head) = HeadPath(this, h)
  def /(e: Entity) = HeadPath(this, e.has)
  def /[T](at: AttributeType[T]) = AttrRef[T](HeadPath(this), at)
  def /[T](a: Attribute[T]) = AttrVal[T](HeadPath(this), a)
  override def toString = entity + "." + link
  override def toScala = entity.toScala + "." + link
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
case object Relation extends TypeObject {
  def apply(h: Head, tail: Model): Relation = new Relation(h.entity, h.link, tail) 
}  

case class HeadType(entityType: EntityType, link: RelationType) extends TypeObject 

trait AttrMaker[T <: Attribute[_]] { def apply(s: String): T }

trait CanMakeAttr {
  def makeAttribute[T <: Attribute[_]](value: String)( implicit make: AttrMaker[T]): T = make(value)
}

trait MetamodelTypes {
  def types: Vector[TypeObject]
  lazy val names: Vector[String] = types map (_.toString)
  lazy val indexOf: Map[String, Int] = names.zipWithIndex.toMap.withDefaultValue(-1)
}

//Primitive Attributes traits
trait StringAttribute extends Attribute[String] {
  override def toScalaBody = value.toScala
}
trait StringType extends AttributeType[String] { 
  val default = "???"
  override  def apply(value: String): StringAttribute
}

trait IntAttribute extends Attribute[Int] 
trait IntType extends AttributeType[Int] { 
  val default = -999999999
  override  def apply(value: Int): IntAttribute
} 

trait VectorAttribute[T] extends Attribute[Vector[T]] {
  override def toScala: String = myType + "(" + value.map(_.toString).mkString(",") + ")"
}
trait VectorType[T] extends AttributeType[Vector[T]] {
  val default: Vector[T] = Vector()
  override def apply(cs: Vector[T]): VectorAttribute[T]
}

case class Constr(cnstr: String)

trait ConstrVectorAttribute extends VectorAttribute[Constr] 
trait ConstrVectorType extends VectorType[Constr] {
  override def apply(cs: Vector[Constr]) = Constraints(cs)
  def apply(cs: Constr*): Constraints = Constraints(cs.toVector)
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
case class Meta(id: String) extends Entity { override val myType: EntityType = Meta }
case object Meta extends EntityType

case class Ent(id: String) extends Entity { override val myType: EntityType = Ent }
case object Ent extends EntityType

case class Attr(value: String) extends StringAttribute { override val myType = Attr }
case object Attr extends StringType 

case class Code(value: String) extends StringAttribute { override val myType = Code }
case object Code extends StringType 

case class Constraints(value: Vector[Constr]) extends ConstrVectorAttribute { override val myType = Constraints }
case object Constraints extends ConstrVectorType {
  def apply(s: String): Constraints = reqT.repl.interpret(s). map ( _ match { 
    case c: Constraints => c 
    case _ => Constraints(Vector()) 
  } ).get 
}

case object has extends RelationType  
case object is extends RelationType  
case object superOf extends RelationType  
  
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
