export Entity._
export Attribute._

trait Elem
case class Model(elems: Elem*)

trait Node extends Elem
enum Attribute[T] extends Node: 
  val value: T 
  case Spec(value: String) extends Attribute[String] 
  case Prio(value: Int) extends Attribute[Int]

enum Entity extends Node:
  val id: String
  case Req(id: String)
  case Feature(id: String)

enum RelationType:  
  case has, requires

case class Relation (
  val entity: Entity,
  val link: RelationType,
  val tail: Model,
) extends Elem

extension [T](e: Entity)
  def has(es: Elem*): Relation = Relation(e, RelationType.has, Model(es: _*)) 
  def requires(es: Elem*): Relation = Relation(e, RelationType.has, Model(es: _*)) 

@main def run = println("*** HELLO Scala 3! ***")
