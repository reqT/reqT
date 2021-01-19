object reqt4 { // a new simpler design for next generation of reqt

  /* jars/subprojects:
    reqt-dsl_2.13-4.1.1.jar dsl bare bone
    reqt-core_2.13-4.1.1.jar dsl with modelops
    reqt-repl-2.13-4.1.1.jar core with jline with IMain
    reqt-2.13-4.1.1.jar repl with swing gui
  */
  // metametamodel:

  final case class Model(toSeq: Elem*)

  sealed trait Elem
  sealed trait Node extends Elem

  trait EntityType extends Product with Serializable {
    def apply(id: String): Entity = Entity(this, id)
  }


  final case class Entity(tpe: EntityType, id: String) extends Node {
    override def toString = s"""$tpe("$id")"""
  }

 trait AttributeType[T] extends Product with Serializable {
    def valueType: ValueType
    def apply(value: T): Attribute[T] = Attribute[T](this, value)
  }

  final case class Attribute[T](tpe: AttributeType[T], value: T) extends Node {
    override def toString = s"""$tpe($value)"""
  }

  trait RelationType extends Product with Serializable
  trait ValueType extends Product with Serializable

  final case class Relation(entity: Entity, link: RelationType, tail: Model) extends Elem {
    override def toString = tail match {
      case Model(Seq()) => s"$entity"
      case Model(Seq(e)) => s"$entity $link $e"
      case Model(es@ _*) => s"""$entity $link (${es.mkString(",")})"""
    }
  }

  // metamodel:

  case object Feature extends EntityType
  case object Req     extends EntityType

  case object IntValue extends ValueType
  case object StringValue extends ValueType

  case object Prio extends AttributeType[Int]    { val valueType = IntValue }
  case object Spec extends AttributeType[String] { val valueType = StringValue}

  case object has extends RelationType
  case object requires extends RelationType

  implicit class RelationConstructors(e: Entity) {
      def has(elems: Elem*): Relation = Relation(e, reqt.has, Model(elems:_*))
      def has(m: Model): Relation = Relation(e, reqt.has, m)

      def requires(elems: Elem*): Relation = Relation(e, reqt.requires, Model(elems:_*))
      def requires(m: Model): Relation = Relation(e, reqt.requires, m)
  }
}
