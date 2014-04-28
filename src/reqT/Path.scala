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

sealed trait Path extends DSL {
  def heads: Vector[Head]
  def tail: Path
  def init: Path 
  val isSingle = heads.size == 1
  val isEmpty = heads.size == 0
  def level: Int
  def / = this
  def pathError(a: Any) = 
    throw new java.lang.UnsupportedOperationException(s"$this / $a") 
  def /(h: Head): HeadPath = pathError(h)
  def /(e: Entity): HeadPath = pathError(e)
  def /[T](at: AttributeType[T]): AttrRef[T] = pathError(at)
  def /[T](a: Attribute[T]):AttrVal[T] = pathError(a)
  
  lazy val head = heads.head
  lazy val headOption: Option[Head] = heads.headOption
  override lazy val toScala = heads.map(_.toScala).mkString("","/","/")
}

sealed trait NodePath extends Path {
  def lastNode: Node
}

case class HeadPath(heads: Vector[Head]) extends NodePath {
  def toModel: Model = if (isEmpty) Model() 
    else if (isSingle) Model(Relation(head, Model())) 
    else Model(Relation(head, tail.toModel)) 
  override def /(h: Head) = HeadPath(heads :+ h)
  override def /(e: Entity) = HeadPath(heads :+ e.has)
  override def /[T](at: AttributeType[T]) = AttrRef[T](this, at)
  override def /[T](a: Attribute[T]) = AttrVal[T](this, a)
  lazy val tail = HeadPath(heads.tail)
  lazy val init = HeadPath(heads.init)
  lazy val lastNode: Entity = heads.lastOption.map(_.entity).getOrElse(NoEntity)
  override lazy val level = heads.size 
  override lazy val toString = heads.mkString("", "/","/")
}
object HeadPath {
  def apply(hs: Head*) = new HeadPath(hs.toVector)
}

case class AttrVal[T](init: HeadPath, attr: Attribute[T]) extends NodePath {
  def toModel: Model = if (isEmpty) Model(attr) 
    else if (isSingle) Model(Relation(head, Model(attr))) 
    else Model(Relation(head, tail.toModel)) 
  override lazy val heads = init.heads
  lazy val tail = AttrVal(HeadPath(heads.drop(1)), attr)
  lazy val lastNode: Attribute[T] = attr 
  override lazy val level = heads.size + 1
  override lazy val toString = ( if (init.isEmpty) "" else init.toString )  + attr + "/"
  override lazy val toScala = ( if (init.isEmpty) "" else init.toScala ) + attr.toScala + "/"
}

case class AttrRef[T](init: HeadPath, attrType: AttributeType[T]) extends Path {
  override lazy val heads = init.heads
  lazy val tail = AttrRef(HeadPath(heads.drop(1)), attrType)
  //def apply(m: Model) = ModelUpdater(m, this)
  override lazy val level = heads.size + 1
  override lazy val toString = ( if (init.isEmpty) "" else init.toString )  + attrType + "/"
  override lazy val toScala = ( if (init.isEmpty) "" else init.toScala ) + attrType + "/"
}

trait RootHeadPathFactory {
  def / = HeadPath()
}
//case class ModelUpdater[T](m: Model, r: AttrRef[T]) {
  // //to enable DSL syntax Stakeholder("a")/Req("x")/Prio(Model()) := 3 
  //def :=(value: T): Model =  m.updated(r, value)
// }