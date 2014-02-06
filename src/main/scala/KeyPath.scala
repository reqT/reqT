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

trait ModelPath {
  def heads: Vector[Head]
  def tail: ModelPath
  val isSingle = heads.size == 1
  val isEmpty = heads.isEmpty
  val head = heads.head
  val headOption: Option[Head] = heads.headOption
}

case class HeadPath(heads: Vector[Head]) extends ModelPath {
  def toModel: Model = if (isEmpty) Model() 
    else if (isSingle) Model(Relation(head, Model())) 
    else Model(Relation(head, tail.toModel)) 
  def /(h: Head) = HeadPath(heads :+ h)
  def /(e: Entity) = HeadPath(heads :+ e.has)
  def /[T](at: AttributeType[T]) = AttrRef[T](this, at)
  def /[T](a: Attribute[T]) = AttrVal[T](this, a)
  def / = this
  lazy val tail = HeadPath(heads.tail)
  override def toString = heads.mkString("/")
}
object HeadPath {
  def apply(hs: Head*) = new HeadPath(hs.toVector)
}

case class AttrRef[T](init: HeadPath, last: AttributeType[T]) extends ModelPath {
  override lazy val heads = init.heads
  lazy val tail = AttrRef(HeadPath(heads.drop(1)), last)
  //def apply(m: Model) = ModelUpdater(m, this)
  override def toString = init.toString + "/" + last 
}

case class AttrVal[T](init: HeadPath, last: Attribute[T]) extends ModelPath {
  def toModel: Model = if (isEmpty) Model(last) 
    else if (isSingle) Model(Relation(head, Model(last))) 
    else Model(Relation(head, tail.toModel)) 
  override lazy val heads = init.heads
  lazy val tail = AttrVal(HeadPath(heads.drop(1)), last)
  override def toString = init.toString + "/" + last 
}

//case class ModelUpdater[T](m: Model, r: AttrRef[T]) {
  // //to enable DSL syntax Stakeholder("a")/Req("x")/Prio(Model()) := 3 
  //def :=(value: T): Model =  m.updated(r, value)
// }