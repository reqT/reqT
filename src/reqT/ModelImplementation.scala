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

import scala.collection.immutable.ListMap
import scala.collection.immutable.HashMap

trait ModelImplementation extends ModelBase  //mixins for trait Model in DSL
  with ModelBasicOps
  with ModelElemAccess
  with ModelEquality
  with ModelIterators 
  with ModelSeparators 
  with ModelTesting { 
    self: Model => 
}

trait ModelBase extends Serializable {
  self: Model =>
 
  protected [ModelBase] def myMap: Map[Key, MapTo]      

  def empty: Model 
  def toListMap: ListMap[Key, MapTo]
  def toHashMap: HashMap[Key, MapTo]
  
  override val myType: TypeObject = Model
  val stringPrefix: String = "Model"  

  lazy val toIterable:Iterable[Elem] = myMap.map(_.toElem)
  lazy val elems: Vector[Elem] = toIterable.toVector
  lazy val toVector: Vector[Elem] = elems
  lazy val toList: List[Elem] = elems.toList
  lazy val toSeq = elems.toSeq
  lazy val toSet: Set[Elem] = elems.toSet
  lazy val toMap: Map[Key, MapTo] = myMap
  def toListModel = Model.fromMap(toListMap) 
  def toHashModel = Model.fromMap(toHashMap)

  def iterator:Iterator[Elem] = toIterable.iterator
  def mapIterator: Iterator[(Key, MapTo)] = myMap.iterator
  
  lazy val toStringBody = 
    if (size == 0) "()" 
    else if (size == 1 && elems(0).isNode) elems.mkString 
    else elems.mkString("(", ", ", ")")
    
  lazy val toStringSimple = stringPrefix +  toStringBody
  lazy val toStringSimpleLineBreak =  stringPrefix + ( if (isEmpty) "()" 
    else if (toStringBody.size < Settings.lineLength) elems.mkString("(", ", ", ")")
    else elems.mkString("(\n  ", ",\n  ", "\n)") ) 
  
  override def toString = Settings.defaultModelToString(this) 
  def toTable = Settings.defaultModelToTable(this) 
  def toGraph = Settings.defaultModelToGraph(this) 
  
  def prettyPrint(): Unit = println(toString)
  def pp(): Unit = prettyPrint()
  
  def save(fileName: String) {
    val outFile = new java.io.FileOutputStream(fileName)
    val out = new java.io.ObjectOutputStream(outFile)
    out.writeObject(this)
    out.close
    outFile.close
    println("Model serialized to file: "+fileName) 
  }
}

object defaultEmptyModel { def apply(): ListModel = new ListModel(ListMap.empty) }

trait ModelCompanion {
  def empty: Model =  defaultEmptyModel() //Default is ListModel
  def apply(elems: Elem*): Model =   {
    var result = empty
    elems.foreach{ e => result += e }
    result
  } 
  def unapplySeq(m: Model): Option[Seq[Elem]] = Some(m.toSeq)

  def apply(m: ListMap[Key, MapTo]): ListModel = new ListModel(m)
  def apply(m: HashMap[Key, MapTo]): HashModel = new HashModel(m)
  
  def pairToElem(pair: (Key, MapTo)):Elem = pair match {
    case (at: AttributeType[_], a: Attribute[_]) => a
    case (head: Head, tail: Model) => 
      if (head.link == has && tail.isEmpty) head.entity
      else Relation(head, tail)
    case _ => throw new IllegalArgumentException(s"$pair illegal combination of (Key, MapTo)")
  }   
  
  def load(fileName: String): Model = {
    val inFile = new java.io.FileInputStream(fileName)
    val in = new java.io.ObjectInputStream(inFile)
    val m: Model = in.readObject.asInstanceOf[Model]
    in.close
    inFile.close
    m
  }
}

trait ModelFromMap {
  def fromMap(m: Map[Key, MapTo]): Model = m match {
    case lm: ListMap[Key, MapTo] => ListModel(lm)       //preserves insertion order but slower
    case hm: HashMap[Key, MapTo] => HashModel(hm)       //pairs in hash order but faster
    case otherTypeOfMap => otherTypeOfMap.toSeq.toModel //unknown: use underlying toSeq order
  }
}

final class ListModel private [reqT] ( override val myMap: ListMap[Key, MapTo]) extends Model {
  type MapType = ListMap[Key, MapTo]
  override def empty: ListModel = ListModel.empty  
  override def toListMap: ListMap[Key, MapTo] = myMap
  override def toHashMap: HashMap[Key, MapTo] = HashMap(myMap.toSeq:_*)
  override def toListModel = this
  override def productPrefix = "ListModel"
}

object ListModel extends ModelCompanion {
  override def empty: ListModel = new ListModel(ListMap.empty)  
}

final class HashModel private [reqT] ( override val myMap: HashMap[Key, MapTo]) extends Model {
  type MapType = HashMap[Key, MapTo]
  override def empty: HashModel = HashModel.empty  
  override def toListMap: ListMap[Key, MapTo] = ListMap(myMap.toSeq:_*)
  override def toHashMap: HashMap[Key, MapTo] = myMap
  override def toHashModel = this
  override def productPrefix = "HashModel"
}

object HashModel extends ModelCompanion {
  override def empty: HashModel = new HashModel(HashMap.empty)  
}





