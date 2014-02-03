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

import scala.language.implicitConversions

import scala.collection.immutable.ListMap
import scala.collection.immutable.HashMap

import reqT.BagUtil._
import Model.pairToElem

trait Model extends MapTo {

//Abstract:
  
  protected [Model] def myMap: Map[Key, MapTo]      
  protected [Model] def newModel(m: Map[Key, MapTo]): Model  = Model.fromMap(m)

  def empty: Model 
  //def toListMap: ListMap[Key, MapTo]
  //def toHashMap: HashMap[Key, MapTo]

//Overrides:

  override val myType: Type = Model

//Implemented:  

  def canEqual(other: Any): Boolean = other.isInstanceOf[Model] //but subclasses to Model are final. is this needed??
  override def equals(other: Any): Boolean = other match {
    case that: Model => (that canEqual this) && (myMap == that.myMap)
    case _ => false
  }
  override def hashCode: Int = myMap.hashCode

  def get(key: Key):Option[MapTo] = myMap.get(key)
  def +(e: Elem): Model = e match { 
    case rel: Relation if isDefinedAt(rel.key) => 
      var newSubmodel: Model = apply(rel.key)
      for (e <- rel.tail.elems) newSubmodel += e
      newModel(myMap + (rel.key -> newSubmodel))
    case _ => 
      newModel(myMap + e.toPair)
  }
  def -(k: Key): Model = newModel(myMap - k)
  def iterator:Iterator[(Key, MapTo)] = myMap.iterator
  def stringPrefix: String = "Model"  
  
  def isDefinedAt(k: Key) = myMap.isDefinedAt(k)
  lazy val size: Int = myMap.size
  
  def foreach[U](f: Elem => U): Unit = elems.foreach(f)
  def filter(f: Elem => Boolean): Model = elems.filter(f).toModel
  def withFilter(f: Elem => Boolean): Model = elems.withFilter(f).toModel
  def filterNot(f: Elem => Boolean): Model = elems.filterNot(f).toModel
  
  def foreachElem[U](f: Elem => U): Unit = elems.foreach { e => 
    e match {
      case n: Node     => f(n)
      case r: Relation => f(r); r.tail.foreach(f)      
    }
  }
  
  def foreachNode[U](f: Node => U): Unit = elems.foreach { e => 
    e match {
      case n: Node     => f(n)
      case r: Relation => r.tail.foreachNode(f)      
    }
  }

  lazy val keys = myMap.keys
  lazy val keySet = myMap.keySet
  lazy val toVector = elems
  lazy val toSeq = toVector.toSeq
  lazy val toSet: Set[Elem] = elems.toSet
  lazy val toMap: Map[Key, MapTo] = myMap
  
  def ++(that: Model): Model = Model((elems ++ that.elems):_*)
  
  def diffKeys(that: Model): Model = newModel(myMap -- that.keys) //should not this be deep????
  def diff(that: Model): Model = (elems diff that.elems).toModel //should not this be deep????
  def --(that: Model): Model = diff(that)  //is diff == diffKeys ??? NO!?

  // the vectors allows for traversal in elems order if ListModel and they are as type specific as possible which is not a problem as Vector is covariant
  // the sets allow for fast contains tests but are all of Set[Elem] as Sets are invariant 
  
  def elemIterator:Iterator[Elem] = iterator.map(pairToElem)
  lazy val elems: Vector[Elem] = elemIterator.toVector
  lazy val topRelations: Vector[Relation] = elems.collect { case r: Relation => r }
  lazy val topRelationKeys: Vector[Head] = topRelations.map { _.key }
  lazy val topHeads: Vector[Entity] = topRelations.map { _.head }
  lazy val topHeadSet: Set[Elem] = topHeads.toSet
  lazy val topNodes: Vector[Node] = elems.collect { case n: Node => n }
  lazy val topNodeSet: Set[Elem] = topNodes.toSet
  lazy val topNodesAndHeads: Vector[Node] = elems.collect { 
    case n: Node => n 
    case r: Relation => r.head      
  } .distinct
  lazy val topElems: Vector[Elem] = elems.flatMap { //expand with head entities if not there already
    case r: Relation if !topNodeSet.contains(r.head) => Vector(r.head, r) 
    case elem => Vector(elem)
  }   //(elems ++ topHeads).distinct will not give this order
  lazy val topEntities: Vector[Entity] = topElems.collect { case e: Entity => e } 
  lazy val topAttributes: Vector[Attribute[_]] = topElems.collect { case a: Attribute[_] => a }
  lazy val topDistinct = topElems.filterNot { topHeadSet.contains(_) } 
  lazy val topEntitiesOfType: Map[EntityType, Vector[Entity]] = 
    Bag(topEntities.map( e => (e.myType, e)):_*).withDefaultValue(Vector())
  lazy val top: Model = topElems.toModel
  def `^`: Model = top
  lazy val topRecursive: Model = ???
  def `^^`: Model = topRecursive
    
  def apply[T](at: AttributeType[T]): T = myMap(at).asInstanceOf[Attribute[T]].value
  def apply(e: Entity): Entity = myMap(e).asInstanceOf[Entity]
  def apply(et: EntityType): Vector[Entity] = topEntitiesOfType(et) 
  def apply(h: Head): Model =  myMap(h).asInstanceOf[Model]

  def get[T](at: AttributeType[T]): Option[T] = if (isDefinedAt(at)) Some(apply(at)) else None 
  def get(e: Entity): Option[Entity] = if (isDefinedAt(e)) Some(e) else None 
  def get(et: EntityType): Vector[Entity] = topEntitiesOfType(et)
  def get(h: Head): Option[Model] = if (isDefinedAt(h)) Some(apply(h)) else None 

  def isDefinedAt[T](a: Attribute[T]): Boolean = isDefinedAt(a.myType) && (apply(a.myType) == a)
  def isDefinedAt(et: EntityType): Boolean = !topEntitiesOfType(et).isEmpty
  
  def existsElem(p: Elem => Boolean): Boolean = myMap.exists((kc: (Key, MapTo) )=> p(pairToElem(kc))) //??? rename to exists?
  
  def visitAll[T](f: PartialFunction[Elem,T]): Vector[T] = elems.flatMap ( e => 
    e match {
      case n: Node if f.isDefinedAt(e) => Vector(f(e))
      case rel: Relation => 
        ( if (f.isDefinedAt(rel.head)) Vector(f(rel.head)) else Vector[T]() ) ++
          ( if (f.isDefinedAt(rel)) Vector(f(rel)) else Vector[T]() ) ++  
            ( rel.tail.visitAll(f) )
      case _ => Vector()
    }
  )
  
  def bfs[T](f: PartialFunction[Elem,T]): Vector[T] = 
    topElems.collect(f) ++ topRelations.map(_.tail.bfs(f)).flatten

  def dfs[T](f: PartialFunction[Elem,T]): Vector[T] = 
    topRelations.map(_.tail.dfs(f)).flatten ++ topElems.collect(f)
    
  def filterTop(p: Elem => Boolean): Model = topElems.filter(p).toModel 
  def filterTopNot(p: Elem => Boolean): Model = topElems.filterNot(p).toModel 
  def filterShallow(p: Elem => Boolean): Model = elems.filter(p).toModel 
  def filterShallowNot(p: Elem => Boolean): Model = elems.filterNot(p).toModel 

  def filterDeep(p: Elem => Boolean): Model = newModel( myMap.flatMap {
    case kc if p(pairToElem(kc)) => pairToElem(kc) match {
      case Relation(s,l,tail) =>  
        if (p(s) || tail.existsElem(p)) 
          Vector(Relation(s, l, tail.filterDeep(p)).toPair)
        else 
          Vector() 
      case _ => Vector(kc)
    }
    case _ => Vector()
  } )

  def filterDeepNot(p: Elem => Boolean): Model = newModel( myMap.flatMap {
    case kc if !p(pairToElem(kc)) => pairToElem(kc) match {
      case Relation(s,l,tail) =>  
        if (!p(s) && !tail.existsElem(p)) 
          Vector(Relation(s, l, tail.filterDeepNot(p)).toPair)
        else 
          Vector() 
      case _ => Vector(kc)
    }
    case _ => Vector()
  } )
  
  def separateKeysOrTails(s: Selector, separator: (Elem => Boolean) => Model): Model = 
    s match {
      case et: EntityType => separator { 
        case e: Entity if e.myType == et => true 
        case Relation(s, _, t) if s.myType == et || t.isDefinedAt(et) => true
        case _ => false
      }
      case at: AttributeType[_] => separator { 
        case a: Attribute[_] if a.myType == at => true 
        case Relation(_, _, tail) if tail.isDefinedAt(at) => true
        case _ => false
      }
      case _ => throw new scala.NotImplementedError("separate on" + s)
    }

  def separateKeys(s: Selector, separator: (Elem => Boolean) => Model): Model = 
    s match {
      case et: EntityType => separator { 
        case e: Entity if e.myType == et => true 
        case Relation(s, _, _) if s.myType == et => true
        case _ => false
      }
      case at: AttributeType[_] => separator { 
        case a: Attribute[_] if a.myType == at => true 
        case _ => false
      }
      case _ => throw new scala.NotImplementedError("separateKeys on" + s)
    }      

  def separateTails(s: Selector, separator: (Elem => Boolean) => Model): Model = 
    s match {
      case et: EntityType => separator { 
        case Relation(_, _, tail) if tail.isDefinedAt(et) => true
        case _ => false
      }
      case at: AttributeType[_] => separator { 
        case Relation(_, _, tail) if tail.isDefinedAt(at) => true
        case _ => false
      }
      case _ => throw new scala.NotImplementedError("separateTails on" + s)
    }       
    
  def enter(e: Entity): Model = get(e.has).getOrElse(Model())
  def enter(h: Head): Model = get(h).getOrElse(Model())
  def enter[T](at: AttributeType[T]): T = get(at).getOrElse(at.default.asInstanceOf[T])

  def /(e: Entity): Model = enter(e)
  def /(h: Head): Model = enter(h)
  def /[T](at: AttributeType[T]): T = enter(at)
  
  def restrict(s: Selector): Model = separateKeysOrTails(s, filterTop)    
  def *(s: Selector): Model = restrict(s)
  
  def restrictTop(s: Selector): Model = separateKeys(s, filterTop)    
  def *^(s: Selector): Model = restrictTop(s)
  
  def restrictTails(s: Selector): Model = separateTails(s, filterTop)   
  def *~(s: Selector): Model = restrictTails(s)

  def restrictAll(s: Selector): Model = separateKeysOrTails(s, filterDeep)    
  def **(s: Selector): Model = restrictAll(s)
  
  def exclude(s: Selector): Model = separateKeysOrTails(s, filterNot)    
  def \(s: Selector): Model = exclude(s)

  def excludeTop(s: Selector): Model = separateKeys(s, filterNot)
  def \^(s: Selector): Model = excludeTop(s)
  
  def excludeTail(s: Selector): Model = separateTails(s, filterNot)
  def \~(s: Selector): Model = excludeTail(s)

  def excludeAll(s: Selector): Model = separateKeysOrTails(s, filterDeepNot)    
  def \\(s: Selector): Model = excludeAll(s)
 
  lazy val toStringBody = 
    if (size == 0) "()" 
    else if (size == 1 && elems(0).isNode) elems.mkString 
    else elems.mkString("(", ", ", ")")
  override def toString = stringPrefix + elems.mkString("(\n  ", ",\n  ", "\n)")
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

  def apply(m: ListMap[Key, MapTo]): Model = new ListModel(m)
  def apply(m: HashMap[Key, MapTo]): Model = new HashModel(m)
  def pairToElem(pair: (Key, MapTo)):Elem = pair match {
    case (_, a: Attribute[_]) => a
    case (e: Entity, _) => e
    case (head: Head, tail: Model) => Relation(head, tail)
    case _ => throw new IllegalArgumentException(pair + " must be (Key, MapTo)")
  }   
}

object Model extends Type with ModelCompanion {
  def fromMap(m: Map[Key, MapTo]): Model = m match {
    case lm: ListMap[Key, MapTo] => ListModel(lm)
    case hm: HashMap[Key, MapTo] => HashModel(hm)
    case otherTypeOfMap => otherTypeOfMap.toSeq.toModel
  }
}

final class ListModel private [reqT] ( override val myMap: ListMap[Key, MapTo]) extends Model {
  type MapType = ListMap[Key, MapTo]
  override def empty: ListModel = ListModel.empty  
  //override def newModel(m: MapType): Model = new ListModel(m) 
  //override def toListMap: ListMap[Key, MapTo] = myMap
  //override def toHashMap: HashMap[Key, MapTo] = myMap
}

object ListModel extends ModelCompanion {
  override def empty: ListModel = new ListModel(ListMap.empty)  
}

final class HashModel private [reqT] ( override val myMap: HashMap[Key, MapTo]) extends Model {
  type MapType = HashMap[Key, MapTo]
  override def empty: HashModel = HashModel.empty  
  //override def newModel(m: MapType): Model = new HashModel(m)
}

object HashModel extends ModelCompanion {
  override def empty: HashModel = new HashModel(HashMap.empty)  
  //override def toListMap: ListMap[Key, MapTo] = myMap
  //override def toHashMap: HashMap[Key, MapTo] = myMap
}






