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
import scala.collection.immutable.ListMap
import scala.collection.immutable.HashMap
import scala.collection.generic.FilterMonadic

trait ModelImplementation  {
  self: Model =>
  
  import BagUtils._
  import Model.pairToElem
  import Model.{fromMap => newModel}

//Abstract:
  
  protected [ModelImplementation] def myMap: Map[Key, MapTo]      

  def empty: Model 
  def toListMap: ListMap[Key, MapTo]
  def toHashMap: HashMap[Key, MapTo]
  
//Field from construction:  
  override val myType: MetaType = Model

//to stuff:  
  lazy val toVector = elems
  lazy val toSeq = toVector.toSeq
  lazy val toSet: Set[Elem] = elems.toSet
  lazy val toMap: Map[Key, MapTo] = myMap
  def toListModel = Model.fromMap(myMap) 
  def toHashModel = Model.fromMap(myMap)

  lazy val toIterable:Iterable[Elem] = myMap.map(_.toElem)
  lazy val elems: Vector[Elem] = toIterable.toVector
  def iterator:Iterator[Elem] = toIterable.iterator
  def mapIterator: Iterator[(Key, MapTo)] = myMap.iterator
  val stringPrefix: String = "Model"  

//add stuff:
  def +(e: Elem): Model = append(e)
  def append(e: Elem): Model = e match { 
    case rel: Relation if isDefinedAt(rel.key) => 
      var newSubmodel: Model = apply(rel.key)
      for (e <- rel.tail.elems) newSubmodel += e
      newModel(myMap + (rel.key -> newSubmodel))
    case  NoElem => this
    case _ => newModel(myMap + e.toMapping)
  }
  
  def +(p: HeadPath): Model = this ++ p.toModel
  def +[T](p: AttrVal[T]): Model = this ++ p.toModel  
  def aggregate(that: Model): Model = this ++ that 
  def ++(that: Model): Model = { var r = this ; that.foreach { r += _ } ; r }  
  
//remove stuff:  
  def -(k: Key): Model = newModel(myMap - k)  //deep???
  def diffKeys(that: Model): Model = newModel(myMap -- that.keys) //should not this be deep????
  def diff(that: Model): Model = (elems diff that.elems).toModel //should not this be deep????
  def --(that: Model): Model = diff(that)  //is diff == diffKeys ??? NO!?
  
//size calculations:
  val mapSize: Int = myMap.size
  lazy val topSize: Int = top.size
  lazy val size: Int = { var n = 0 ; foreachElem { n += 1 } ; n }
  lazy val nodeSize: Int = { var n = 0 ; foreachNode { n += 1 } ; n }
  val isEmpty: Boolean = mapSize == 0

//iteration stuff:  
  def foreach[U](f: Elem => U): Unit = toIterable.foreach(f)
  def foreach(block: => Unit): Unit = foreach(_ => block)
  def filter(f: Elem => Boolean): Model = toIterable.filter(f).toModel
  def filterNot(f: Elem => Boolean): Model = toIterable.filterNot(f).toModel
  def withFilter(f: Elem => Boolean): FilterMonadic[reqT.Elem,Iterable[reqT.Elem]] = toIterable.withFilter(f)
  def map[U](f: Elem => U): Iterable[U] = toIterable.map(f)
  
  def foreachElem[U](f: Elem => U): Unit = elems.foreach { e => 
    e match {
      case n: Node     => f(n)
      case r: Relation => f(r); r.tail.foreachElem(f)      
      case NoElem =>       
    }
  }
  def foreachElem(block: => Unit): Unit = foreachElem { _ => block }
  
  def foreachNode[U](f: Node => U): Unit = elems.foreach { e => 
    e match {
      case n: Node     => f(n)
      case r: Relation => f(r.entity); r.tail.foreachNode(f) 
      case NoElem =>
    }
  }
  def foreachNode(block: => Unit): Unit = foreachNode { _ => block }

//collection of elements:  
  lazy val keys: Iterable[Key] = myMap.keys
  lazy val keySet: Set[Key] = myMap.keySet
  lazy val keyVector: Vector[Key] = myMap.collect { case (k,v) => k } .toVector
  
  // the vectors allows for traversal in elems order if ListModel and they are as type specific as possible which is not a problem as Vector is covariant
  // the sets allow for fast contains tests but are all of Set[Elem] as Sets are invariant 

  lazy val topNodes: Vector[Node] = elems.collect { 
    case Relation(e, l, tail) => e
    case n: Node => n
  }  
  lazy val topNodeSet: Set[Elem] = topNodes.toSet
  lazy val top: Model = elems.collect {
    case Relation(e, l, tail) => Relation(e, l, tail.topNodes.toModel)
    case elem => elem
  } .toModel
  lazy val tip: Model = elems.collect {
    case Relation(e, l, tail) => e
    case elem => elem
  } .toModel  
  def `^`: Model = top
  
//------------------ check below
  lazy val head: Elem = myMap.head.toElem
  lazy val headOption: Option[Elem] = myMap.headOption.map(_.toElem)
  lazy val tail: Model = myMap.tail.toModel
  lazy val topRelations: Vector[Relation] = myMap.collect { case (h: Head, m: Model) => Relation(h,m) } .toVector
  lazy val topRelationTypes: Vector[RelationType] = topRelations.collect { case r => r.link }
  lazy val topEntities: Vector[Entity] = myMap.collect { case (h: Head, _) => h.entity } .toVector
  lazy val topEntitySet: Set[Elem] = topEntities.toSet
  lazy val topHeads: Vector[Head] = myMap.keys.collect { case h: Head => h } .toVector
  lazy val topNodesAndHeads: Vector[Node] = elems.collect { 
    case n: Node => n 
    case r: Relation => r.entity      
  } .distinct
  lazy val topElems: Vector[Elem] = elems.flatMap { //expand with head entities if not there already ???
    case r: Relation if !topNodeSet.contains(r.entity) => Vector(r.entity, r) 
    case elem => Vector(elem)
  }   //(elems ++ topEntities).distinct will not give this order
  //lazy val topEntities: Vector[Entity] = topElems.collect { case e: Entity => e }  ??
  lazy val topAttributes: Vector[Attribute[_]] = topElems.collect { case a: Attribute[_] => a }
  lazy val topDistinct = topElems.filterNot { topEntities.contains(_) } 
  lazy val topEntitiesOfType: Map[EntityType, Vector[Entity]] = 
    Bag(topEntities.map( e => (e.myType, e)):_*).withDefaultValue(Vector())
  lazy val topRelationsOfType: Map[RelationType, Vector[Relation]] =
    Bag(topRelations.map( r => (r.link, r)):_*).withDefaultValue(Vector())
  lazy val topHeadsOfType: Map[HeadType, Vector[Head]] =
    Bag(topHeads.map( h => (HeadType(h.entity.myType, h.link), h)):_*).withDefaultValue(Vector())

  lazy val topIds: Vector[String] = topEntities.map(_.id)
  lazy val topEntityOfId : Map[String, Entity] = topEntities.map(e => (e.id, e)).toMap.withDefaultValue(NoEntity)
    
  def existsElem(p: Elem => Boolean): Boolean = myMap.exists((kc: (Key, MapTo) )=> p(pairToElem(kc))) //??? rename to exists?
  
  def visitAll[T](f: PartialFunction[Elem,T]): Vector[T] = elems.flatMap ( e => 
    e match {
      case n: Node if f.isDefinedAt(e) => Vector(f(e))
      case rel: Relation => 
        ( if (f.isDefinedAt(rel.entity)) Vector(f(rel.entity)) else Vector[T]() ) ++
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
          Vector(Relation(s, l, tail.filterDeep(p)).toMapping)
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
          Vector(Relation(s, l, tail.filterDeepNot(p)).toMapping)
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
      case e: Entity => separator {
        case e2: Entity if e == e2 => true
        case Relation(e2, _, _) if e == e2 => true
        case _ => false
      }
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
  override def toString = stringPrefix + ( if (isEmpty) "()" 
    else if (toStringBody.size < 60) elems.mkString("(", ", ", ")")
    else elems.mkString("(\n  ", ",\n  ", "\n)") )
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
}

object HashModel extends ModelCompanion {
  override def empty: HashModel = new HashModel(HashMap.empty)  
}





