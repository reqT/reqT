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

import scala.collection.immutable.MapLike
import scala.collection.IndexedSeqLike
import scala.collection.mutable.LinkedHashMap
import scala.collection.immutable.ListMap

import reqT.BagUtil._

class Model private ( private val myMap: collection.mutable.Map[Key, Node]) 
extends Node with Map[Key, Node] with MapLike[Key, Node, Model] {
        
  import Model.{keyNodeToElem, elemToKeyNode} 

  //--------------methods required for integration with Map and MapLike:  
  
  def get(key: Key):Option[Node] = myMap.get(key)
  override def empty = new Model(LinkedHashMap.empty)  
  def +[B1 >: Node](kn: (Key, B1)): Model = kn match { 
    case (head: Head, submodel: Model) if isDefinedAt(head) => 
      var newSubmodel = this (head)
      for (knSub <- submodel) newSubmodel += knSub
      new Model(myMap + (head -> newSubmodel))
    case (k: Key, n: Node) => new Model(myMap + (k -> n))
    case _ => throw new IllegalArgumentException(kn + " must be (Key, Node)") 
  }
  def -(k: Key): Model = new Model(myMap - k)
  def iterator:Iterator[(Key, Node)] = myMap.iterator
  override def stringPrefix: String = "Model" 
  //--- reqT-specific members
  
  override val myType: Type = Model
  
  def ++(that: Model): Model = {
    var m = this
    for (kn <- that) m += kn
    m    
  }
  def +(e: Elem): Model = this + (e.key -> e.node)
  
  def diffKeys(that: Model): Model = this -- that.keys //should not this be deep????
  def diff(that: Model): Model = (elems diff that.elems).toModel //should not this be deep????
  def --(that: Model): Model = diff(that)  //is diff == diffKeys ???

  //SHOULD WE DISTINGUISH LEAFS FROM SOURCES??? DISALLOW BOTH??? Probably not...
  
  // the vectors allows for traversal in elems order and they are as type specific as possible which is not a problem as Vector is covariant
  // the sets allow for fast contains tests but are all of Set[Elem] as Sets are invariant 
  
  def elemIterator:Iterator[Elem] = iterator.map(keyNodeToElem)
  lazy val elems: Vector[Elem] = elemIterator.toVector
  lazy val elemSet: Set[Elem] = elems.toSet
  lazy val topRelations: Vector[Relation] = elems.collect { case r: Relation => r }
  lazy val topHeads: Vector[Head] = topRelations.map { _.head }
  lazy val topSources: Vector[Entity] = topRelations.map { _.source }
  lazy val topSourceSet: Set[Elem] = topSources.toSet
  lazy val topLeafs: Vector[Leaf] = elems.collect { case l: Leaf => l }
  lazy val topLeafSet: Set[Elem] = topLeafs.toSet
  lazy val topLeafsAndSources: Vector[Leaf] = elems.collect { 
    case l: Leaf => l 
    case r: Relation => r.source      
  } .distinct
  lazy val topElems: Vector[Elem] = elems.flatMap { 
    case r: Relation if !topLeafSet.contains(r.source) => Vector(r.source, r) 
    case elem => Vector(elem)
  }   //(elems ++ topSources).distinct will not give this order
  lazy val topEntities: Vector[Entity] = topElems.collect { case e: Entity => e } 
  lazy val topAttributes: Vector[Attribute] = topElems.collect { case a: Attribute => a }
  lazy val topDistinct = topElems.filterNot { topSourceSet.contains(_) } 
  lazy val topEntitiesOfType: Map[EntityType, Vector[Entity]] = 
    Bag(topEntities.map( e => (e.myType, e)):_*).withDefaultValue(Vector())
  lazy val top: Model = topElems.toModel
  def `^`: Model = top
  lazy val topRecursive: Model = ???
  def `^^`: Model = topRecursive
    
  def apply[T](at: AttributeType[T]): T = myMap(at).asInstanceOf[Attribute].value.asInstanceOf[T]
  def apply(e: Entity): Entity = myMap(e).asInstanceOf[Entity]
  def apply(et: EntityType): Vector[Entity] = topEntitiesOfType(et) 
  def apply(h: Head): Model =  myMap(h).asInstanceOf[Model]

  def get[T](at: AttributeType[T]): Option[T] = if (isDefinedAt(at)) Some(apply(at)) else None 
  def get(e: Entity): Option[Entity] = if (isDefinedAt(e)) Some(e) else None 
  def get(et: EntityType): Vector[Entity] = topEntitiesOfType(et)
    //try { topEntitiesOfType(et) } catch { case e: NoSuchElementException => Vector() } 
  def get(h: Head): Option[Model] = if (isDefinedAt(h)) Some(apply(h)) else None 

  def isDefinedAt(a: Attribute): Boolean = isDefinedAt(a.myType) && (apply(a.myType) == a)
  def isDefinedAt(et: EntityType): Boolean = !topEntitiesOfType(et).isEmpty
  
  def existsElem(p: Elem => Boolean): Boolean = exists((kn: (Key, Node) )=> p(keyNodeToElem(kn)))
  
  def visitAll[T](f: PartialFunction[Elem,T]): Vector[T] = elems.flatMap ( e => 
    e match {
      case leaf: Leaf if f.isDefinedAt(e) => Vector(f(e))
      case rel: Relation => 
        ( if (f.isDefinedAt(rel.source)) Vector(f(rel.source)) else Vector[T]() ) ++
          ( if (f.isDefinedAt(rel)) Vector(f(rel)) else Vector[T]() ) ++  
            ( rel.submodel.visitAll(f) )
      case _ => Vector()
    }
  )
  
  def bfs[T](f: PartialFunction[Elem,T]): Vector[T] = 
    topElems.collect(f) ++ topRelations.map(_.submodel.bfs(f)).flatten

  def dfs[T](f: PartialFunction[Elem,T]): Vector[T] = 
    topRelations.map(_.submodel.dfs(f)).flatten ++ topElems.collect(f)
    
  def filter(p: Elem => Boolean): Model = filter(keyNodeToElem _ andThen p )
  def filterNot(p: Elem => Boolean): Model = filterNot(keyNodeToElem _ andThen p )
    
  def filterDeep(p: Elem => Boolean): Model = flatMap {
    case kn if p(keyNodeToElem(kn)) => keyNodeToElem(kn) match {
      case Relation(s,l,tail) =>  
        if (p(s) || tail.existsElem(p)) 
          Vector(elemToKeyNode(Relation(s, l, tail.filterDeep(p))))
        else 
          Vector() 
      case _ => Vector(kn)
    }
    case _ => Vector()
  }

  def filterDeepNot(p: Elem => Boolean): Model = flatMap {
    case kn if p(keyNodeToElem(kn)) => keyNodeToElem(kn) match {
      case Relation(s,l,tail) =>  
        if (p(s) || tail.existsElem(p)) 
          Vector(elemToKeyNode(Relation(s, l, tail.filterDeepNot(p))))
        else 
          Vector() 
      case _ => Vector(kn)
    }
    case _ => Vector()
  }
  
  
  def filterDeep2(p: Elem => Boolean): Model = flatMap {
    case kn if p(keyNodeToElem(kn)) => keyNodeToElem(kn) match {
      case Relation(s,l,tail) => 
        Some(elemToKeyNode(Relation(s, l, tail.filterDeep2(p))))
      case _ => Some(kn)
    }
    case _ => None
  }

  def filterDeepNot2(p: Elem => Boolean): Model = flatMap {
    case kn if !p(keyNodeToElem(kn)) => keyNodeToElem(kn) match {
      case Relation(s,l,tail) => 
        Some(elemToKeyNode(Relation(s, l, tail.filterDeepNot2(p))))
      case _ => Some(kn)
    }
    case _ => None
  }

  def tailsExists(p: Elem => Boolean): Model = flatMap {
    case kn => keyNodeToElem(kn) match {
      case Relation(s,l,tail) if tail.existsElem(p) => Some(kn)
      case _ => None
    }
  }

  def tailsExistsNot(p: Elem => Boolean): Model = flatMap {
    case kn => keyNodeToElem(kn) match {
      case Relation(s,l,tail) if !tail.existsElem(p) => Some(kn)
      case _ => None
    }
  }    
  
  def filterTails(p: Elem => Boolean): Model = flatMap {
    case kn => keyNodeToElem(kn) match {
      case Relation(s,l,tail) => 
        Some(elemToKeyNode(Relation(s, l, tail.filter(p))))
      case _ => Some(kn)
    }
  }
  
  def filterTailsNot(p: Elem => Boolean): Model = flatMap {
    case kn => keyNodeToElem(kn) match {
      case Relation(s,l,tail) => 
        Some(elemToKeyNode(Relation(s, l, tail.filterNot(p))))
      case _ => Some(kn)
    }
  }
  
  def separateKeysOrTails(base: Selector, separator: (Elem => Boolean) => Model): Model = 
    base match {
      case et: EntityType => separator { 
        case e: Entity if e.myType == et => true 
        case Relation(s, _, t) if s.myType == et || t.isDefinedAt(et) => true
        case _ => false
      }
      case at: AttributeType[_] => separator { 
        case a: Attribute if a.myType == at => true 
        case Relation(_, _, tail) if tail.isDefinedAt(at) => true
        case _ => false
      }
      case _ => throw new scala.NotImplementedError("separate base=" + base)
    }

  def separateKeys(base: Selector, separator: (Elem => Boolean) => Model): Model = 
    base match {
      case et: EntityType => separator { 
        case e: Entity if e.myType == et => true 
        case Relation(s, _, _) if s.myType == et => true
        case _ => false
      }
      case at: AttributeType[_] => separator { 
        case a: Attribute if a.myType == at => true 
        case _ => false
      }
      case _ => throw new scala.NotImplementedError("separateKeys base=" + base)
    }      

  def separateTails(base: Selector, separator: (Elem => Boolean) => Model): Model = 
    base match {
      case et: EntityType => separator { 
        case Relation(_, _, tail) if tail.isDefinedAt(et) => true
        case _ => false
      }
      case at: AttributeType[_] => separator { 
        case Relation(_, _, tail) if tail.isDefinedAt(at) => true
        case _ => false
      }
      case _ => throw new scala.NotImplementedError("separateTails base=" + base)
    }       
    
  def enter(e: Entity): Model = get(e.has).getOrElse(Model())
  def enter(h: Head): Model = get(h).getOrElse(Model())
  def enter[T](at: AttributeType[T]): T = get(at).getOrElse(at.default.asInstanceOf[T])

  def /(e: Entity): Model = enter(e)
  def /(h: Head): Model = enter(h)
  def /[T](at: AttributeType[T]): T = enter(at)
  
  def restrict(s: Selector): Model = separateKeysOrTails(s, filter)    
  def *(s: Selector): Model = restrict(s)
  
  def restrictTop(s: Selector): Model = separateKeys(s, filter)    
  def *^(s: Selector): Model = restrictTop(s)
  
  def restrictTails(s: Selector): Model = separateTails(s, filter)   
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
    else if (size == 1) elems.mkString 
    else elems.mkString("(", ", ", ")")
  override def toString = stringPrefix + elems.mkString("(\n", ",\n", ")\n")
}

object Model extends Type {
  // --- integration with collections:
  import scala.collection.mutable.{Builder, MapBuilder}
  import scala.collection.generic.CanBuildFrom

  def empty: Model = new Model(LinkedHashMap.empty)  
  def newBuilder: Builder[(Key, Node), Model] = new MapBuilder[Key, Node, Model](empty)
  implicit def canBuildFrom: CanBuildFrom[Model, (Key, Node), Model] =
    new CanBuildFrom[Model, (Key, Node), Model] {
      def apply(from: Model): Builder[(Key, Node), Model] = newBuilder 
      def apply(): Builder[(Key, Node), Model] = newBuilder
    }
    
  // --- reqT-specific members
  def newModelBuilder: Builder[Elem, Model] = new ModelBuilder(Model())  //??? is this needed
  
  implicit def canBuildFromModel: CanBuildFrom[Vector[Elem], Elem, Model] = //??? is this needed
    new CanBuildFrom[Vector[Elem], Elem, Model] {
      def apply(from: Vector[Elem]): Builder[Elem, Model] = newModelBuilder 
      def apply(): Builder[Elem, Model] = newModelBuilder
    }
  
  def elemToKeyNode(e: Elem):(Key, Node) = (e.key, e.node)

  def keyNodeToElem(kn: (Key, Node)):Elem = kn match {
    case (_, a: Attribute) => a
    case (e: Entity, _) => e
    case (head: Head, submodel: Model) => Relation(head, submodel)
    case _ => throw new IllegalArgumentException(kn + " must be (Key, Node)")
  }    
  
  def fromKeyNodes(keyNodes: (Key, Node)*): Model = {
    new Model(LinkedHashMap(keyNodes:_*))
  }  
  
  def apply(elems: Elem*): Model = {
    new Model(LinkedHashMap(elems.map(elemToKeyNode):_*))
  }
}

class ModelBuilder( private var myModel: Model) //??? is this needed
extends scala.collection.mutable.Builder[Elem, Model] {
  def +=(elem: Elem): this . type = { myModel += elem; this }
  def clear(): Unit =  { myModel = Model() } 
  def result(): Model = myModel
}





