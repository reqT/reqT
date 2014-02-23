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

import scala.language.{implicitConversions, postfixOps}

trait ModelBasicOps  {
  self: Model =>

  import Model.{fromMap => newModel}
  import BagUtils._
  
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
  def -(e: Entity): Model = newModel(myMap - e.has)
  def -(id: String): Model = newModel(myMap - tipEntityOfId(id).has)
  def diffKeys(that: Model): Model = newModel(myMap -- that.keys) //should not this be deep????
  def diff(that: Model): Model = (elems diff that.elems).toModel //should not this be deep????
  def --(that: Model): Model = diff(that)  //is diff == diffKeys ??? NO!?
  
//size calculations:
  val mapSize: Int = myMap.size
  lazy val topSize: Int = top.size
  lazy val size: Int = { var n = 0 ; foreachDeep { n += 1 } ; n }
  lazy val nodeSize: Int = { var n = 0 ; foreachNodeDeep { n += 1 } ; n }
  val isEmpty: Boolean = mapSize == 0
  lazy val isDeep: Boolean = tip != top


//collection of elements:  
  lazy val keys: Iterable[Key] = myMap.keys
  lazy val keySet: Set[Key] = myMap.keySet
  lazy val keyVector: Vector[Key] = myMap.collect { case (k,v) => k } .toVector
  
  // the vectors allows for traversal in elems order if ListModel and they are as type specific as possible which is not a problem as Vector is covariant
  // the sets allow for fast contains tests but are all of Set[Elem] as Sets are invariant 

  lazy val tipNodes: Vector[Node] = elems.collect { 
    case Relation(e, l, tail) => e
    case n: Node => n
  }  
  lazy val tipNodeSet: Set[Elem] = tipNodes.toSet

  lazy val top: Model = elems.collect {
    case Relation(e, l, tail) => Relation(e, l, tail.tip)
    case elem => elem
  } .toModel
  
  lazy val tip: Model = tipNodes.toModel  
  def `^`: Model = top
  def `^^`: Model = tip

  lazy val flattenDeep: Model = mapDeep( m => m).toModel.top  //????
  lazy val elemsWithTip: Vector[Elem] = elems.flatMap { //was topElems was elemsExpanded was expandTip
    case r: Relation => Vector(r.entity, r) 
    case elem => Vector(elem)
  }.distinct   
  
  lazy val elemsWithTop: Vector[Elem] = elems.flatMap { 
    case Relation(e, l, tail) => Vector(Vector(e), tail.tipEntities, Vector(Relation(e, l, tail))).flatten 
    case elem => Vector(elem)
  } .distinct  
  
  lazy val expandTop: Model = elemsWithTop.toModel
  lazy val pruneTop: Model = filterNot { 
    case e: Entity if topSourceSet(e) || topDestinationSet(e) => true
    case _ => false
  } 
  
  def reverseTop(from: RelationType, to: RelationType): Model =  elems.collect { 
    case Relation(e1, r1, tail1) if from == r1 => tail1.elems.collect {
      case Relation(e2, has, Model()) => Vector(Relation(e2, to, Model(e1)))
      case Relation(e2, r2, tail2) => 
        Vector(Relation(e2, to, Model(e1)), Relation(e2, r2, tail2))  //is this right???
      case e2: Entity => Vector(Relation(e2, to, Model(e1)))
      case a: Attribute[_] => Vector(Relation(e1, r1, Model(a))) //is this right???
    } .flatten
    case anyElem => Vector(anyElem)
  } .flatten.toModel

  def reverseTails(from: RelationType, to: RelationType): Model =  elems.collect { 
    case Relation(e, r, tail) => Relation(e, r, tail.reverseTop(from, to))
    case anyElem => anyElem
  } .toModel

  def reverse(from: RelationType, to: RelationType): Model =  elems.collect {  // ??? requires deep thinking
    case Relation(e1, r1, tail1) if from == r1 => tail1.elems.collect {
      case Relation(e2, has, Model()) => Vector(Relation(e2, to, Model(e1)))
      case Relation(e2, r2, tail2) =>
        Vector(
          Vector(Relation(e2, to, Model(e1))), //lift the reversed relation
          Vector(Model(Relation(e2, r2, tail2)).reverse(from,to).elems:_*) //recursive call
        ).flatten  //is this right  ^ ???
      case e2: Entity => Vector(Relation(e2, to, Model(e1)))
      case a: Attribute[_] => Vector(Relation(e1, r1, Model(a))) //is this right???
    } .flatten
    case Relation(e,r,tail) => Vector(Relation(e,r,tail.reverse(from, to))) //recursive call for other tails
    case anyElem => Vector(anyElem)
  } .flatten.toModel

  
//------------------ check below
  lazy val submodels: Vector[Model] = ??? //all models dfs in a flat vector 
  lazy val tails: Vector[Model] = myMap.collect { case (h: Head, tail: Model) if !tail.isEmpty => tail } .toVector
  lazy val tailsMerged: Model = tails.foldLeft(Model())(_ ++ _)
  lazy val `~`: Model = tailsMerged
  lazy val topRelations: Vector[Relation] = myMap.collect { case (h: Head, m: Model) if !m.isEmpty => Relation(h,m) } .toVector
  lazy val topRelationTypes: Vector[RelationType] = topRelations.map(_.link)
  lazy val tipEntities: Vector[Entity] = myMap.collect { case (h: Head, _) => h.entity } .toVector .distinct
  lazy val tipEntitySet: Set[Elem] = tipEntities.toSet
  lazy val topSources: Vector[Entity] =  myMap.collect { case (h: Head, m: Model) if !m.isEmpty=> h.entity } .toVector .distinct
  lazy val topSourceSet: Set[Entity] = topSources.toSet
  lazy val topDestinations: Vector[Entity] =  
    myMap.collect { case (h: Head, m: Model) if !m.isEmpty=> m.tipEntities } .toVector .flatten .distinct
  lazy val topDestinationSet: Set[Entity] = topDestinations.toSet
  lazy val topHeads: Vector[Head] = myMap.keys.collect { case h: Head => h } .toVector
  lazy val topNodesAndHeads: Vector[Node] = elems.collect { 
    case n: Node => n 
    case r: Relation => r.entity      
  } .distinct
  lazy val tipAttributes: Vector[Attribute[_]] = elems.collect { case a: Attribute[_] => a }
  lazy val tipAttributeSet: Set[Attribute[_]] = tipAttributes.toSet
  lazy val tipEntitiesOfType: Map[EntityType, Vector[Entity]] = 
    Bag(tipEntities.map( e => (e.myType, e)):_*).withDefaultValue(Vector())
  lazy val topRelationsOfType: Map[RelationType, Vector[Relation]] =
    Bag(topRelations.map(r => (r.link, r)) :_*).withDefaultValue(Vector())
  lazy val topHeadsOfType: Map[HeadType, Vector[Head]] =
    Bag(topHeads.map( h => (HeadType(h.entity.myType, h.link), h)):_*).withDefaultValue(Vector())

  lazy val topIds: Vector[String] = top.ids
  lazy val tipIds: Vector[String] = tip.ids
  lazy val tipEntityOfId: Map[String, Entity] = tipEntities.map(e => (e.id, e)).toMap.withDefaultValue(NoEntity)
  lazy val tipEntitiesOfId: Map[String, Set[Entity]] = SetBag(tipEntities.map(e => (e.id, e)):_*).withDefaultValue(Set())
  
  lazy val ids: Vector[String] = collectDeep { case e: Entity => e.id } .distinct
  lazy val entitiesOfId: Map[String, Set[Entity]] = 
    SetBag(collectDeep { case e: Entity => (e.id, e) } :_*).withDefaultValue(Set())
  lazy val entityOfId: Map[String, Entity] = 
    entitiesOfId.collect { case (id, es) if !es.isEmpty => (id, es.head) } .toMap.withDefaultValue(NoEntity)
  
  def existsElem(p: Elem => Boolean): Boolean = myMap.exists((kc: (Key, MapTo) )=> p(kc.toElem)) //??? rename to exists? Deep???
  
  lazy val entityAttributePairs: Vector[(Entity, Attribute[_])] = collectLeafPaths { 
    case AttrVal(p,a) => (p.heads.lastOption.getOrElse(reqT./), a)
  } .collect { case (Head(e,l),a) if l == has => (e,a) }
  lazy val entHas: Map[Entity,Attribute[_]] = entityAttributePairs.toMap
  
  lazy val headAttributePairs: Vector[(Head, Attribute[_])] = collectLeafPaths { 
    case AttrVal(p,a) => (p.heads.lastOption.getOrElse(reqT./), a)
  } .collect { case (h: Head,a) => (h,a) }
  lazy val attrOf: Map[Head,Attribute[_]] = headAttributePairs.toMap
}
