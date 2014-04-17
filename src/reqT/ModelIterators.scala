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

import scala.collection.generic.FilterMonadic


trait ModelIterators extends ModelBase {
  self: Model =>
  
  import Model.{fromMap => newModel}
  
  def mapLeafPaths[T](f: Path => T): Vector[T] = {
    def iter(m: Model, p: HeadPath): Vector[T] = m.myMap.toVector.flatMap {
      case (at: AttributeType[_], a: Attribute[_]) => Vector(f(p/a))
      case (h: Head, tail: Model) if tail.isEmpty => Vector(f(p / h))
      case (h: Head, tail: Model) => iter(tail, p / h)
      case _ => Vector()
    }
    iter(this, HeadPath())
  }

  def collectLeafPaths[T](f: PartialFunction[Path,T]): Vector[T] = {
    def iter(m: Model, p: HeadPath): Vector[T] = m.myMap.toVector.flatMap {
      case (at: AttributeType[_], a: Attribute[_]) if f.isDefinedAt(p/a) => Vector(f(p/a))
      case (h: Head, tail: Model) if tail.isEmpty && f.isDefinedAt(p/h) => Vector(f(p / h))
      case (h: Head, tail: Model) => iter(tail, p/h)
      case _ => Vector()
    }
    iter(this, HeadPath())
  }
  
  def collectLeafs[T](f: PartialFunction[Elem,T]): Vector[T] = {
    def iter(m: Model, p: HeadPath): Vector[T] = m.myMap.toVector.flatMap {
      case (at: AttributeType[_], a: Attribute[_]) if f.isDefinedAt(a) => Vector(f(a))
      case km@(h: Head, tail: Model) if tail.isEmpty && f.isDefinedAt(km.toElem) => Vector(f(km.toElem))
      case km@(h: Head, tail: Model) => iter(tail, p/h)
      case _ => Vector()
    }
    iter(this, HeadPath())
  }
  
  //def collect[T](f: PartialFunction[Elem,T]): Vector[T] = elems.collect(f) 
  def collectDeep[T](f: PartialFunction[Elem,T]): Vector[T] = elems.flatMap ( e =>   //???
    e match {
      case n: Node if f.isDefinedAt(e) => Vector(f(e))
      case rel: Relation => 
        ( if (f.isDefinedAt(rel.entity)) Vector(f(rel.entity)) else Vector[T]() ) ++
          ( if (f.isDefinedAt(rel)) Vector(f(rel)) else Vector[T]() ) ++  
            ( rel.tail.collectDeep(f) )
      case _ => Vector()
    }
  )
  
  // def collectElems[T](f: PartialFunction[Elem,T]): Vector[T] = elems.flatMap ( e =>   //???
    // e match {
      // case n: Node if f.isDefinedAt(e) => Vector(f(e))
      // case rel: Relation if f.isDefinedAt(rel) => Vector(f(rel)) ++ rel.tail.collectElems(f) 
      // case _ => Vector()
    // }
  // )
  
  def foreach[U](f: Elem => U): Unit = toIterable.foreach(f)
  def foreach(block: => Unit): Unit = foreach(_ => block)
  def filter(f: Elem => Boolean): Model = toIterable.filter(f).toModel
  def filterNot(f: Elem => Boolean): Model = toIterable.filterNot(f).toModel
  def withFilter(f: Elem => Boolean): FilterMonadic[reqT.Elem,Iterable[reqT.Elem]] = 
    toIterable.withFilter(f) //needed to make for-comprehensions work over Model
  def map[U](f: Elem => U): Iterable[U] = toIterable.map(f)

  def filterDeep(p: Elem => Boolean): Model = newModel( myMap.flatMap {
    case km if p(km.toElem) => km match {
      case (h: Head,tail: Model) =>  Some((h, tail.filterDeep(p)))
      case _ => Some(km)
    }
    case _ => None
  } )

  def filterDeepNot(p: Elem => Boolean): Model = newModel( myMap.flatMap {
    case km if !p(km.toElem) => km match {
      case (h: Head,tail: Model) =>  Some((h, tail.filterDeepNot(p)))
      case _ => Some(km)
    }
    case _ => None
  } )  
  
  def flatMapDeep[T](f: Elem => Option[Elem]): Model = newModel( //???
    myMap.flatMap { km =>
      km match {
        case (h: Head,tail: Model) =>  f((h, tail.flatMapDeep(f)).toElem).map(_.toMapping)
        case (at: AttributeType[_], a: Attribute[_]) => f((at,a).toElem).map(_.toMapping)
        case _ => None
      }
    } 
  )
  
  
  def mapDeep[U](f: Elem => U): Vector[U] = elems.flatMap { e =>  //???
    e match {
      case n: Node     => Vector(f(n))
      case r: Relation => Vector(f(r)) ++ r.tail.mapDeep(f)      
      case NoElem => Vector()       
    }
  }
  
  def foreachDeep[U](f: Elem => U): Unit = elems.foreach { e => 
    e match {
      case n: Node     => f(n)
      case r: Relation => f(r); r.tail.foreachDeep(f)      
      case NoElem =>       
    }
  }
  def foreachDeep(block: => Unit): Unit = foreachDeep { _ => block }
  
  def foreachNodeDeep[U](f: Node => U): Unit = elems.foreach { e => 
    e match {
      case n: Node     => f(n)
      case r: Relation => f(r.entity); r.tail.foreachNodeDeep(f) 
      case NoElem =>
    }
  }
  def foreachNodeDeep(block: => Unit): Unit = foreachNodeDeep { _ => block }  
  

  
  def bfs[T](f: PartialFunction[Elem,T]): Vector[T] =  //funkar ??? , eller missar sources ???
    elemsWithTip.collect(f) ++ topRelations.map(r => r.tail.bfs(f)).flatten

  def dfs[T](f: PartialFunction[Elem,T]): Vector[T] = //funkar ??? , eller missar sources ???
    topRelations.map(r => r.tail.dfs(f)).flatten ++ elemsWithTip.collect(f)

  def mapModelDeep(f: Model => Model): Model = { //????
    val m = myMap.map { 
      case (h,tail: Model) => (h, f(tail.mapModelDeep(f))) 
      case any => any } .toModel
    f(m)
  }
  
  def mapModelTopTails(f: Model => Model): Model = 
    myMap.map { case (h,tail: Model) => (h, f(tail)) ; case any => any } .toModel
    
  def mapModelTop(f: Model => Model): Model = {
    val m = myMap.map { case (h,tail: Model) => (h, f(tail)) ; case any => any } .toModel
    f(m)
  }
    
  def mapModelTailsDeep(f: Model => Model): Model = 
    myMap.map { case (h,tail: Model) => (h, f(tail.mapModelTailsDeep(f))) ; case any => any } .toModel
    
 
}