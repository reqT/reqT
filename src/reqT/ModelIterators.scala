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
  
  def mapLeafPaths[T](f: NodePath => T): Vector[T] = {
    def iter(m: Model, p: HeadPath): Vector[T] = m.myMap.toVector.flatMap {
      case (at: AttributeType[_], a: Attribute[_]) => Vector(f(p/a))
      case (h: Head, tail: Model) if tail.isEmpty => Vector(f(p / h))
      case (h: Head, tail: Model) => iter(tail, p / h)
      case _ => Vector()
    }
    iter(this, HeadPath())
  }

  def collectLeafPaths[T](f: PartialFunction[NodePath,T]): Vector[T] = {
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
  
  def collect[T](f: PartialFunction[Elem,T]): Vector[T] = elems.flatMap ( e =>   // ???
    e match {
      case n: Node if f.isDefinedAt(e) => Vector(f(e))
      case rel: Relation => 
        ( if (f.isDefinedAt(rel.entity)) Vector(f(rel.entity)) else Vector[T]() ) ++
        ( if (f.isDefinedAt(rel)) Vector(f(rel)) else Vector[T]() ) ++  
        ( rel.tail.collect(f) )  // ???
      case _ => Vector()  }  )
      
  def map[T](f: Elem => T): Vector[T] = collect { case e => f(e) } 
  def foreach(f: Elem => Unit): Unit = map(f)

 /* def map[U](f: Elem => U): Vector[U] = elems.flatMap { e =>  //???
    e match {
      case n: Node     => Vector(f(n))
      case r@Relation(e,l,t) => Vector(f(e)) ++ Vector(f(r)) ++ t.map(f)      
      case _ => Vector()       
    }
  } 
  
  def map(f: Elem => Elem): Model = elems.map { e =>  //???
    e match {
      case n: Node     => Model(f(n))
      case Relation(e,l,t) => f(e) match {
         case ent: Entity => Model(ent) ++ Model(f(Relation(ent,l,t.map(f))))
         case _ => Model(f(e)) ++ Model(f(Relation(e,l,t.map(f))))        
      }      
      case _ => Model()       
    }
  } .foldLeft(Model())(_ ++ _)
  
  def foreach[T](f: Elem => Unit): Unit =  elems.foreach { e =>  //???
    e match {
      case n: Node     => f(n)
      case Relation(e,l,t) => 
        f(e)
        t.foreach(f)
        f(Relation(e,l,t))
      case _ =>        
    }
  } */

  def transform(f: PartialFunction[Elem,Elem]): Model = elems.map { e =>  //???
    e match {
      case n: Node if f.isDefinedAt(n) => Model(f(n))
      case r@Relation(e,l,t) if f.isDefinedAt(e) && f.isDefinedAt(r) => f(e) match {
         case ent: Entity if ent != e => Model(f(Relation(ent,l,t.transform(f)))) ++ Model(ent) //is this the order what we want???
         case ent: Entity if ent == e => Model(f(Relation(ent,l,t.transform(f)))) //to make transform{case e=>e} not inflate the model
         case _ => Model(f(Relation(e,l,t.transform(f)))) ++ Model(f(e)) //is this the order what we want???       
      }      
      case r@Relation(e,l,t) if f.isDefinedAt(e) => f(e) match {
         case ent: Entity if ent != e => Model(Relation(ent,l,t.transform(f))) ++ Model(ent) //is this the order what we want???     
         case ent: Entity if ent == e => Model(Relation(ent,l,t.transform(f))) //to make transform{case e=>e}  transform not inflate the model     
         case _ =>  Model(Relation(e,l,t.transform(f))) ++ Model(f(e)) //is this the order what we want???     
      } 
      case r@Relation(e,l,t) if f.isDefinedAt(r) => Model(f(Relation(e,l,t.transform(f))))         
      case Relation(e,l,t) => Model(Relation(e,l,t.transform(f)))         
      case _ => Model(e)       
    }
  } .foldLeft(Model())(_ ++ _)  
  
  def transformEntity(f: PartialFunction[Entity, Entity]): Model = elems.map { elem => 
    elem match {
      case e : Entity if f.isDefinedAt(e) => f(e)
      case Relation(e,l,t) if f.isDefinedAt(e) => Relation(f(e),l,t.transformEntity(f))
      case Relation(e,l,t) => Relation(e,l,t.transformEntity(f))
      case _ => elem      
    }
  } .toModel

  def transformLeafEntity(f: PartialFunction[Entity, Elem]): Model = elems.map { elem => 
    elem match {
      case e : Entity if f.isDefinedAt(e) => f(e)
      case Relation(e,l,t) => Relation(e,l,t.transformLeafEntity(f))
      case _ => elem      
    }
  } .toModel
  
  def transformAttribute(f: PartialFunction[Attribute[_],Elem]): Model = elems.map { elem => 
    elem match {
      case a : Attribute[_] if f.isDefinedAt(a) => f(a)
      case Relation(e,l,t) => Relation(e,l,t.transformAttribute(f))
      case _ => elem      
    }
  } .toModel  

  def transformRelation(f: PartialFunction[Relation,Elem]): Model = elems.map { elem => 
    elem match {
      case r@Relation(e,l,t) if f.isDefinedAt(r) => f(Relation(e,l,t.transformRelation(f)))
      case Relation(e,l,t) => Relation(e,l,t.transformRelation(f))
      case _ => elem      
    }
  } .toModel  
  
  def withFilter(f: Elem => Boolean): FilterMonadic[reqT.Elem,Iterable[reqT.Elem]] = 
    toIterable.withFilter(f) //needed to make for-comprehensions work over Model //too shallow??

  //def map[U](f: Elem => U): Vector[U] = elems.map(f)  //better clients use: m.toVector.map(...)

  def filter(p: Elem => Boolean): Model = elems.flatMap ( e =>   //???
    e match {
      case n: Node if p(e) => Vector(e)
      case rel: Relation => 
        lazy val ftail = rel.tail.filter(p)
        if (p(rel.entity)||p(rel)|| !ftail.isEmpty) Vector(Relation(rel.head, ftail)) 
        else Vector()  
      case _ => Vector()
    } ).toModel
    
  def filterNot(p: Elem => Boolean): Model = filter(e => !p(e)) //???
  
  /* old filterDeep newModel( myMap.flatMap {  //???
    case km if p(km.toElem) => km match {
      case (h: Head,tail: Model) =>  Some((h, tail.filterDeep(p)))
      case _ => Some(km)
    }
    case _ => None
  } ) */

  /*def filterDeepNot(p: Elem => Boolean): Model = newModel( myMap.flatMap { //???
    case km if !p(km.toElem) => km match {
      case (h: Head,tail: Model) =>  Some((h, tail.filterDeepNot(p)))
      case _ => Some(km)
    }
    case _ => None
  } )*/  
  
  def flatMapDeep[T](f: Elem => Option[Elem]): Model = newModel( //???
    myMap.flatMap { km =>
      km match {
        case (h: Head,tail: Model) =>  f((h, tail.flatMapDeep(f)).toElem).map(_.toMapping)
        case (at: AttributeType[_], a: Attribute[_]) => f((at,a).toElem).map(_.toMapping)
        case _ => None
      }
    } 
  )
  
/*  def mapDeep[U](f: Elem => U): Vector[U] = elems.flatMap { e =>  //???
    e match {
      case n: Node     => Vector(f(n))
      case r: Relation => Vector(f(r)) ++ r.tail.mapDeep(f)      
      case NoElem => Vector()       
    }
  }*/



  
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