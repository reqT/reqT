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
//!!!! beter to apply etc on key 
//reqT> m.keys.map(k => m(k))
//<console>:15: error: overloaded method value apply with alternatives:
//  (p: reqT.HeadPath)reqT.Model <and>
//!!!!!! TODO add Relation // RelationType as argument in all/some access methods??

trait ModelAccess { //isDefinedAt, apply, get, enter
  self: Model =>

  def isDefinedAt(k: Key): Boolean = myMap.isDefinedAt(k) 
  def isDefinedAt[T](a: Attribute[T]): Boolean = isDefinedAt(a.myType) && (apply(a.myType) == a.value)
  def isDefinedAt(et: EntityType): Boolean = !topEntitiesOfType(et).isEmpty  
  def isDefinedAt(e: Entity): Boolean = isDefinedAt(e.has)  
  def isDefinedAt(r: Relation): Boolean = isDefinedAt(r.head) && apply(r.head) == r.tail
  def isDefinedAt(rt: RelationType): Boolean = !topRelationsOfType(rt).isEmpty
  def isDefinedAt(ht: HeadType): Boolean = !topHeadsOfType(ht).isEmpty

  def isDefinedAt[T](p: AttrRef[T]): Boolean = if (p.isEmpty) isDefinedAt(p.attrType) 
    else if (p.isSingle) isDefinedAt(p.head) && apply(p.head).isDefinedAt(p.attrType)
    else isDefinedAt(p.head) && apply(p.head).isDefinedAt(p.tail)

  def isDefinedAt[T](p: AttrVal[T]): Boolean = if (p.isEmpty) isDefinedAt(p.attr) 
    else if (p.isSingle) isDefinedAt(p.head) && apply(p.head).isDefinedAt(p.attr)
    else isDefinedAt(p.head) && apply(p.head).isDefinedAt(p.tail)  
  
  def isDefinedAt(p: HeadPath): Boolean = if (p.isEmpty) true 
    else if (p.isSingle) isDefinedAt(p.head) 
    else isDefinedAt(p.head) && apply(p.head).isDefinedAt(p.tail)  
  
  def apply(e: Entity): Model = apply(e.has)
  def apply(et: EntityType): Vector[Model] = {
    val topEnts = topEntitiesOfType(et)
    assert(!topEnts.isEmpty, "No top entities of type $et exists")
    topEnts.map(apply(_))
  }
  def apply(h: Head): Model =  myMap(h).asInstanceOf[Model]
  def apply[T](at: AttributeType[T]): T = myMap(at).asInstanceOf[Attribute[T]].value
  def apply[T](a: Attribute[T]): T = {
    val value = myMap(a.myType).asInstanceOf[Attribute[T]].value
    assert(value == a.value, 
      s"an attribute of type ${a.myType} is defined here, but its value $value does not match "+a.value)
    value
  }
  
  def apply[T](p: AttrRef[T]): T = if (p.isEmpty) apply(p.attrType) 
    else if (p.isSingle) apply(p.head).apply(p.attrType)
    else apply(p.head).apply(p.tail)

  def apply[T](p: AttrVal[T]): T = if (p.isEmpty) apply(p.attr) 
    else if (p.isSingle) apply(p.head).apply(p.attr)
    else apply(p.head).apply(p.tail)  
  
  def apply(p: HeadPath): Model = if (p.isEmpty) this 
    else if (p.isSingle) apply(p.head) 
    else apply(p.head).apply(p.tail)      
    
  def get(e: Entity): Option[Model] = if (isDefinedAt(e)) Some(apply(e)) else None   

  def get(h: Head): Option[Model] = myMap.get(h).asInstanceOf[Option[Model]] 
  
  def get[T](a: Attribute[T]): Option[T] = 
    if (isDefinedAt(a.myType) && (apply(a.myType) == a.value)) Some(a.value) else None 
    
  def get[T](at: AttributeType[T]): Option[T] = if (isDefinedAt(at)) Some(apply(at)) else None 
  
  def get(r: Relation): Option[Model] = if (isDefinedAt(r)) Some(apply(r.head)) else None 
  
  def get(et: EntityType): Vector[Model] = topEntitiesOfType(et).flatMap(get(_)).filterNot(_.isEmpty)

  def get(rt: RelationType): Vector[Model] = topRelationsOfType(rt).flatMap(get(_)).filterNot(_.isEmpty)
    
  def get(ht: HeadType): Vector[Model]= topHeadsOfType(ht).flatMap(get(_)).filterNot(_.isEmpty)
  
  def get[T](p: AttrRef[T]): Option[T] = if (p.isEmpty) get(p.attrType) 
    else if (!isDefinedAt(p.head)) None
    else if (p.isSingle) apply(p.head).get(p.attrType)
    else apply(p.head).get(p.tail)

  def get[T](p: AttrVal[T]): Option[T] = 
    if (p.isEmpty) get(p.attr.myType).collect { case a if a == p.attr.value => a }   
    else if (!isDefinedAt(p.head)) None
    else if (p.isSingle) apply(p.head).get(p.attr.myType)
    else apply(p.head).get(p.tail)    
  
  def get(p: HeadPath): Option[Model] = if (p.isEmpty || !isDefinedAt(p.head)) None 
    else if (p.isSingle) Some(apply(p.head))
    else apply(p.head).get(p.tail)      
    
  def enter(id: String): Model = get(topEntityOfId(id).has).getOrElse(Model())
  
  def enter(e: Entity): Model = get(e.has).getOrElse(Model())

  def enter(h: Head): Model = get(h).getOrElse(Model())
  def enter[T](at: AttributeType[T]): T = get(at).getOrElse(at.default.asInstanceOf[T])

  def enter(p: HeadPath): Model = if (p.isEmpty) this
    else if (p.isSingle) enter(p.head)
    else enter(p.head) / p.tail
    
  def enter[T](p: AttrRef[T]): T = if (p.isEmpty) this / p.attrType
    else if (p.isSingle) enter(p.head) / p.attrType
    else enter(p.head) / p.tail
  
  def /(e: Entity): Model = enter(e)
  def /(id: String): Model = enter(id)
  def /(h: Head): Model = enter(h)
  def /[T](at: AttributeType[T]): T = enter(at)  
  def /(p: HeadPath): Model = enter(p) 
  def /[T](p: AttrRef[T]): T = enter(p)
}
