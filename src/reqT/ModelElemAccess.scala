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

trait ModelElemAccess { //isDefinedAt, access, get, enter
  self: Model =>

  def isDefinedAt(k: Key): Boolean = myMap.isDefinedAt(k) 
  def isDefinedAt[T](a: Attribute[T]): Boolean = isDefinedAt(a.myType) && (access(a.myType) == a.value)
  def isDefinedAt(et: EntityType): Boolean = !tipEntitiesOfType(et).isEmpty  
  def isDefinedAt(e: Entity): Boolean = isDefinedAt(e.has)  
  def isDefinedAt(id: String): Boolean = !tipEntitiesOfId(id).isEmpty  
  def isDefinedAt(r: Relation): Boolean = isDefinedAt(r.head) && access(r.head) == r.tail
  def isDefinedAt(rt: RelationType): Boolean = !topRelationsOfType(rt).isEmpty
  def isDefinedAt(ht: HeadType): Boolean = !topHeadsOfType(ht).isEmpty

  def isDefinedAt[T](path: Path): Boolean = path match {
    case np: NodePath => np match {
      case p: HeadPath => 
        if (p.isEmpty) true 
        else if (p.isSingle) isDefinedAt(p.head) 
        else isDefinedAt(p.head) && access(p.head).isDefinedAt(p.tail)  
      case p: AttrVal[_] => 
        if (p.isEmpty) isDefinedAt(p.attr) 
        else if (p.isSingle) isDefinedAt(p.head) && access(p.head).isDefinedAt(p.attr)
        else isDefinedAt(p.head) && access(p.head).isDefinedAt(p.tail)  
    }
    case p: AttrRef[_] =>
      if (p.isEmpty) isDefinedAt(p.attrType) 
      else if (p.isSingle) isDefinedAt(p.head) && access(p.head).isDefinedAt(p.attrType)
      else isDefinedAt(p.head) && access(p.head).isDefinedAt(p.tail)
  }
  
   
  def access(e: Entity): Model = access(e.has)
  def access(id: String): Model = access(tipEntityOfId(id).has)
  def access(et: EntityType): Vector[Model] = {
    val topEnts = tipEntitiesOfType(et)
    assert(!topEnts.isEmpty, "No top entities of type $et exists")
    topEnts.map(access(_))
  }
  def access(h: Head): Model =  myMap(h).asInstanceOf[Model]
  def access[T](at: AttributeType[T]): T = myMap(at).asInstanceOf[Attribute[T]].value
  def access[T](a: Attribute[T]): T = {
    val value = myMap(a.myType).asInstanceOf[Attribute[T]].value
    assert(value == a.value, 
      s"an attribute of type ${a.myType} is defined here, but its value $value does not match "+a.value)
    value
  }
  
  def access[T](p: AttrRef[T]): T = if (p.isEmpty) access(p.attrType) 
    else if (p.isSingle) access(p.head).access(p.attrType)
    else access(p.head).access(p.tail)

  def access[T](p: AttrVal[T]): T = if (p.isEmpty) access(p.attr) 
    else if (p.isSingle) access(p.head).access(p.attr)
    else access(p.head).access(p.tail)  
  
  def access(p: HeadPath): Model = if (p.isEmpty) this 
    else if (p.isSingle) access(p.head) 
    else access(p.head).access(p.tail) 
    
  def get(e: Entity): Option[Model] = if (isDefinedAt(e)) Some(access(e)) else None   
  def get(id: String): Vector[Model] = tipEntitiesOfId(id).toVector.flatMap(get(_)).filterNot(_.isEmpty)   

  def get(h: Head): Option[Model] = myMap.get(h).asInstanceOf[Option[Model]] 
  
  def get[T](a: Attribute[T]): Option[T] = 
    if (isDefinedAt(a.myType) && (access(a.myType) == a.value)) Some(a.value) else None 
    
  def get[T](at: AttributeType[T]): Option[T] = if (isDefinedAt(at)) Some(access(at)) else None 
  
  def get(r: Relation): Option[Model] = if (isDefinedAt(r)) Some(access(r.head)) else None 
  
  def get(et: EntityType): Vector[Model] = tipEntitiesOfType(et).flatMap(get(_)).filterNot(_.isEmpty)

  def get(rt: RelationType): Vector[Model] = topRelationsOfType(rt).flatMap(get(_)).filterNot(_.isEmpty)
    
  def get(ht: HeadType): Vector[Model]= topHeadsOfType(ht).flatMap(get(_)).filterNot(_.isEmpty)
  
  def get[T](p: AttrRef[T]): Option[T] = if (p.isEmpty) get(p.attrType) 
    else if (!isDefinedAt(p.head)) None
    else if (p.isSingle) access(p.head).get(p.attrType)
    else access(p.head).get(p.tail)

  def get[T](p: AttrVal[T]): Option[T] = 
    if (p.isEmpty) get(p.attr.myType).collect { case a if a == p.attr.value => a }   
    else if (!isDefinedAt(p.head)) None
    else if (p.isSingle) access(p.head).get(p.attr.myType)
    else access(p.head).get(p.tail)    
  
  def get(p: HeadPath): Option[Model] = if (p.isEmpty || !isDefinedAt(p.head)) None 
    else if (p.isSingle) Some(access(p.head))
    else access(p.head).get(p.tail)      
    
  def enter(id: String): Model = get(tipEntityOfId(id).has).getOrElse(Model())
  
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
