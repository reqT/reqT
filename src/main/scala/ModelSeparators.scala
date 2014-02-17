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


trait ModelSeparators {
  self: Model =>
  
  def separateKeysOrTails(s: Selector, separator: (Elem => Boolean) => Model): Model = 
    s match {
      case e: Entity => separator { 
        case e2: Entity if e == e2 => true 
        case Relation(e2, _, tail) if e == e2 || tail.isDefinedAt(e) => true
        case _ => false
      }
      case StringSelector(id) => separator {
        case e2: Entity if tipEntitiesOfId(id).contains(e2) => true
        case Relation(e2, _, tail) if tipEntitiesOfId(id).contains(e2) || tail.isDefinedAt(id) => true
        case _ => false
      }
      case et: EntityType => separator { 
        case e: Entity if e.myType == et => true 
        case Relation(s, _, tail) if s.myType == et || tail.isDefinedAt(et) => true
        case _ => false
      }
      case at: AttributeType[_] => separator { 
        case a: Attribute[_] if a.myType == at => true 
        case Relation(_, _, tail) if tail.isDefinedAt(at) => true
        case _ => false
      }
      case rt: RelationType => separator { 
        case Relation(_, link, tail) if link == rt || tail.isDefinedAt(rt) => true
        case _ => false
      }
      case _ => throw new scala.NotImplementedError("separateKeysOrTails TODO: " + s)
    }

  def separateKeys(s: Selector, separator: (Elem => Boolean) => Model): Model = 
    s match {
      case e: Entity => separator {
        case e2: Entity if e == e2 => true
        case Relation(e2, _, _) if e == e2 => true
        case _ => false
      }
      case StringSelector(id) => separator {
        case e2: Entity if tipEntitiesOfId(id).contains(e2) => true
        case Relation(e2, _, _) if tipEntitiesOfId(id).contains(e2) => true
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
      case rt: RelationType => separator { 
        case Relation(_, link, _) if link == rt => true
        case _ => false
      }
      case _ => throw new scala.NotImplementedError("separateKeys TODO: " + s)
    }      

  def separateTails(s: Selector, separator: (Elem => Boolean) => Model): Model = 
    s match {
      case e: Entity => separator { 
        case Relation(_, _, tail) if tail.isDefinedAt(e) => true
        case _ => false
      }
      case StringSelector(id) => separator {
        case Relation(_, _, tail) if tail.isDefinedAt(id) => true
        case _ => false
      }
      case et: EntityType => separator { 
        case Relation(_, _, tail) if tail.isDefinedAt(et) => true
        case _ => false
      }
      case at: AttributeType[_] => separator { 
        case Relation(_, _, tail) if tail.isDefinedAt(at) => true
        case _ => false
      }
      case rt: RelationType => separator { 
        case Relation(_, _, tail) if tail.isDefinedAt(rt) => true
        case _ => false
      }      
      case _ => throw new scala.NotImplementedError("separateTails TODO:" + s)
    }       
    
  def restrict(s: Selector): Model = separateKeysOrTails(s, filter)    
  def *(s: Selector): Model = restrict(s)
  
  def restrictTop(s: Selector): Model = separateKeys(s, filter)    
  def *^(s: Selector): Model = restrictTop(s)
  
  def restrictTails(s: Selector): Model = separateTails(s, filter)   
  def *~(s: Selector): Model = restrictTails(s)

  def restrictAll(s: Selector): Model = separateKeysOrTails(s, filterDeep)    
  def **(s: Selector): Model = restrictAll(s)
  
  def restrictNot(s: Selector): Model = separateKeysOrTails(s, filterNot)    
  def *!(s: Selector): Model = restrictNot(s)

  def restrictTopNot(s: Selector): Model = separateKeys(s, filterNot)
  def *^!(s: Selector): Model = restrictTopNot(s)
  
  def restrictTailNot(s: Selector): Model = separateTails(s, filterNot)
  def *~!(s: Selector): Model = restrictTailNot(s)

  def restrictAllNot(s: Selector): Model = separateKeysOrTails(s, filterDeepNot)    
  def **!(s: Selector): Model = restrictAllNot(s)
} 