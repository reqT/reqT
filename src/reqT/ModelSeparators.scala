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

trait ModelSeparators {
  self: Model =>
  
  def count(s: Selector): Int = elems. map {
    case e if s.selects(e) => 1
    case Relation(e, l, t) if s.selects(e) || s == l || s == Head(e,l) => 1
    case Relation(e, l, t) => t.count(s) 
    case _ => 0
  } .sum 
  
  def contains(s: Selector): Boolean = elems. map {
    case e if s.selects(e) =>  true 
    case Relation(e, l, t) if s.selects(e) || s == l || s == Head(e,l) =>  true 
    case Relation(e, l, t) =>  t.contains(s)
    case _ =>  false 
  } .contains(true)
  
  def containsTipHeads(s: Selector): Boolean = elems. map {
    case e: Entity if s.selects(e) =>  true 
    case a: Attribute[_] if s.selects(a) =>  true 
    case Relation(e, l, t) if s.selects(e) || s == l || s == Head(e,l) => true 
    case r: Relation if s.isInstanceOf[HeadType] && s.selects(r) =>  true 
    case _ =>  false 
  } .contains(true)

  def restrict(s: Selector): Model = elems.filter(e => Model(e).contains(s)).toModel
  def *(s: Selector): Model = restrict(s)
  
  def restrictNot(s: Selector): Model = elems.filter(e => !Model(e).contains(s)).toModel
  def *!(s: Selector): Model = restrictNot(s)

  def restrictTipHeads(s: Selector): Model = elems.filter(e => Model(e).containsTipHeads(s)).toModel    
  def *^(s: Selector): Model = restrictTipHeads(s)
  
  //def restrict(s: Selector): Model = separateKeysOrTails(s, filter)    
  //def *(s: Selector): Model = restrict(s)
  
  //def restrictTop(s: Selector): Model = separateKeys(s, filter)    
  //def *^(s: Selector): Model = restrictTop(s)
  
  //def restrictTails(s: Selector): Model = separateTails(s, filter)   
  def *~(s: Selector): Model = ???

  //def restrictAll(s: Selector): Model = separateKeysOrTails(s, filterDeep)    
  //def **(s: Selector): Model = restrictAll(s)
  
  //def restrictNot(s: Selector): Model = separateKeysOrTails(s, filterNot)    
  //def *!(s: Selector): Model = restrictNot(s)

  //def restrictTopNot(s: Selector): Model = separateKeys(s, filterNot)
  //def *^!(s: Selector): Model = restrictTopNot(s)
  
  //def restrictTailNot(s: Selector): Model = separateTails(s, filterNot)
  //def *~!(s: Selector): Model = restrictTailNot(s)

  //def restrictAllNot(s: Selector): Model = separateKeysOrTails(s, filterDeepNot)    
  //def **!(s: Selector): Model = restrictAllNot(s)
} 