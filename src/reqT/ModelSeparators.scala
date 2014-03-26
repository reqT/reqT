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
  
  def count(s: Selector): Int = s match {
    case AndSelector(left, right) => (this * s).count(left) + (this * s).count(right) //???
    case OrSelector(left, right)  => (this * s).count(left) + (this * s).count(right) //???
    case _ => elems. map {
      case n: Node if s.selects(n)  =>  1 
      case Relation(e, l, t) if s.selects(e) || s.selects(l) || s.selects(Head(e,l)) => 1 + t.count(s) 
      case Relation(e, l, t) => t.count(s) 
      case _ => 0
    } .sum
  }  
  
  def contains(s: Selector): Boolean = s match {
    case AndSelector(left, right) if contains(left) && contains(right) => true
    case OrSelector(left, right)  if contains(left) || contains(right) => true
    case _ => elems. map {
      case n: Node if s.selects(n)  =>  true 
      case Relation(e, l, t) if s.selects(e) || s.selects(l) || s.selects(Head(e,l)) =>  true 
      case Relation(e, l, t) =>  t.contains(s)
      case _ =>  false 
    } .contains(true)
  }
 
  def containsTipAndHeads(s: Selector): Boolean = elems. map {
    case n: Node if s.selects(n)  =>  true 
    case Relation(e, l, t) if s.selects(e) || s.selects(l) || s.selects(Head(e,l)) => true 
    case r: Relation if s.isInstanceOf[HeadType] && s.selects(r) =>  true 
    case _ =>  false 
  } .contains(true)

  def containsTails(s: Selector): Boolean = elems. map {
    case Relation(e, l, t) if t.contains(s)=> true 
    case _ =>  false 
  } .contains(true)
  
  def restrict(s: Selector): Model = elems.filter(e => Model(e).contains(s)).toModel
  def *(s: Selector): Model = restrict(s)
  
  def restrictNot(s: Selector): Model = elems.filter(e => !Model(e).contains(s)).toModel
  def *!(s: Selector): Model = restrictNot(s)

  def restrictTipAndHeads(s: Selector): Model = elems.filter(e => Model(e).containsTipAndHeads(s)).toModel    
  def *^(s: Selector): Model = restrictTipAndHeads(s)
  
  def restrictTails(s: Selector): Model = elems.filter(e => Model(e).containsTails(s)).toModel  
  def *~(s: Selector): Model = restrictTails(s)
  
  def extractNodes(s: Selector): Vector[Node] = s match {
    case AndSelector(left, right) => (this * s).extractNodes(left) ++ (this * s).extractNodes(right) //???
    case OrSelector(left, right)  => (this * s).extractNodes(left) ++ (this * s).extractNodes(right) //???
    case _ => elems .flatMap {
      case n: Node if s.selects(n) =>  Vector(n) 
      case Relation(e, l, t) if s.selects(e) => Vector(e) ++ t.extractNodes(s)
      case Relation(e, l, t) if s.selects(l) => Vector(e) ++ t.extractNodes(s)
      case Relation(e, l, t) if s.selects(Head(e,l)) => Vector(e) ++ t.extractNodes(s)
      case Relation(e, l, t) => t.extractNodes(s) 
      case _ =>  Vector()
    } 
  }
  def \(s: Selector): Vector[Node] = extractNodes(s: Selector)

  //def restrictTopNot(s: Selector): Model = separateKeys(s, filterNot)
  //def *^!(s: Selector): Model = restrictTopNot(s)
  
  //def restrictTailNot(s: Selector): Model = separateTails(s, filterNot)
  //def *~!(s: Selector): Model = restrictTailNot(s)

  //def restrictAllNot(s: Selector): Model = separateKeysOrTails(s, filterDeepNot)    
  //def **!(s: Selector): Model = restrictAllNot(s)
} 