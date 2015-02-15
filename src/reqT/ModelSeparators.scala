/***     
**                  _______        
**                 |__   __|   reqT - a requirements engineering tool  
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
  
  def contains(s: Selector): Boolean = {
    for ( e <- elems)  { if (s =*= e) return true else () } 
    false 
  }
  def ?(s: Selector): Boolean = contains(s)

  def restrict(s: Selector): Model = elems. filter { s =*= _ } .toModel
  def *(s: Selector): Model = restrict(s)
 
  def restrictTipAndHeads(s: Selector): Model = elems .collect { 
    case n: Node if s =*= n => n
    case r @ Relation(e,l,_) if s =*= Relation(e,l,Model()) => r 
  } .toModel    
  def *^(s: Selector): Model = restrictTipAndHeads(s)

  def restrictTails(s: Selector): Model = elems .collect { 
    case r @ Relation(_,_,t) if t ? s => r 
  } .toModel
  def *~(s: Selector): Model = restrictTails(s)

  def extract(s: Selector): Vector[Elem] = elems collect { case e if s =*= e => e }
  def \(s: Selector): Vector[Elem] = extract(s)
  
  def count(s: Selector): Int = extract(s).size
  def \#(s: Selector): Int = count(s)
  
} 