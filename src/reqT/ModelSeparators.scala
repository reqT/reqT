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
  
  def contains(s: Selector): Boolean = {
    for ( e <- elems)  { if (s =*= e) return true else () } 
    false 
  }
  def ?(s: Selector): Boolean = contains(s)

  def restrict(s: Selector): Model = elems. filter { s =*= _ } .toModel
  def *(s: Selector): Model = restrict(s)
 
  def restrictTipAndHeads(s: Selector): Model = ???    
  def *^(s: Selector): Model = restrictTipAndHeads(s)

  def restrictTails(s: Selector): Model = ???  
  def *~(s: Selector): Model = restrictTails(s)

  def extractElems(s: Selector): Vector[Elem] = collectElems { case e if s =*= e => e }
  def \(s: Selector): Vector[Elem] = extractElems(s)
  
  def count(s: Selector) = extractElems(s).size
  
} 