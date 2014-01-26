/*     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/

package reqT

trait Choice extends IndexedOrder[Choice] {
  def order: Vector[Choice] = Vector(Zero, One, ZeroOrOne, OneOrMany, ZeroOrMany)
}
case object Zero extends Choice
case object One extends Choice
case object ZeroOrOne extends Choice
case object OneOrMany extends Choice
case object ZeroOrMany extends Choice