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
 
trait IndexedOrder[T] extends Ordered[T] {
  self: T => 
  def order: Vector[T]
  lazy val indexOf: Map[T, Int] = order.zipWithIndex.toMap 
  def compare(that: T): Int = indexOf(this).compare(indexOf(that))
}
