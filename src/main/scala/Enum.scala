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
 
trait Enum[T <: Ordered[T]] extends Ordered[T] {
  self : T =>
  val myType: EnumType[T]
  import myType._
  def compare(that: T): Int = indexOf(this).compare(indexOf(that))
  def toInt: Int = indexOf(this)
}

trait EnumType[T <: Ordered[T]] {
  val values: Vector[T]
  lazy val names: Vector[String] = values.map(_.toString)
  lazy val valueOf: Map[String, T] = names.zip(values).toMap
  lazy val indexOf: Map[T, Int] = values.zipWithIndex.toMap 
  implicit val ordering = Ordering.fromLessThan[T](_ < _)
}
