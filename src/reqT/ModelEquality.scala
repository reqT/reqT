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

/** A trait mixed in by Model, implementing the Scala equals contract.
    The equals method uses structural equality, not considering order.
    The equals method delegates to the underlying Map implementations.
    
    Examples: 
    
    The following expressions are true:
    reqT> Model(Req("x"), Req("y")) == Model(Req("y"), Req("x"))
    res1: Boolean = true

    reqT> ListModel(Req("x"), Req("y")) == HashModel(Req("y"), Req("x"))
    res2: Boolean = true
    
    If you want to test equality also with respect to order, use toSeq:
    reqT> Model(Req("x"), Req("y")).toSeq == Model(Req("y"), Req("x")).toSeq
    res2: Boolean = false
*/
trait ModelEquality extends ModelBase {
  self: Model =>
  
  def canEqual(other: Any): Boolean = other.isInstanceOf[Model] 
  
  override def equals(other: Any): Boolean = other match {
    case that: Model => (that canEqual this) && (myMap == that.myMap)
    case _ => false
  }
  
  override def hashCode: Int = myMap.hashCode
}
