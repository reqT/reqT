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
import scala.language.implicitConversions

object BagUtil {
  type Bag[K, V] = Map[K, Vector[V]]
  //type SetBag[K, V] = Map[K, Set[V]]
  
  def bagMerge[K,V](m1: Bag[K, V],m2: Bag[K, V]): Bag[K, V] =
    (m1.keys++m2.keys).map(k => (k,m1.getOrElse(k,Vector())++m2.getOrElse(k,Vector()))).toMap

  def bagAdd[K,V](bag: Bag[K, V], k: K, v: V): Bag[K, V] = 
    bag.updated(k, ( if (bag.isDefinedAt(k)) bag(k) else Vector()) :+ v)
  
  implicit class RichMapAsVectorBag[K,V](bag: Map[K,Vector[V]])  {
    def join[B >: V](that: Map[K,Vector[B]]): Map[K,Vector[B]] = bagMerge(this.bag, that)
    def bagAdd(elem: (K, V)): Map[K,Vector[V]] = BagUtil.bagAdd(this.bag, elem._1, elem._2)  
    def :+(elem: (K, V)): Map[K,Vector[V]] = BagUtil.bagAdd(this.bag, elem._1, elem._2)  
  }
  
  object Bag {
    def apply[K,V](elems: (K, V)*): Bag[K,V] = {
      var bag: Bag[K,V] = Map()
      elems.map(e => bag :+= e)
      bag
    }
  }
}  