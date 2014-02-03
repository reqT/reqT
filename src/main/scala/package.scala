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

package object reqT 
extends Init with Constants with ImplicitFactoryObjects {
  import scala.language.implicitConversions

  // implicit class ElemSeqToModel(seq: Seq[Elem]) {
    // def toModel = Model(seq:_*)
    // def toListModel = ListModel(seq:_*)
    // def toHashModel = HashModel(seq:_*)
  // }

  implicit class ListMapToModel(lm: scala.collection.immutable.ListMap[Key, MapTo]) {
    def toModel = Model(lm)
  }

  implicit class KeyValueSeqToModel(seq: Seq[(Key, MapTo)]) {
    def toModel = Model(seq.map(Model.pairToElem) :_*)
  }  
  
  implicit class FilterMonadicToModel(fm: scala.collection.generic.FilterMonadic[Elem, Seq[Elem]]) {
    def toModel = Model(fm.map(x => x):_*)
  }  
  
  implicit class KeyMapToTuple2ToElem(p: Tuple2[Key,MapTo]) { 
    def toElem: Elem = Model.pairToElem(p) 
  }
  def uuid = java.util.UUID.randomUUID.toString

  def timed[T](block: => T) = {
    val tick = System.currentTimeMillis
    val result = block
    val tock = System.currentTimeMillis
    println(s"*** Timed: ${tock-tick} ms")
    result
  }

  def bigModel(n: Int) = Model((1 to n).map(i => Req(s"$i")):_*)
  def bigHashModel(n: Int) = HashModel((1 to n).map(i => Req(s"$i")):_*)
  
  object IdGenerator {
    @volatile var myId = 0
    def next: Int = synchronized {
      myId += 1
      myId
    }
    def reset: Unit = synchronized { myId = 0 }
  }
  
  def nextId: Int = IdGenerator.next
}
