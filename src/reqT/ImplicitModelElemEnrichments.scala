/***     
**                  _______        
**                 |__   __|   reqT - a free requriements engineering tool  
**   _ __  ___   __ _ | |      (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is free open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
***************************************************************************/

package reqT 
import scala.language.implicitConversions

trait ImplicitModelElemEnrichments {
  implicit class ElemIterableToModel(it: Iterable[Elem]) {
    def toModel = Model(it.toSeq:_*)
    def toListModel = ListModel(it.toSeq:_*)
    def toHashModel = HashModel(it.toSeq:_*)
  }

  implicit class MapToModel(m: scala.collection.Map[Key, MapTo]) {
    def toModel = Model.fromMap(m.toMap)
    import scala.collection.immutable.{ListMap, HashMap}
    def toListModel = 
      if (m.isInstanceOf[ListMap[Key, MapTo]]) Model.fromMap(m.toMap) 
      else m.toSeq.toListModel
    def toHashModel = 
      if (m.isInstanceOf[HashMap[Key, MapTo]]) Model.fromMap(m.toMap) 
      else m.toSeq.toHashModel
  }

  implicit class KeyValueSeqToModel(seq: Seq[(Key, MapTo)]) {
    def toModel = Model(seq.map(Model.pairToElem) :_*)
    def toListModel = ListModel(seq.map(Model.pairToElem) :_*)
    def toHashModel = HashModel(seq.map(Model.pairToElem) :_*)
  }  
  
  implicit class KeyMapToTuple2ToElem(p: Tuple2[Key,MapTo]) { 
    def toElem: Elem = Model.pairToElem(p) 
  }
  
  implicit class StringIterableToModel(it: Iterable[String]) {
    def as(et: EntityType): Iterable[Entity] = it.map(id => et(id))
  }
}
