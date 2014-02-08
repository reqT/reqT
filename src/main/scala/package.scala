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

package object reqT 
extends Init with GlobalConstants with ImplicitFactoryObjects {
  import scala.language.implicitConversions

  implicit class ElemIterableToModel(it: Iterable[Elem]) {
    def toModel = Model(it.toSeq:_*)
    def toListModel = ListModel(it.toSeq:_*)
    def toHashModel = HashModel(it.toSeq:_*)
  }

  implicit class MapToModel(m: scala.collection.Map[Key, MapTo]) {
    def toModel = Model.fromMap(m.toMap)
    import scala.collection.immutable.{ListMap, HashMap}
    def toListModel = if (m.isInstanceOf[ListMap[Key, MapTo]]) Model.fromMap(m.toMap) else m.toSeq.toListModel
    def toHashModel = if (m.isInstanceOf[HashMap[Key, MapTo]]) Model.fromMap(m.toMap) else m.toSeq.toHashModel
  }

  implicit class KeyValueSeqToModel(seq: Seq[(Key, MapTo)]) {
    def toModel = Model(seq.map(Model.pairToElem) :_*)
    def toListModel = ListModel(seq.map(Model.pairToElem) :_*)
    def toHashModel = HashModel(seq.map(Model.pairToElem) :_*)
  }  
  
  implicit class KeyMapToTuple2ToElem(p: Tuple2[Key,MapTo]) { 
    def toElem: Elem = Model.pairToElem(p) 
  }

  def timed[T](block: => T) = {
    val tick = System.currentTimeMillis
    val result = block
    val tock = System.currentTimeMillis
    println(s"*** Timed: ${tock-tick} ms")
    result
  }

  object random {
    private val sing = Vector("do", "re", "mi", "fa", "so", "la", "ti")
    def rndUUID = java.util.UUID.randomUUID.toString
    def rndInt(until: Int): Int = util.Random.nextInt(until)
    def rndInt(from: Int, to: Int): Int = rndInt(to+1-from) + from
    def rndLetters(n: Int) = List.fill(n)(rndInt(97,122).toChar).mkString
    def rndSpeakable: String = rndPick(sing)
    def rndSpeakable(n: Int): String = List.fill(n)(rndSpeakable).mkString 
    def rndId: String = rndLetters(1) + rndInt(1,9)
    def rndText(n: Int, l: Int): String = List.fill(rndInt(1,n))(rndSpeakable(rndInt(1,l))).mkString(" ").capitalize + "." 
    def rndPick[T](xs: Seq[T]): T = xs(rndInt(xs.size))
    def rndType: Type = rndPick(metamodel.types)
    def rndEntityType: EntityType = rndPick(metamodel.entityTypes)
    def rndAttributeType: AttributeType[_] = rndPick(metamodel.attributeTypes)
    def rndEntity: Entity = rndEntityType(rndId)
    def rndStringAttribute: StringAttribute = rndPick(metamodel.stringAttributes)(rndText(3,5))
    def rndIntAttribute: IntAttribute = rndPick(metamodel.intAttributes)(rndInt(1,10))
    def rndRelationType: RelationType = rndPick(metamodel.relationTypes)
    def rndElem(n: Int, div: Int): Elem = rndInt(0,100) match {
      case i if i < 30 => rndEntity
      case i if i < 50 => rndStringAttribute
      case i if i < 60 => rndIntAttribute
      case _ => Relation(rndEntity, rndRelationType, rndModel(n/div, div)) 
    }
    def rndModel(n: Int = 7, div: Int = 2): Model = if (n > 0) List.fill(n)(rndElem(n, div max 2)).toHashModel else HashModel()
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
