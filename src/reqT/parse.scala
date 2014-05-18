package reqT
package parse

case class Parsed[T](opt: Option[T], errMsg: String = "") 
object Parsed {
  def apply[T](result: T): Parsed[T] = new Parsed(Some(result))
}

object node {
  def apply(myType: String, body: String): Parsed[Node] = {
    if (entityFromString.isDefinedAt(myType)) 
      Parsed(entityFromString(myType)(body.trimUnquote))
    else if (attributeFromString.isDefinedAt(myType)) 
      Parsed(attributeFromString(myType)(body.trimUnquote))
    else Parsed(None, s"ERROR: node type unknown:  $myType($body)")
  }
  def apply(nodeString: String): Parsed[Node] = {
    val (from, to) = (nodeString.indexOf("("), nodeString.indexOf(")"))
    if (from > 0 && to >0) {
      val (myType, body) = nodeString.splitAt(from)
      apply(myType, body.drop(1).take(body.length-2))
    } else Parsed(None, s"""ERROR: missing ( ) in:  $nodeString""")
  }
}

object headPath {
  def apply(pathString: String): Parsed[HeadPath] = {
    val ps = pathString.split("/").toSeq.map(_.split("[.]").toSeq)
    ???
  }
  
}


