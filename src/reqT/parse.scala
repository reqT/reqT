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
    if (from > 0 && to > 0) {
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

case class Tab(
    headings: Vector[String], 
    table: Vector[Vector[String]], 
    separator: String = ";")
extends Serializable {
  def apply(row: Int) = table(row)
  def apply(row: Int, col: Int) = table(row)(col)
  def mapRow[T](f: Vector[String] => T): Vector[T] = table.map(f)
  def map[T](f: String => T): Vector[Vector[T]] = table.map(_.map(f))
  def toString(separator: String) = 
    headings.mkString("",separator,"\n") +
    table.map(_.mkString(separator)).mkString("\n")
  override def toString = toString(separator)
}

object Tab {
  def loadLines(fileName:String): Vector[String] = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.getLines.toVector
    source.close
    lines
  }
  def load(fileName: String, separator: String = ";", hasHeadingRow: Boolean = true): Tab = {
    val lines: Vector[String] = loadLines(fileName) 
    val headings: Vector[String] = 
      if (hasHeadingRow) lines.headOption.getOrElse("").split(separator).toVector
      else Vector()
    val h = if (hasHeadingRow) 1 else 0
    val table: Vector[Vector[String]] =
      lines.drop(h).map(_.split(separator).toVector).toVector
    Tab(headings, table, separator)
  }
}

object loadTab {

  def prioVoting(fileName: String): Model = 
    Tab.load(fileName).mapRow {
      case Vector(s,f,prio) => Stakeholder(s) has(Feature(f) has Prio(prio.toInt))
    }.toModel
}

