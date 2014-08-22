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

object Textified {
  //TODO: finish integration with gui
  //TODO: make this work for Contstraints and other Vector attributes
  val test1 = """
  Section sect1 has
    Text hej hej hej
    Text hej ehje hej
    //testcomment
    Section sect2 has
      Text hej
  """

  val test2 = """
  # section 1
    * gurka
    * banan
  # section 2
    * päron
    * äpple
  """
  val isRelation = metamodel.relationTypes.map(_.toString).toSet
  val isEntity = metamodel.entityTypes.map(_.toString).toSet
  val isAttribute = metamodel.attributeTypes.map(_.toString).toSet
  val relationTypeFromString: Map[String, RelationType] = metamodel.relationTypes.map(rt => (rt.toString, rt)).toMap
  def split(s: String): Seq[String] = s.split("\n").filterNot(_ == "").filterNot(_.trim.startsWith("//"))  
  def indentSize(s: String): Int = s.takeWhile(c => c == ' ' ).size
  def firstWord(s: String): String = s.trim.split(" ").take(1).headOption.getOrElse("")
  def middle(s: String): String = s.trim.split(" ").drop(1).dropRight(1).mkString(" ")
  def lastWord(s: String): String = s.trim.split(" ").takeRight(1).headOption.getOrElse("")
  def parts(s: String):(Int,String,String,String) = (indentSize(s), firstWord(s), middle(s), lastWord(s))
  def placeRelation(tuple : (Int,String,String,String)) = tuple match {
    case (indent, first, mid, last) =>
      val (mid2, last2) = if (isRelation(last) || !isEntity(first)) (mid, last) else (mid+last,"")
      val last3 = if (last2 == "" && isEntity(first)) "has" else last2
      (indent, first, mid2, last3)
  }
  def replaceSectionItem(tuple: (Int,String,String,String)): (Int,String,String,String) = tuple match {
    case (indent, first, mid, last) =>
      val (first2, mid2) = first match {
          case _ if first.startsWith("#") => ("Section", first.drop(1) + mid)
          case _ if first.startsWith("*") => ("Item", first.drop(1) + mid)
          case _ => (first, mid)
      }
      (indent, first2, mid2, last)
  }
  def mergeSentences(s1: String, s2: String): String = List(s1,s2).mkString(" ")
  def parseElem(tuple: (Int,String,String,String)): (Int, Elem) = tuple match {
    case (indent, first, mid, last) =>
      val elem = first match {
        case _ if isEntity(first) => Relation(Head(reqT.entityFromString(first)(mid), relationTypeFromString(last)), Model()) 
        case _ if isAttribute(first) => reqT.attributeFromString(first)(mergeSentences(mid, last).trim)
        case _ => reqT.makeAttribute[Text](mergeSentences(mid, last))        
      }
      (indent, elem)      
  }
  def indentElemSeq(s: String): List[(Int, Elem)] = split(s).map(parts).map(replaceSectionItem).map(placeRelation).map(parseElem).toList
  def recursiveMerge(levelElems: List[(Int, Elem)]): List[(Int, Elem)] = {
    def level(pair: (Int, Elem)): Int = pair._1
    def elem(pair: (Int, Elem)): Elem = pair._2
    def insertSub(le1: (Int, Elem), le2: (Int, Elem)): (Int, Elem) = elem(le1) match {
      case Relation(ent, link, sub) => (level(le1), Relation(ent, link, sub + elem(le2)))
      case other => throw new Error(s"Parse error on insertion of $le2 in $le1; Relation expected but found $other")
    }
    def isSameLevel(a: (Int, Elem), b: (Int, Elem)) = level(a) == level(b)
    def isGettingDeeper(a: (Int, Elem), b: (Int, Elem)) =  level(b) > level(a)
    def isSameOrGettingShallower(a: (Int, Elem), b: (Int, Elem)) = level(b) <= level(a)
    levelElems match {
      case Nil => Nil
      case le::Nil =>levelElems
      case le1::le2::Nil if isGettingDeeper(le1, le2) => insertSub(le1,le2)::Nil
      case le1::le2::Nil => levelElems
      case le1::le2::le3::rest if isGettingDeeper(le1, le2) && isSameOrGettingShallower(le2,le3) => 
        recursiveMerge(insertSub(le1, le2)::le3::rest)  
      case le1::le2::le3::rest if isGettingDeeper(le1, le2) && isGettingDeeper(le2, le3) => 
        val tail = recursiveMerge(le2::le3::rest)
        tail match {
          case Nil => Nil
          case x::Nil => insertSub(le1, x)::Nil 
          case x::xs => recursiveMerge(insertSub(le1, x) :: xs)
        }
      case le1::rest => le1::recursiveMerge(rest)     
    }
  }
  def apply(text: String): Model = 
    recursiveMerge(indentElemSeq(text)).map{ case (_, elem) => elem } .toModel
}

