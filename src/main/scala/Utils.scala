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


object BagUtils {
  type Bag[K, V] = Map[K, Vector[V]]
  type SetBag[K, V] = Map[K, Set[V]]
  
  def bagMerge[K,V](m1: Bag[K, V],m2: Bag[K, V]): Bag[K, V] =
    (m1.keys++m2.keys).map(k => (k,m1.getOrElse(k,Vector())++m2.getOrElse(k,Vector()))).toMap
    
  def setBagMerge[K,V](m1: SetBag[K, V],m2: SetBag[K, V]): SetBag[K, V] =
    (m1.keys++m2.keys).map(k => (k,m1.getOrElse(k,Set())++m2.getOrElse(k,Set()))).toMap    

  def bagAdd[K,V](bag: Bag[K, V], k: K, v: V): Bag[K, V] = 
    bag.updated(k, ( if (bag.isDefinedAt(k)) bag(k) else Vector()) :+ v)
  
  def setBagAdd[K,V](bag: SetBag[K, V], k: K, v: V): SetBag[K, V] = 
    bag.updated(k, ( if (bag.isDefinedAt(k)) bag(k) + v else Set(v)))
  
  implicit class RichMapVectorBag[K,V](bag: Map[K,Vector[V]])  {
    def join[B >: V](that: Map[K,Vector[B]]): Map[K,Vector[B]] = bagMerge(this.bag, that)
    def bagAdd(elem: (K, V)): Map[K,Vector[V]] = BagUtils.bagAdd(this.bag, elem._1, elem._2)  
    def :+(elem: (K, V)): Map[K,Vector[V]] = BagUtils.bagAdd(this.bag, elem._1, elem._2)  
  }
  
  implicit class RichMapSetBag[K,V](bag: Map[K,Set[V]])  {
    def join(that: Map[K,Set[V]]): Map[K,Set[V]] = setBagMerge(this.bag, that)
    def bagAdd(elem: (K, V)): Map[K,Set[V]] = BagUtils.setBagAdd(this.bag, elem._1, elem._2)  
    def :+(elem: (K, V)): Map[K,Set[V]] = BagUtils.setBagAdd(this.bag, elem._1, elem._2)  
  }
  
  object Bag {
    def apply[K,V](elems: (K, V)*): Bag[K,V] = {
      var bag: Bag[K,V] = Map()
      elems.map(e => bag :+= e)
      bag
    }
  }
  
  object SetBag {
    def apply[K,V](elems: (K, V)*): SetBag[K,V] = {
      var bag: SetBag[K,V] = Map()
      elems.map(e => bag :+= e)
      bag
    }
  }
} 


trait StringUtils {
  implicit class EnrichedString(s: String) {
    def toModel: Model = repl.interpretModel(s).get
    def toScala: String = "" + '\"' + convertEscape + '\"'
    def toIntOrZero: Int = try {s.toInt} catch { case e: NumberFormatException => 0}
    //def toLevel: StatusLevel = levelFromString(s)
    def decapitalize: String = strUtils.decapitalize(s)
    def truncPad(n: Int) = strUtils.truncPad(s, n)
    def trunc(n: Int) = strUtils.trunc(s, n)
    def indentNewline(n: Int = 2) = strUtils.indentNewline(s, n)
    def filterEscape: String = strUtils.filterEscapeChar(s)
    def convertEscape: String = strUtils.escape(s)
    def p: Unit       = Console.println(s)
    def print: Unit   = Console.print(s)
    def println: Unit = Console.println(s) 
    def show: Unit    = Console.println(s) 
  }
  
  object strUtils { 
    def decapitalize(s:String) = s.take(1).toLowerCase + s.drop(1)
    def indentNewline(s: String, n: Int) = s.replace("\n","\n"+ (" " * n))
    def quoteIfString(a:Any):String = a match {
      case s:String => "\"" + s + "\""
      case _ => a.toString
    }
    def escapeSeq(s:String):String = (for (c <- s) yield c match {
      case '\b' => '\\'+"b"
      case '\t' => '\\'+"t"
      case '\n' => '\\'+"n"
      case '\f' => '\\'+"f"
      case '\r' => '\\'+"r"
      case '\"' => ""+'\\'+'\"'
      //case '\'' =>  ""+'\\'+ """'"""
      case '\\' => ""+'\\'+'\\'	
      case _ => c.toString
    }).mkString("")
    def charToUnicodeSeq(c:Char):String = if (c >= ' ') c.toString else {
      val h = Integer.toHexString(c)
      val zeroes = ( for (i <- 1 to (4-h.length)) yield "0").mkString("")
      "\\u" + zeroes + h
    }
    def unicodeSeq(s:String):String = 
      (for (c <- s) yield charToUnicodeSeq(c)).mkString("")
    def escape(s:String):String = unicodeSeq(escapeSeq(s)) 
    def filterEscapeChar(s:String) = s.toList.filterNot(_ < ' ').mkString
    //def lineBreaks(s:String):String =  
    //	if (!s.contains("//")) s else 
    //		"(" + s.replaceAll("//","\" +\n      \"//") + "\n    )"
    def valueToString(v:Any):String = v match {
      case s:String =>  "\"" + escape(s) + "\""    //lineBreaks(escape(s)) //removed as collide with latex
      case _ => v.toString
    }
    def valueToRawString(v:Any) :String = v match {
      case s:String =>  "\"\"\"" + s + "\"\"\""
      case _ => v.toString
    }
    def scalaSuffix(s:String):String = if (!s.contains(".")) s + ".scala" else s
    def latexSuffix(s:String):String = if (!s.contains(".")) s + ".tex" else s
    def txtSuffix(s:String):String = if (!s.contains(".")) s + ".txt" else s
    def varPrefix(s:String):String = if (s.contains(".")) "" else  "var " + s + " = "
    def truncPad(s: String, n: Int): String = trunc(s,n) + (" " * (n - s.size))
    def trunc(s: String, n: Int): String = { 
      val s2 = s.take(n)
      if (s2.size < s.size) s2.take(n-3) + "..." else s2
    }
  } 
}  

trait FileUtils {

  import fileUtils._
  
  implicit class StringSaver(s: String) {
    def save(fileName: String): Unit = saveString(s, fileName)
  }
  
  def pwd { println(workDir)}

  def load(fileName:String): String = {
    val fn = resolveFileName(fileName)
    try  { loadLines(fn).mkString("\n") } catch  { case e: Throwable => "ERROR " + e }
  }

  def ls(d: String): Unit = { println(listFiles(d).getOrElse { 
        println("ERROR Directory not found:" + d ); List[java.io.File]()
      } .map { f => f.getName + ( if (f.isDirectory) "/" else "")  }  .mkString("\n")) 
  }
  def ls: Unit = { ls(workingDirectory) }
  def dir: Unit = { ls } 
  def dir(d: String): Unit = ls(d)
  def cd(d: String): Unit = { 
    val dd = if (d == "..") new java.io.File(workingDirectory).getParent.toString  else resolveFileName(d)
    val f = new java.io.File(dd)
    if (f.isDirectory && f.exists) workingDirectory = dd else println("ERROR Directory not found:" + dd )
    pwd  
  }
  def cd: Unit = cd(startDir)
  
  object fileUtils {
    //implicit val codec: scala.io.Codec = scala.io.Codec.UTF8
    def fileSep = java.lang.System.getProperty("file.separator")
    def slashify(s:String) = s.replaceAllLiterally(fileSep, "/")
    val startDir = slashify(java.lang.System.getProperty("user.dir"))
    val homeDir = slashify(java.lang.System.getProperty("user.home"))
    protected [FileUtils] var workingDirectory = startDir
    def workDir = workingDirectory
    def resolveFileName(fileName: String): String = {
      val f = new java.io.File(fileName)
      val fn = slashify(f.toString)
      if (f.isAbsolute || fn.take(1) == "/" || fn.contains(":")) fn else workingDirectory + "/" + fn
    }
    def listFiles(dir: String): Option[List[java.io.File]] = 
      new java.io.File(resolveFileName(dir)).listFiles match { case null => None; case a => Some(a.toList) }
    def saveString(s:String, fileName:String) = {
      val fn = resolveFileName(fileName)
      // val outFile = new java.io.File(fn)
      // val outStream = new java.io.PrintWriter(outFile,"UTF-8")
      // try { outStream.println(s.toString) } finally { outStream.close }
      scala.tools.nsc.io.File(fn).writeAll(s)
      println("Saved to file: "+fn) 
    }
    def loadLines(fileName:String): List[String] = {
      val fn = resolveFileName(fileName)
      val source = scala.io.Source.fromFile(fn)
      val lines = source.getLines.toList
      source.close
      lines
    }
    
    // def loadTable(fileName:String, rowSeparator: String = "\t"): Model = 
      // Model.fromTable(load(fileName), rowSeparator)  
  }
 
}

trait RandomUtils {
  object rnd {
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
    def rndType: MetaType = rndPick(metamodel.types)
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
}

trait DebugUtils {  
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
 
  def timed[T](block: => T): T = {
    val tick = java.lang.System.currentTimeMillis
    val result = block
    val tock = java.lang.System.currentTimeMillis
    println(s"*** Timed: ${tock-tick} ms")
    result
  }
}


