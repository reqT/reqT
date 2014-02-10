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

trait StringUtil {
  implicit class EnrichedString(s: String) {
    def toScala: String = "" + '\"' + convertEscape + '\"'
    //def toModel: Model = if (s == "") Model() else Model.interpret(s)
    def toIntOrZero: Int = try {s.toInt} catch { case e: NumberFormatException => 0}
    //def toLevel: StatusLevel = levelFromString(s)
    def decapitalize: String = strUtil.decapitalize(s)
    def truncPad(n: Int) = strUtil.truncPad(s, n)
    def trunc(n: Int) = strUtil.trunc(s, n)
    def indentNewline(n: Int = 2) = strUtil.indentNewline(s, n)
    def filterEscape: String = strUtil.filterEscapeChar(s)
    def convertEscape: String = strUtil.escape(s)
    //def save(fileName:String) = saveString(s, fileName) 
    def show { println(s) }
  }
  
  object strUtil { 
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