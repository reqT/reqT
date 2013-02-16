/****************************************************************     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2013, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/
package org.reqt {  
  object util {
    def fileSep = System.getProperty("file.separator")
    def slashify(s:String) = s.replaceAllLiterally(fileSep, "/")
    def pwd = slashify(System.getProperty("user.dir"))
    def listFiles(dir: String) = new java.io.File(dir).listFiles.toList
    def ls(d: String) { println(listFiles(d) map { f => f.getName + ( if (f.isDirectory) "/" else "") }  mkString("\n")) }
    def ls { ls(pwd) }
    def dir { ls } 
    def dir(d: String)  = ls(d)
    var defaultPath = ""
    def saveString(s:String, fileName:String) = {
      val fn = defaultPath + fileName
      val outFile = new java.io.FileOutputStream(fn)
      val outStream = new java.io.PrintStream(outFile)
      try { outStream.println(s.toString) } finally { outStream.close }
      println("Saved to file: "+fn) 
    }
    def loadLines(fileName:String) = {
      val fn = defaultPath + fileName
      val source = scala.io.Source.fromFile(fn)
      val lines = source.getLines.toList
      source.close
      lines
    }
    def load(fileName:String): String = {
      val fn = defaultPath + fileName
      try  { loadLines(fn).mkString("\n") } catch  { case e: Throwable => "ERROR " + e }
    }
    //dbg utils to be used in REPL> :wrap timedSec
    def timedSec[T](body: => T): T = {
      val start = System.nanoTime
      try body
      finally println((System.nanoTime - start)/1e9 + " seconds elapsed.")
    }
  }
}