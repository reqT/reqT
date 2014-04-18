/*     
**                  _______        
**                 |__   __|   reqT - a free requriements engineering tool  
**   _ __  ___   __ _ | |      (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
***************************************************************************/

package reqT
import java.lang.{System => JSystem}

trait Init {
  import scala.tools.nsc.interpreter.IMain
  def makeIMain(): IMain = {
    //to be used when no ILoop is needed, e.g. from another main
    //reqT.initInterpreter()
    println("** Starting Scala interpreter ...")
    val settings = new scala.tools.nsc.Settings()
    settings.classpath.value = JSystem.getProperty("java.class.path")
    val writer = new java.io.PrintWriter((new java.io.OutputStreamWriter(Console.out)))
    new IMain(settings, writer)
  }
  
  def initInterpreter(intp: IMain = makeIMain() ): IMain = {
    //println("** Initializing reqT ...")
    intp.quietRun("import scala.language._")
    intp.quietRun("import reqT._")
    //intp.quietRun("import reqT." + reqt.elementNames.mkString("{",", ","}")) //to allow tab completion on model elements
    //intp.quietRun("import reqT.abbrev._")
    intp
  }
  
  def init(intp: IMain = makeIMain() ): Unit = {
    println(repl.startMsg)
    println(repl.versionMsg)
    initInterpreter(intp)
    ()
  }

}