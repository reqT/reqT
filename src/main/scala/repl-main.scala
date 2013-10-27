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

package reqt
/*
scalac repl-main.scala
jar cfe reqT.jar reqT.start reqT
scala reqT.jar
*/
import scala.tools.nsc._
import interpreter._
import java.io._

object repl {
  //val helpOnReqT: String = "** Type ?? for help on reqT, type :h for help on Scala REPL"
  val helpOnReqT: String = "** Type :h for help on Scala REPL"
  val startMsg: String = 
    s"\n** Welcome to reqT version $VERSION compiled with Scala version $SCALA_VERSION" +  
     "\n** Running Java version " + System.getProperty("java.version") +
    s"\n$reqT_PREAMBLE\n$helpOnReqT" +
    "\n** Starting reqT ..."
  
  var interpreter: Option[ReqTILoop] = None
  
  def reset() { 
  /*
    //the idea was that this should avoid memory leaks
    //https://issues.scala-lang.org/browse/SI-4331
    //but it does not seem to help...
    //test case:
      var j = 0
      while (true) {
        if (j % 1000 == 0) {
          $intp.interpret("""reqt.repl.reset""")
          $intp.bind("j",j)
          $intp.interpret(s"var i = $j")
        }
        $intp.interpret("""i += 1;println(i + ":" + new java.util.Date)""")
        j += 1
      }    
  */
    interpreter.map { i => 
      i.intp.reset
      i.initReqT
    }
  }
  
  class ReqTILoop(out : PrintWriter) extends ILoop(None, out) {
     override val prompt = "\nreqT> "
     override def loop() {
    	 if (isAsync) awaitInitialized()
    	 initReqT()
    	 super.loop()
     }
     def initReqT() {
       intp.quietBind("$intp", intp) //check if this is really needed??
       intp.interpret("reqt.initInterpreter($intp)")
     }
    override def helpCommand(line: String): Result = {
      if (line == "") echo(helpOnReqT)
      super.helpCommand(line)
    }

     override def printWelcome(): Unit = {
       out.println(startMsg)
       out.flush()
     }
  }

  def startInterpreting = {
    val out = new PrintWriter( new BufferedWriter( new OutputStreamWriter(System.out) ) )
    val settings = new GenericRunnerSettings(out.println)
    settings.usejavacp.value = true
    interpreter = Some(new ReqTILoop(out))
    interpreter.map(_.process(settings))
  }
}

object start {
  def main(args : Array[String]) : Unit =  {
    if (args.size == 0) repl.startInterpreting
    else args(0) match {
      case "hello" => println("hello reqT")
      case arg => println("Unknown args: " + args.mkString(" "))
    }
  }
}
