package reqt
/*
scalac repl-main.scala
jar cvfe reqT.jar reqT.start reqT
scala reqT.jar
*/
import scala.tools.nsc._
import interpreter._
import java.io._

object repl {
  val helpOnReqT: String = "** Type ? for help on reqT"
  val startMsg: String = 
    s"\nStarting reqT-v$VERSION compiled with Scala $SCALA_VERSION ..." +  
    s"\n$reqT_PREAMBLE\n$helpOnReqT\n"
    
  class ReqTILoop(out : PrintWriter) extends ILoop(None, out) {
     override val prompt = "\nreqT> "
     override def loop() {
    	 if (isAsync) awaitInitialized()
    	 initReqT()
    	 super.loop()
     }
     def initReqT() {
       intp.quietBind("$intp", intp) //is this really good/needed??
       intp.interpret("reqt.init($intp)")
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
    val interpreter = new ReqTILoop(out)
    interpreter.process(settings)
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
