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

import scala.tools.nsc._
import interpreter._
import java.io._
import java.lang.{System => JSystem}

object repl { 
  //val helpOnReqT: String = "** Type ?? for help on reqT, type :h for help on Scala REPL"
  val helpOnReqT = "** Type :help for help on the Scala interpreter"
  val versionMsg = s"\n** Welcome to reqT version $reqT_VERSION" +
    s"\n** Snapshot build: $BUILD_DATE" + 
    s"\n** Compiled with Scala $SCALA_VERSION" +  
     "\n** Running Java version " + JSystem.getProperty("java.version") +  
     "\n** Running on " + JSystem.getProperty("java.vm.name")
  val startMsg = versionMsg +
     s"\n$PREAMBLE\n$helpOnReqT"  
  
  @volatile
  var interpreter: Option[ReqTILoop] = None
  def checkIntp() { 
    if (interpreter == None) 
      throw new Error("No interpreter available. Try reqT.initInterpreter() ") 
  }  

  def reset() { 
  /*
    //the idea was that this should avoid memory leaks
    //https://issues.scala-lang.org/browse/SI-4331
    //but it does not seem to help...
    //test case:
      var j = 0L
      while (j<100000L) {
        if (j % 1000 == 0) {
          $intp.interpret("""reqT.repl.reset""")
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
     intp.quietRun("reqT.initInterpreter($intp)")
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
  
  class FileRunner(out : PrintWriter, fileName: String) extends ReqTILoop(out : PrintWriter) {
    override def loop() {
     if (isAsync) awaitInitialized() 
     intp.quietBind("$intp", intp) //check if this is really needed??
     intp.quietRun("reqT.initInterpreter($intp)")
     intp.quietRun(reqT.load(fileName))
    }
     override def printWelcome(): Unit = {
       //out.println("** reqT FileRunner starting ...")
       out.flush()
     }
  }
  
  class CodeRunner(out : PrintWriter, code: String) extends ReqTILoop(out : PrintWriter) {
    override def loop() {
     if (isAsync) awaitInitialized() 
     intp.quietBind("$intp", intp) //check if this is really needed??
     intp.quietRun("reqT.initInterpreter($intp)")
     intp.quietRun(s"{$code}")
    }
     override def printWelcome(): Unit = {
       out.flush()
     }
  }

  def startInterpreting() {
    val out = new PrintWriter( new BufferedWriter( new OutputStreamWriter(JSystem.out) ) )
    val settings = new GenericRunnerSettings(out.println)
    settings.usejavacp.value = true
    interpreter = Some( new ReqTILoop(out) )
    interpreter.map(_.process(settings))
  }
  
  def interpretFile(fileName: String) {
    val out = new PrintWriter( new BufferedWriter( new OutputStreamWriter(JSystem.out) ) )
    val settings = new GenericRunnerSettings(out.println)
    settings.usejavacp.value = true
    interpreter = Some( new FileRunner(out, fileName) )
    interpreter.map(_.process(settings))    
  }

  def initInterpreterAndRun(code: String) {
    val out = new PrintWriter( new BufferedWriter( new OutputStreamWriter(JSystem.out) ) )
    val settings = new GenericRunnerSettings(out.println)
    settings.usejavacp.value = true
    interpreter = Some( new CodeRunner(out, code) )
    interpreter.map(_.process(settings))      
  }
  
  
  def quietRun(code: String) { 
    checkIntp() 
    interpreter .map { i => i.quietRun(code) }
  }
  
  def run(code: String) { 
    checkIntp() 
    interpreter .map { i => i.interpret(code) }
  }
  
  def interpret(code: String): Option[Any] = { 
    checkIntp() 
    interpreter .map { i =>
          val result = Array[Any](null)
          i.beQuietDuring(i.bind("result", "Array[Any]", result))
          val verdict = i.quietRun(s"""{result(0) = $code}""")
          if (verdict == scala.tools.nsc.interpreter.IR.Success)
            Some(result(0)) 
          else None          
    } .getOrElse(None)
  }
  
  def interpretInt(code: String): Option[Int] = 
    interpret(s"""{$code}: Int""").map(_.asInstanceOf[Int])
    
  def interpretString(code: String): Option[String] = 
    interpret(s"""{$code}: String""").map(_.asInstanceOf[String])

  def interpretBoolean(code: String): Option[Boolean] = 
    interpret(s"""{$code}: Boolean""").map(_.asInstanceOf[Boolean])    
  
  def interpretModel(code: String): Option[Model] =  
    interpret(s"""{$code}: reqT.Model""").map(_.asInstanceOf[Model])

  def interpretTransformer(code: String): Option[Model => Model] =  
    interpret(s"""{$code}: reqT.Model => reqT.Model""").map(_.asInstanceOf[Model => Model])
    
  // def interpretString(code: String): Option[String] = {
    // checkIntp() 
    // interpreter .map { i =>
          // val result = Array[String]("ERROR")
          // i.beQuietDuring(i.bind("result", "Array[String]", result))
          // val verdict = i.quietRun(s"""result(0) = $code""")
          // if (verdict == scala.tools.nsc.interpreter.IR.Success)
            // Some(result(0))
          // else None          
    // } .getOrElse(None)  }

  // def interpretBoolean(code: String): Option[Boolean] = {
    // checkIntp() 
    // interpreter .map { i =>
          // val result = Array[Boolean](false)
          // i.beQuietDuring(i.bind("result", "Array[Boolean]", result))
          // val verdict = i.quietRun(s"""result(0) = $code""")
          // if (verdict == scala.tools.nsc.interpreter.IR.Success)
            // Some(result(0))
          // else None          
    // } .getOrElse(None)  }
    
  // def interpretOrElse[T](code: String, orElse: T)( implicit m: scala.reflect.ClassTag[T]): T =  
    // interpret(code).map(r => //gotta trick erasure
      // try { m.runtimeClass.cast(r).asInstanceOf[T] } catch { case t: Throwable => orElse } 
    // ) .getOrElse(orElse)  
  /* 
    why does this not work:
    reqT> repl.interpretOrElse("true",false)
    res9: Boolean = false
    when this works:
    reqT> repl.interpretOrElse(""" "12" """, "13")
    res5: String = 12
  */

  
  
  // def interpretModel(code: String): Option[Model] = { 
    // checkIntp() 
    // interpreter .map { i =>
          // val result = Array[Model](Model())
          // i.beQuietDuring(i.bind("result", "Array[reqT.Model]", result))
          // val verdict = i.quietRun(s"""result(0) = $code""")
          // if (verdict == scala.tools.nsc.interpreter.IR.Success)
            // Some(result(0)) 
          // else None          
    // } .getOrElse(None)
  // }
}
