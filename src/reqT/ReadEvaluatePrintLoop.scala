/***
**                  _______
**                 |__   __|   reqT - a requirements engineering tool
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
  val helpOnReqT =
      "** Type  edit      to start model editor gui" +
    "\n** Type  :help     for help on the Scala interpreter" +
    "\n** Type  :pa       to enter paste mode" +
    "\n** Type  :q        to exit when all sub-threads are done" +
    "\n** Type  sys.exit  to exit and terminate all threads" +
    "\n** Type  Feature?  to get help on a concept, e.g. Feature"
  val versionMsg = s"\n** Welcome to reqT version $reqT_VERSION" +
    s"\n** Snapshot build number: $reqT_BUILD" +
    s"\n** Scala $SCALA_VERSION" +
     "\n** Java  version " + JSystem.getProperty("java.version") +
     " " + JSystem.getProperty("java.vm.name")
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
    //the idea was that this should avoid the memory leak bug:
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
     reqT.initInterpreter()
    }
  }

  class ReqTILoop(out : PrintWriter) extends ILoop(None, out) {
    override def createInterpreter() {
      super.createInterpreter()
      initReqT()
    }
    override val prompt = "\nreqT> "
    def initReqT() {
      intp.quietRun("import scala.language._")
      intp.quietRun("import reqT._")
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
     override def createInterpreter() {
      super.createInterpreter()
      intp.quietRun(reqT.load(fileName))
     }
     override def printWelcome(): Unit = { out.flush() }
  }

  class CodeRunner(out : PrintWriter, code: String) extends ReqTILoop(out : PrintWriter) {
    override def createInterpreter() {
      super.createInterpreter()
      intp.quietRun(s"{$code}")
    }
    override def printWelcome(): Unit = { out.flush() }
  }

  class EditorLauncher(out : PrintWriter, args : Array[String])
  extends ReqTILoop(out : PrintWriter) {
    override def createInterpreter() {
      super.createInterpreter()
      println("Launching Editor with args: " + args.mkString(","))
      if (args.isEmpty) run("val editor0 = edit()")
      else for (i <- 0 until args.size) {
        val f = args(i)
        println("f = "+f)
        intp.interpret(s"println($i)")
        val code = s"""val editor$i = edit(repl.interpretModel(load("$f")).getOrElse(Model(Spec(s"ERROR loading Model from file $f")))"""
        println("code = " + code)
        intp.interpret(code)
      }
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

  def initInterpreterAndEdit(args : Array[String]) {
    val out = new PrintWriter( new BufferedWriter( new OutputStreamWriter(JSystem.out) ) )
    val settings = new GenericRunnerSettings(out.println)
    settings.usejavacp.value = true
    interpreter = Some(new EditorLauncher(out, args) )
    interpreter.map(_.process(settings))
  }

  def quietRun(code: String) {
    checkIntp()
    interpreter .map { i => i.quietRun(code) }
  }

  def run(code: String): Option[IR.Result] = {
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
    interpret(s"""{$code}: Model""").map(_.asInstanceOf[Model])

  def interpretTransformer(code: String): Option[Model => Model] =
    interpret(s"""{$code}: Model => Model""").map(_.asInstanceOf[Model => Model])


  // SERVER MODE reading from stdio, no jline, no string print truncate
  // http://docs.scala-lang.org/overviews/repl/embedding


  @volatile
  var isServerMode = false

  class ServerILoop(in: BufferedReader, out : PrintWriter) extends ILoop(in, out) {
    override def createInterpreter() {
      super.createInterpreter()
      initReqTServer()
    }

    override val prompt = "\n<!-- reqT server ready for input -->\n"

    def initReqTServer() {
      intp.quietRun("import scala.language._")
      intp.quietRun("import reqT._")
      intp.isettings.maxPrintString = Int.MaxValue
      isServerMode = true
    }

    override def printWelcome(): Unit = {
      val jvm =  JSystem.getProperty("java.version")
      val scla = scala.util.Properties.versionNumberString
      out.println(s"reqT v$reqT_VERSION running in server mode with Scala $scla on JVM $jvm ")
      out.flush()
    }
  }

  def loopInterpreterFromStandardIO(): Unit = {
    val out = new PrintWriter(scala.Console.out, true)
    val settings = new scala.tools.nsc.Settings
    settings.usejavacp.value = true
    val iloop = new ServerILoop(scala.Console.in, out)
    isServerMode = true
    while (true) {
      iloop.process(settings)
      println("<!-- Exit issued. Restarting reqT in server mode... -->")
    }
  }

}
