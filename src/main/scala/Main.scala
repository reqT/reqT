import reqt.MainWindow.initFileName
export reqt.Main.edit

package reqt:
  object Main:

    val scalaVersion = "3.6.4-RC1"

    val reqTVersion  = "4.5.0"

    val latestVersionURL = "https://reqT.github.io/latest-version/index.html"

    def getLatestVersion(): String = 
      util.Try:
        val s = scala.io.Source.fromURL(latestVersionURL)
        try s.mkString finally s.close()
      .toOption.getOrElse(s"Error: cannot connect to $latestVersionURL")

    val reqTHome = "https://reqT.github.io"

    val reqTDownload = "https://github.com/reqT/reqT/releases/latest/download/reqT.jar"
    
    val replInitScript = """ import reqt.* """.trim

    val helpMessage = 
      s"""|Welcome to reqT $reqTVersion $reqTHome
          |
          |  Main program args:
          |
          |    <none>       open a reqT window with empty model
          |    edit f1 f2   for each file open a window with model from file
          |    repl         start the scala repl and do 'import reqt.*'
          |    quiz         start a quiz game in terminal
          |    version      print version, also -v --version 
          |    help         print this message, also -h, --help
          |
          |""".stripMargin

    val welcomeMessage = 
      s"""|Welcome to reqT $reqTVersion $reqTHome
          |Type 'edit' to open a new reqT window.
          |Type ':quit' or press Ctrl+D to exit.
          |Re-run with -h for help on main program args""".stripMargin

    def editMessage = 
      s"Opening new reqT window... MainWindow.nbrWindows=${MainWindow.nbrWindows}"

    /** start a new editor window */
    def edit: Unit = edit()

    /** start a new editor window for each file in args */
    def edit(args: String*): Unit = 
      if args.isEmpty then MainWindow.newWindow(initFileName())
      else try 
        for f <- args do
          MainWindow.newWindow(f)
          SwingPlatform.runInSwingThread(println(editMessage))
      catch 
        case e: Throwable => 
          val msg = s"Exception on edit: $e\n\nStack Trace:${e.getStackTrace().mkString("\n")}"
          println(msg)
          SwingPlatform.runInSwingThread: // log exception in all open windows 
            for i <- 0 until MainWindow.nbrWindows do 
              MainWindow.get(i).map(w => w.log(msg))
    end edit

    /** start repl in terminal **/
    def repl: Unit = repl()

    def pathToMyJar: os.Path = os.Path(reqt.Main.getClass.getProtectionDomain().getCodeSource().getLocation().toURI)

    def findJarOrExit: os.Path = 
      val selected = 
        if os.exists(pathToMyJar) then pathToMyJar 
        else 
          (listReqTJars(os.pwd) ++ listReqTJars(os.home) ++ listReqTJars(os.home/"reqT"))
            .headOption.getOrElse(os.pwd / "reqT.jar")

      if os.exists(selected) then selected 
      else
        println(s"Cannot find $selected\n Download reqT.jar from $reqTHome and place it here ${os.pwd}")
        sys.exit(1)  // bail out with error
        selected
    end findJarOrExit

    def replCmd(initScript: String, quote: Boolean = false) = 
      val tweakedInitScript = if quote then s"\"$initScript\"" else initScript
      Seq("scala", "repl", "-S", Main.scalaVersion, "--jar", findJarOrExit.toString, "--","--repl-init-script", s"$tweakedInitScript")

    /** List .jar files starting with reqt (case-insensitive) in p.*/
    def listReqTJars(p: os.Path): Seq[os.Path] = 
      if os.exists(p) then
        os.list(p)
          .filter(f => f.last.endsWith(".jar") && f.last.toLowerCase.startsWith("reqt"))
          .sorted.reverse
      else Seq()
    end listReqTJars

    /** start repl in terminal with initScript and args **/
    def repl(initScript: String = replInitScript, args: String*): Unit =

      val tryStartRepl = scala.util.Try: 
        println(s"\n$welcomeMessage\n")
        println(replCmd(initScript, quote = true).mkString(" "))
        os.proc(replCmd(initScript))
          .call(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
      
      if tryStartRepl.isFailure then 
        println(s"ERROR: Failed to start reqT $reqTVersion repl with Scala $scalaVersion")
        println(s"More information here: $reqTHome")
        println(s"$tryStartRepl")
        sys.exit(1) // bail out with error

    end repl 

    def update(): Unit = 
      println(s"You are running reqT $reqTVersion from $pathToMyJar")
      val latest = getLatestVersion()
      if reqTVersion == latest 
      then println(Console.GREEN + "You have latest version :)" + Console.RESET) 
      else
        val wantUpdate = 
          val input = Option(io.StdIn.readLine(s"Update to reqT $latest\nY/n? ")).getOrElse("Y")
          input.toLowerCase.startsWith("y")

        if wantUpdate then 
          val pathToNewJar = os.Path(pathToMyJar.segments.toSeq.dropRight(1).appended(s"reqT.jar").mkString("/", "/", ""))
          val isOk = if !os.exists(pathToNewJar) then true else
            val input = Option(io.StdIn.readLine(s"File exists: $pathToNewJar\nOverwrite Y/n? ")).getOrElse("Y")
            input.toLowerCase.startsWith("y")
          
          if isOk then 
            val msg = if os.exists(pathToNewJar) then "Replacing" else "New file"
            println(s"Downloading reqT.jar from $reqTDownload\n$msg: $pathToNewJar")
            println(s"  ... ... ...")
            val online = java.net.URI(reqTDownload).toURL().openStream() //java.net.URL(reqTDownload).openStream()
            try
              java.nio.file.Files
                .copy(online, pathToNewJar.toNIO, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
            finally online.close()
          else println(s"Aborting.")
        else println(s"Aborting.")
    end update

    /** Main program of reqT
     * Accepts these command line args: 
     *   <none>  start the reqT Swing QUI
     *   help    print help on main program args
     *   version print version message in terminal 
     *   repl    start reqT in the scala repl in terminal
     *   quiz    start a quiz game in terminal
     *   update  download reqT.jar to working dir
    */
    def main(args: Array[String]): Unit = 
      if args.isEmpty || args(0) == "edit" then edit(args.toSeq.drop(1)*) 
      else args(0) match
        case "repl" => repl(replInitScript, args.toSeq.drop(1)*)
        case "quiz" => quizGame()
        case "version" | "-v" | "--version" => println(s"reqT version: $reqTVersion $reqTHome") 
        case "update" => update()
        case "help" | "-h" | "--help"=> println(helpMessage)
        case _ => println(s"Unknown args: ${args.mkString(",")}\n  use arg 'help' for help")

    object quizGame:
      var N = 0

      def ask(n: Int) = 
        N += 1
        val (questLines, correct) = quiz.generateQuestion(n)
        println(s"\n--- Quiz number $N \n \n")
        println(questLines.mkString("\n"))
        val allowed = correct.sorted.mkString
        val input = util.Try(
          io.StdIn.readLine(
            s"\nAnswer letters ${correct.sorted.mkString} in correct order or just Enter or Ctrl+D to quit\n> "
          ).distinct
        ).getOrElse("")
        if input.isEmpty then -1 else
          val points = correct.zipWithIndex.map((c, i) => if Some(c) == input.lift(i) then 1 else 0).sum
          println:
            s"""|  Filtered distinct: ${input.filter(c => correct.contains(c))}
                |  Correct answer:    ${correct.mkString}
                |  You got $points of $n points!
                |""".stripMargin.stripTrailing
          points
        end if
      end ask

      def apply(nbrConceptsPerQuestion: Int = 5) = 
        println("\n*** Welcome to the reqT entity quiz!\n")
        var tot = 0 
        var max = 0 
        var continue = true
        while continue do 
          val points = ask(nbrConceptsPerQuestion)
          if points == -1 then continue = false 
          else
            tot += points
            max += nbrConceptsPerQuestion
            println(s"--- Your current total is $tot out of $max")
        end while
        println(s"Goodbye champion!\nYour score is $tot out of $max")
    end quizGame
  end Main
  
end reqt





  