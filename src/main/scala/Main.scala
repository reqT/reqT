export reqt.Main.edit

package reqt:
  object Main:

    /** start a new editor window */
    def edit: Unit = edit()

    def edit(args: String*): Unit = 
        try MainWindow.newWindow()
        catch 
          case e: Throwable => 
            val msg = s"Exception in edit: $e\n\nStack Trace:${e.getStackTrace().mkString("\n")}"
            println(msg)
            SwingPlatform.runInSwingThread:
              for i <- 0 until MainWindow.nbrWindows do
                MainWindow.get(i).map(w => w.log(msg))
        //SwingPlatform.runInSwingThread:
        //  println(s"New window started! EditorWindow.nbrWindows=${MainWindow.nbrWindows}")

    def repl: Unit = repl()

    def pathToMyJar: os.Path = os.Path(reqt.Main.getClass.getProtectionDomain().getCodeSource().getLocation().toURI)

    def listReqTJars(p: os.Path): Seq[os.Path] = 
      if os.exists(p) then
        os.list(p)
          .filter(f => f.last.endsWith(".jar") && f.last.toLowerCase.startsWith("reqt"))
          .sorted.reverse
      else Seq()

    def repl(args: String*): Unit =
      val tryStartRepl = scala.util.Try: 
        val url = "https://github.com/reqT/reqT/releases"

        val jar = 
          val selected = 
            if os.exists(pathToMyJar) then pathToMyJar 
            else 
              (listReqTJars(os.pwd) ++ listReqTJars(os.home) ++ listReqTJars(os.home/"reqT"))
                .headOption.getOrElse(os.pwd / "reqT.jar")
          if os.exists(selected) then selected 
          else
            println(s"Cannot find $selected\n Download reqT.jar from $url and place it here ${os.pwd}")
            sys.exit(1)

        val cmd = Seq[String]("scala", "repl", "-S", Main.scalaVersion, "--jar", jar.toString)
        println(s"Running command: ${cmd.mkString(" ")}")
        println(s"Type 'edit' to open a new reqT window.")
        println(s"Type 'import reqt.*' for direct access to full api.")
        println(s"See https://github.com/reqT/reqT for more information on how to use reqT.")
        os.proc(cmd).call(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
      
      if tryStartRepl.isFailure then 
        println("ERROR: Failed to start scala repl with reqT on path")
        println(s"$tryStartRepl")
        println(s"\nYou may need to install Scala version ${Main.scalaVersion} or later from here: https://www.scala-lang.org/")
        sys.exit(1)
    end repl 

    val scalaVersion = "3.6.3-RC2"
    val reqTVersion  = "4.4.0"

    /** Main program */
    def main(args: Array[String]): Unit = 
      if args.isEmpty || args(0) == "edit" then edit(args.toSeq.drop(1)*) 
      else args(0) match
        case "version" | "-v" | "--version" => println(s"reqT version $reqTVersion https://github.com/reqT/reqT") 
        case "repl" => repl(args.toSeq.drop(1)*)
        case "quiz" => quiz()
        case _ => println(s"Unknown args: ${args.mkString(",")}")

    object quiz:    
      val n = 5

      var N = 0

      def selectRandom(pairs: Seq[(String, String)]) = util.Random.shuffle(pairs).take(n).zipWithIndex.toVector

      def ask() = 
        N += 1

        val selected: Seq[((String, String), Int)] = selectRandom(meta.entityConcepts)

        val secondShuffle: Seq[(String, Int, String)] = 
          val xs = util.Random.shuffle: 
            selected.map((p, i) => p._1 -> i)
          (0 until xs.length).map(i => (xs(i)._1, xs(i)._2, selected(i)._1._2))

        extension (i: Int) def toChoice: Char = ('a' + i).toChar

        val correct = secondShuffle.map(_._2.toChoice)

        val pad = secondShuffle.map(_._1.length).max 

        val questLines = secondShuffle.zipWithIndex.map: 
          case ((concept, correctOrder, defInWrongPlace), i) =>
            s"${concept.padTo(pad, ' ')}  ${i.toChoice}: ${defInWrongPlace.takeWhile(_ != '.')}."
        
        println(s"\n--- Quiz number $N \n \n")
        println(questLines.mkString("\n"))
        
        val allowed = correct.sorted.mkString
        val input = util.Try(
          io.StdIn.readLine(
            s"\nAnswer letters ${correct.sorted.mkString} in correct order or just Enter to quit\n> "
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

      def apply() = 
        println("\n*** Welcome to the reqT entity quiz!\n")
        var tot = 0 
        var max = 0 
        var continue = true
        while continue do 
          val p = ask()
          if p == -1 then 
            continue = false 
          else
            tot += p
            max += n
            println(s"Your current total is $tot out of $max")
        end while
        println(s"Goodbye champion!\nYour score is $tot out of $max")





  