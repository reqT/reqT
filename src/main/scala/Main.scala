package reqt

def edit: Unit = edit()

def edit(args: String*): Unit = 
    EditorWindow.newWindow()
    SwingPlatform.runInSwingThread:
      println(s"New window started! EditorWindow.nbrWindows=${EditorWindow.nbrWindows}")


def repl: Unit = repl()

def repl(args: String*): Unit =
  val result = scala.util.Try {
    val wdJar   = os.pwd/"reqT.jar"
    val homeJar = os.home/"reqT"/"reqT.jar"
    val url = "https://github.com/reqT/reqT/releases"

    val jar = if os.exists(wdJar) then wdJar else if os.exists(homeJar) then homeJar else
      println(s"You need download reqT.jar from $url and place it in ${os.home/"reqT"}")
      sys.exit(1)

    val cmd = Seq[String]("scala-cli", "repl", "-S", Main.scalaVersion, "--jar", jar.toString)
    println(s"Running command: ${cmd.mkString(" ")}")
    println(s"Type 'import reqt.*' for direct access to full api.")
    println(s"Type 'edit' to open an editor window.")
    println(s"Type 'help' for more information on how to use reqT.")
    os.proc(cmd).call(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
  }
  if result.isFailure then 
    println(s"$result")
    println(s"\nInstall scala-cli from here: https://scala-cli.virtuslab.org/install")
    sys.exit(1)

object Main:
  val scalaVersion = "3.4.1"

  def main(args: Array[String]): Unit = 
    if args.isEmpty || args(0) == "edit" then edit(args.toSeq.drop(1)*) 
    else if args.lift(0) == Some("repl") then repl(args.toSeq.drop(1)*)
    else println(s"Unknown args: ${args.mkString(",")}")




  