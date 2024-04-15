package reqt

import os.exists

val scalaVersion = "3.4.1"

def edit(args: Array[String]): Unit = 
    EditorWindow.newWindow()
    SwingPlatform.runInSwingThread:
      println(s"New window started! EditorWindow.nbrWindows=${EditorWindow.nbrWindows}")

def repl(args: Array[String]): Unit =
  val result = scala.util.Try {
    val wdJar   = os.pwd/"reqT.jar"
    val homeJar = os.home/"reqT"/"reqT.jar"
    val url = "https://github.com/reqT/reqT/releases"

    val jar = if os.exists(wdJar) then wdJar else if os.exists(homeJar) then homeJar else
      println(s"You need download reqT.jar from $url and place it in ${os.home/"reqT"}")
      sys.exit(1)

    val cmd = Seq[String]("scala-cli", "repl", "-S", scalaVersion, "--jar", jar.toString)
    println(s"Running command: ${cmd.mkString(" ")}")
    os.proc(cmd).call(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
  }
  if result.isFailure then 
    println(s"$result")
    sys.exit(1)

object Main:
  def main(args: Array[String]): Unit = 
    if args.isEmpty || args(0) == "edit" then edit(args) 
    else if args.lift(0) == Some("repl") then repl(args)
    else println(s"Unknown args: ${args.mkString(",")}")




  