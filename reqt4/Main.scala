package reqt

import dotty.tools.repl.{*, given}

val replDriver =  OpenReplDriver(Array("-usejavacp"), prompt = "reqt4> ")

object Main:
  def main(args: Array[String]): Unit = 
    println("*** Hello reqt4 ***")
    replDriver.tryRunning
