package reqt

import dotty.tools.repl.{*, given}

val replDriver =  OpenReplDriver(Array("-usejavacp"), prompt = "reqt4> ")

var finalState:  Option[State] = None

object Main:
  def main(args: Array[String]): Unit = 
    println("*** Hello reqt4 ***")
    finalState = replDriver.tryRunning()
