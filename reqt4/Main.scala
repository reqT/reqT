package reqt

import dotty.tools.repl.{*, given}

val replDriver =  //OpenReplDriver(Array("-usejavacp"), prompt = "reqt4> ")
  ReplDriver(Array("-usejavacp"))

// var finalState:  Option[State] = None

val result: Array[Any] = new Array(1)
val test = 42
// def getResult[T]: T = result(0).asInstanceOf[T]

object Main:
  def main(args: Array[String]): Unit = 
    println("*** Hello reqt4 ***")
    //finalState = 
    replDriver.tryRunning
