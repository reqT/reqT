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

object Main {

  def help() {
    println("reqT is a requriements engineering tool, visit http://reqT.org")
    println("<no arg>     Start reqT inside the Scala Read-Evaluate-Print-Loop")
    println("--help       -h   Print this message")
    println("--interpret  -i   <file> Interpret file")
    println("--test       -t   <file> Test script with reqT.Model")
    println("--meta       -m   Generate metamodel to file GENERATED-metamodel.scala")
  }
  
  def genmeta() {
        println("Generating metamodel...")
        reqT.meta.gen().save("GENERATED-metamodel.scala")
  }
  
  def interpretFile(args : Array[String]) {
    println("reqT --interpret ") 
    if (args.isEmpty) println("ERROR no file; usage reqT -i <filename>")
    args.map { s => print(s"  $s "); repl.interpretFile(s) } .collect { 
      case Some(true) => println("DONE!")
      case _ => println("FAILED!")
    }
  }

  def test(args : Array[String]) {
    println("** reqT firing up ...") 
    if (args.isEmpty) println("ERROR no file; usage reqT -i <filename>")
    repl.initInterpreterAndRun(s"""
      val modelFileNames: Vector[String] = Vector(${args.map( f => f.toScala ).mkString(",")})
      println("** Test model files:  " + modelFileNames.mkString(","))
      modelFileNames .map( f => 
        repl.interpretModel(load(f)) .getOrElse { throw new Error(
          "INTERPRETATION ERROR! script must evaluate to valid reqT.Model" 
          ) ; Model() 
        }
      ) .foreach ( _.test )
    """) 
  }
  
  def main(args : Array[String]) : Unit =  {
    if (args.size == 0) repl.startInterpreting
    else args(0) match {
      case arg0 if Set("--hello", "--help", "-h", "-help", "help", "?")(arg0) => help()
      case arg0 if Set("--meta", "-m")(arg0)            => genmeta()
      case arg0 if Set("--interpret", "-i")(arg0)       => interpretFile(args.drop(1))
      case arg0 if Set("--test", "-t")(arg0)       => test(args.drop(1))
      case arg0 => println("ERROR Unknown arg: " + args.mkString(" ")); help()
    }
  }
  
}
