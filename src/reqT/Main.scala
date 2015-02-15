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

object Main {

  def help() {
    println("reqT is a requriements engineering tool, visit http://reqT.org")
    println("<no arg> Start the reqT shell inside the Scala Read-Evaluate-Print-Loop")
    println("help     -h  Print this message")
    println("edit     -e  <file> Launch the reqT shell editor with <file> if any")
    println("init     -i  <file> Interpret <file> before starting the reqT shell")
    println("test     -t  <file> Run test script with Model in file")
    println("meta     -m  [<from>] Generate metamodel [<from>] to file: GENERATED-metamodel.scala")
    println("flex     -f  Print jflex clauses to file: reqT-flex-clauses.txt")
  }

  def main(args : Array[String]) : Unit =  {
    if (args.size == 0) repl.startInterpreting
    else args(0) match {
      case a if Set("help", "--help", "-h", "-help", "help", "?")(a) => help()
      case a if Set("edit", "-e")(a) => repl.initInterpreterAndEdit(args.drop(1))
      case a if Set("init", "-i")(a)   => interpretFile(args.drop(1))
      case a if Set("flex", "-f")(a) => genJFlex(args.drop(1))
      case a if Set("meta", "-m")(a) => genMeta(args.drop(1))
      case a if Set("test", "-t")(a) => test(args.drop(1))
      case _ => 
        println("ERROR Unknown arg: " + args.mkString(" ")); help()
    }
  }
  
  def genMeta(args : Array[String]) {
        println(s"Generating metamodel ${args.mkString(",")} ...")
        if (args.isEmpty) reqT.meta.gen().save("GENERATED-metamodel.scala")
        else repl.initInterpreterAndRun { 
          val q3 = "\"\"\"" // trippel " avoids trouble when backslash in file names
          s"""
            val modelString = load(${q3}${args(0)}${q3})
            repl.interpretModel(modelString) match {
              case Some(model) => reqT.meta.gen(model).save("GENERATED-metamodel.scala")
              case _ => println("ERROR in reqT.Main.genMeta: interpretation failed.")
            }
          """
        }
  }
  
  def interpretFile(args : Array[String]) {
    if (args.isEmpty) println("ERROR no file; usage reqT -i <filename>")
    args.foreach { s => println(s"interpreting file: $s "); repl.interpretFile(s) } 
    println("")
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
  
  def genJFlex(args : Array[String]) {
  
    val ent = reqT.metamodel.entityTypes.map(e => s""" "$e" """).
      mkString(" /* Entity Types */ \n", "| \n", "{ addToken(Token.DATA_TYPE); } \n")    
    
    val attr = reqT.metamodel.attributeTypes.map(e => s""" "$e" """).
      mkString(" /* Attribute Types */ \n", "| \n", "{ addToken(Token.RESERVED_WORD_2); } \n")
    
    val rel = reqT.metamodel.relationTypes.map(e => s""" "$e" """).
      mkString(" /* Relation Types */ \n", "| \n", "{ addToken(Token.FUNCTION); } \n")
    
    (s"$ent\n$attr\n$rel").save("reqT-flex-clauses.txt")
  }
  

  
}
