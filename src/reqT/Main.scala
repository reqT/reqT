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
    println("--help  -h   Print this message")
    println("--in    -i   <file> Interpret file")
    println("--test  -t   <file> Run test script with Model in file")
    println("--meta  -m   [<from>] Generate metamodel [<from>] to GENERATED-metamodel.scala")
    println("--jflex -j   Print jflex clauses for ReqTTokenMaker.flex")
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
      mkString(" /* Entity Types */ \n", "| \n", "{ addToken(Token.RESERVED_WORD); } \n")    
    
    val attr = reqT.metamodel.attributeTypes.map(e => s""" "$e" """).
      mkString(" /* Attribute Types */ \n", "| \n", "{ addToken(Token.RESERVED_WORD_2); } \n")
    
    val rel = reqT.metamodel.relationTypes.map(e => s""" "$e" """).
      mkString(" /* Relation Types */ \n", "| \n", "{ addToken(Token.FUNCTION); } \n")
    
    println(s"$ent\n$attr\n$rel")
  }
  
  def main(args : Array[String]) : Unit =  {
    if (args.size == 0) repl.startInterpreting
    else args(0) match {
      case a if Set("--hello", "--help", "-h", "-help", "help", "?")(a) => help()
      case a if Set("--meta", "-m")(a) => genMeta(args.drop(1))
      case a if Set("--in",   "-i")(a)   => interpretFile(args.drop(1))
      case a if Set("--test", "-t")(a) => test(args.drop(1))
      case a if Set("--jflex", "-j")(a) => genJFlex(args.drop(1))
      case _ => 
        println("ERROR Unknown arg: " + args.mkString(" ")); help()
    }
  }
  
}
