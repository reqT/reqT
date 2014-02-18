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
/*
scalac Main.scala
jar cfe reqT.jar reqT.Main reqT
scala reqT.jar
*/

object Main {
  def main(args : Array[String]) : Unit =  {
    if (args.size == 0) ReadEvaluatePrintLoop.startInterpreting
    else args(0) match {
      case "hello" => println("hello reqT")
      case arg => println("Unknown args: " + args.mkString(" "))
    }
  }
  
}
