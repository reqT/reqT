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
/**
  * reqT.NanoZap -- A minimalistic DSL for testing.
  *
  * ==Example usage==
  {{{
   object myTestCases extends NanoZap {
    def zap: Boolean = test("Arithmetic basics") { 
      "basic addition" .
          test { 1 + 1 == 3 } +
      "associativity of multiplication" .
          test { 2 * 3 == 3 * 2 } 
     }
   }
  }}}  
  {{{
    scala> myTestCases.zap
    NanoZap test(Arithmetic basics) ... ZAPPED!
    *** TEST FAILED: basic addition
    res0: Boolean = false
  }}}
  */
  
trait NanoZap {
  implicit class StringTest(string: String) { 
    def test(isOk: Boolean):String = 
        if (isOk) "" else s"*** TEST FAILED: $string\n" 
  }
  
  def test(name: String)(report: String): Boolean = {
    print(s"NanoZap test($name) ... ")
    if (report.isEmpty) println("Ok!") else println(s"ZAPPED!\n$report")
    report == ""
  }
}

object NanoZap extends NanoZap 
