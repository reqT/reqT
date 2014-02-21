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

object specification extends Testable {
  import reqT._
  
  def spec = Model(
    TestCase("addAttr") has (
      Code(""" Model(Spec("x")) + Model(Spec("y")) == Model(Spec("y")) """),
      Expectation("true")
    )
  )
  
}

  