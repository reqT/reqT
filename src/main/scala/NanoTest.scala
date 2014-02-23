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
  * A minimalistic DSL for test case scripting.
  *
  * ==Example usage==
  * {{{
  *  object test extends reqT.nanoTest {
  *    def report(): String = { 
  *      "Add empty"          .test {   
  *          Model() ++ Model() <==> Model()
  *        } +
  *    "Add same attribute type should overwrite" .test {   
  *        Model(Spec("a")) ++ Model(Spec("b")) <==> Model(Spec("b"))  
  *      } +
  *    "Add same entity"    .test { 
  *        Model(Ent("a")) ++ Model(Ent("b")) <==> Model(Ent("a"),Ent("b")) 
  *      }
  *    }
  *  }
  * }}}  
  */
  
trait NanoTest {
  
  /** A wrapper of @param isOk being true if test is Ok. */
  case class TestVerdict(isOk: Boolean)
  
  /** Enriches @param anything with method '''shouldEqual''' aka <==> */
  implicit class AnyShouldEqual(anything: Any) { 
    /** Factory method for test verdicts. Same as '''<==>''' */
    def shouldEqual(that: Any) = TestVerdict(anything == that) 

    /** Factory method for test verdicts. Same as '''shouldEqual'''*/
    def <==> (that: Any) = shouldEqual(that) 
  }
  
  /** Enriches any @param string with method '''test''' 
    * being silent or *loud* depending on TestVerdict 
    */
  implicit class StringTest(string: String) { 
    def test(tv: TestVerdict):String = if (tv.isOk) "" else s"*** TEST FAILED: $string\n" 
  }

}

  