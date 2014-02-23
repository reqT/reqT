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

trait ModelTesting {
  self: Model =>
  def test = flatMapDeep {
    case Relation(TestCase(id),l,t) if l == has =>
        val result: Option[String] = t.get(Code).map(Code(_).run)
        Some(Relation(TestCase(id),l,result.map(s => t + Output(s)).getOrElse(t)))
    case e => Some(e)
  }  
}

