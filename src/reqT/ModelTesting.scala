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

//???? this needs to be ckecked after Output and Input was changed to from attr to entities
trait ModelTesting {
  self: Model =>
  def testOutputAdded(): Model = flatMapDeep {
    case Relation(Test(id),l,t) if l == has =>
        val result: Option[String] = t.get(Code).map(Code(_).run)
        Some(Relation(Test(id),l,result.map(t + Output(_)).getOrElse(t)))
    case e => Some(e)
  }  
  lazy val testCode: Map[Test, Code] = 
    entityAttributePairs.collect { case (e: Test, a: Code) => (e,a) } .toMap
  def testOutputMap(): Map[Test, Output] = testCode.map { 
    case (e,c) => (e, Output(c.interpretString.getOrElse("INTERPRETATION ERROR! Fix test bug."))) 
  }
  def test(): Map[Test, Output] = testOutputMap.flatMap { 
    case (e, o) if o.id != "" => println(s"$e: ${o.id}"); Some((e,o)) 
    case (e, o) => println(s"$e OK!"); None
  } .withDefaultValue(Output(""))
}

