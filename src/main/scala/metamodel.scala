/*     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2014, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/

package reqT 

case class Spec(value: String) extends StringAttribute { override val myType = Spec }
case object Spec extends StringType 

case class Prio(value: Int) extends IntAttribute { override val myType = Prio }
case object Prio extends IntType 

case class Alternative(value: Choice) extends ChoiceAttribute { override val myType = Alternative }
case object Alternative extends ChoiceType 


case class Req(id: String) extends Entity { override val myType: EntityType = Req }
case object Req extends EntityType

case class Feature(id: String) extends Entity { override val myType: EntityType = Feature }
case object Feature extends EntityType

case object has extends Link  
case object requires extends Link
