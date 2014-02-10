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
package metameta 

//this file contains input to the MetaGen metamodel generator

object model extends MetaMetamodel {
  import scala.collection.immutable.ListMap
  
  override val enums = ListMap(
    "Alternative" -> List("NoAlternative", "Zero", "One", "ZeroOrOne", "OneOrMany", "ZeroOrMany")
  )
  override val attributes = ListMap(
    "String" -> List("Gist", "Spec", "Text", "Title"),
    "Int" -> List("Cost", "Prio"),
    "Alternative" -> List("Opt")
  )
  override val attributeDefault = ListMap(
    "String" -> "\"???\"",
    "Int" -> "-99999999",
    "Alternative" -> "NoAlternative"
  )
  override val generalEntities = List("Section", "Item", "Label")
  override val contextEntities = List("Stakeholder","Product","System", "Subdomain")
  override val requriementEntities = ListMap(
    "GeneralReq" -> List("Req", "Idea", "Feature"),
    "IntentionalReq" -> List("Goal","Wish")
  )
  override val defaultRelation = "has"
  override val moreRelations = List("requires","relatesTo")
}



