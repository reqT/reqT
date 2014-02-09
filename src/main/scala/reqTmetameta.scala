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

object reqT extends MetaMetamodel {
  val enumTypes = Map(
    "Cardinality" -> List("NoOption", "Zero", "One", "ZeroOrOne", "OneOrMany", "ZeroOrMany")
  )
  val attributesAndDefaults = Map(
    "String" -> "\"???\"",
    "Int" -> "-99999999",
    "Cardinality" -> "NoOption"
  )
  val generalEntities = List("Titel", "Text", "Item", "Label")
  val contextEntities = List("Stakeholder","Product","System")
  val requirementEntities = Map(
    "Generic" -> List("Req", "Idea", "Feature"),
    "Intentional" -> List("Goal","Wish")
  )
  val defaultRelation = "has"
  val moreRelations = List("requires","relatesTo")
}



