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

package object metameta extends MetaModel

package metameta {

  //this file contains input to the MetaGen metamodel generator

  trait HasModel { def model: Model }

  trait MetaModel extends HasModel {
    lazy val model = Model(
      Type("Entity") has (
        Type("General") has  (Type("Section"), Type("Item"), Type("Label")),
        Type("Context") has (), 
        Type("Requirement") has ()
      ),
      Type("Attribute") has (
        Type("String") has (Type("Gist"), Type("Spec"), Type("Text"), Type("Title")), 
        Type("Int") has (Type("Prio"), Type("Cost")), 
        Type("Enum") has (
          Type("Cardinality") has (
            Type("NoOption"), Type("Zero"), Type("One"), Type("ZeroOrOne"), Type("OneOrMany"), Type("ZeroOrMany")
          )
        )
      ),
      Type("Relation") has (),
      Type("defaultValues") has (
        Type("String") has Val("\"???\""),
        Type("Int") has Val("-99999999"),
        Type("Cardinality") has Val("NoOption")      
      )
    )   
  }

  object generate extends  MetaMetamodel {
    import scala.collection.immutable.ListMap
    
    val enums = {
      val m = model / "Attribute" / "Enum"
      ListMap((m.topIds).map(id => (id , (m / id).topIds)):_*)
    }

    override val attributes = ListMap(
      "String" -> List("Gist", "Spec", "Text", "Title"),
      "Int" -> List("Cost", "Prio"),
      "Cardinality" -> List("Opt")
    )
    override val attributeDefault = ListMap(
      "String" -> "\"???\"",
      "Int" -> "-99999999",
      "Cardinality" -> "NoOption"
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
}

