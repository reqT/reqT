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
        Type("General") has List("Section", "Item", "Label").toModelOf(Type),
        Type("Context") has List("Stakeholder","Product","System", "Subdomain").toModelOf(Type), 
        Type("Requirement") has (
          Type("GeneralReq") has List("Req", "Idea", "Feature").toModelOf(Type), 
          Type("IntentionalReq") has List("Goal","Wish").toModelOf(Type)
        )
      ),
      Type("Attribute") has (
        Type("String") has (Type("Gist"), Type("Spec"), Type("Text"), Type("Title")), 
        Type("Int") has (Type("Prio"), Type("Cost")), 
        Type("Enum") has (
          Type("Cardinality") has (
            Type("attrs") has Type("Opt"),
            Type("vals") has (
              Type("NoOption"), Type("Zero"), Type("One"), Type("ZeroOrOne"), Type("OneOrMany"), Type("ZeroOrMany")
            )
          )
        )
      ),
      Type("Relation") has List("requires","relatesTo").toModelOf(Type),
      Type("defaults") has (
        Type("String") has Val("\"???\""),
        Type("Int") has Val("-99999999"),
        Type("Cardinality") has Val("NoOption")      
      )
    )   
  }

  object make extends  MetaMetamodel {
    import scala.collection.immutable.ListMap
    
    val attr = model / "Attribute" - "Enum"
    val enum = model / "Attribute" / "Enum"
    
    override val enums = ListMap((enum.topIds).map(id => (id , (enum / id / "vals").topIds)):_*)
    // override val enums = ListMap(
      // "Cardinality" -> List("NoOption", "Zero", "One", "ZeroOrOne", "OneOrMany", "ZeroOrMany")
    // )
    
    override val attributes = ListMap(
      "String" -> (attr / "String").topIds,   
      "Int"    -> (attr / "Int")   .topIds
    ) ++ ListMap((enum.topIds).map(id => (id , (enum / id / "attrs").topIds)):_*)
    // override val attributes = ListMap(
      // "String" -> List("Gist", "Spec", "Text", "Title"),
      // "Int" -> List("Cost", "Prio"),
      // "Cardinality" -> List("Opt")
    // )
    
    override val attributeDefault = ListMap(
      "String" -> "\"???\"",
      "Int" -> "-99999999",
      "Cardinality" -> "NoOption"
    )
    val subentities = ListMap(
      "General" -> List("Section", "Item", "Label"),
      "Context" -> List("Stakeholder","Product","System", "Subdomain")
    )
    val subsubentities = ListMap(
      "Requirements" -> ListMap(
        "GeneralReq" -> List("Req", "Idea", "Feature"),
        "IntentionalReq" -> List("Goal","Wish")
      )
    )
// reqT> val m = reqT.metameta.model    
// reqT> (m / "Entity").topIds .flatMap (id => if ((m / "Entity" / id).isDeep) Some(id) else None)
// res27: scala.collection.immutable.Vector[String] = Vector(Requirement)

// reqT> (m / "Entity").topIds .flatMap (id => if (!(m / "Entity" / id).isDeep) Some(id) else None)
// res28: scala.collection.immutable.Vector[String] = Vector(General, Context)
    
    override val generalEntities = List("Section", "Item", "Label")
    override val contextEntities = List("Stakeholder","Product","System", "Subdomain")
    override val requriementEntities = ListMap(
      "GeneralReq" -> List("Req", "Idea", "Feature"),
      "IntentionalReq" -> List("Goal","Wish")
    )
    override val defaultEntity = Type
    override val defaultAttribute = Val
    override val relations = List("requires","relatesTo")
  }
}

