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
import scala.language.postfixOps

package object metameta extends MetaModel

package metameta {
  //this file contains input to the MetaGen metamodel generator

  
  trait HasModel { def model: Model }

  trait MetaModel extends HasModel {
    val model = Model(
      Ent("entities") has (
        Seq("General", "Context", "Requirements").map(Ent(_) is Ent("Entity")) ++
        Seq("Section", "Item", "Label").map(Ent(_) is Ent("General")) ++
        Seq("Stakeholder","Product","System", "Subdomain").map(Ent(_) is Ent("Context")) ++
        Seq("GeneralReq","IntentionalReq").map(Ent(_) is Ent("Requirement")) ++
        Seq("Req", "Idea", "Feature").map(Ent(_) is Ent("GeneralReq")) ++
        Seq("Goal","Wish").map(Ent(_) is Ent("IntentionalReq")) :_*
      ),
      Ent("relations") has 
        Seq("requires","relatesTo").map(Ent(_) is Ent("Relation")).toModel,
      Ent("attributes") has (
        Seq("Gist", "Spec", "Text", "Title").map(Ent(_) is Ent("String")) ++
        Seq("Prio", "Cost").map(Ent(_) is Ent("Int")) ++
        Seq(Ent("Opt") is Ent("Cardinality")) :_*
      ), 
      Ent("enums") has (
        Ent("Cardinality") has (
          Ent("NoOption"), Ent("Zero"), Ent("One"), Ent("ZeroOrOne"), Ent("OneOrMany"), Ent("ZeroOrMany")
        )
      ),
      Ent("defaults") has (
        Ent("String") has Attr("\"???\""),
        Ent("Int") has Attr("-99999999"),
        Ent("Cardinality") has Attr("NoOption")             
      )
    )
// reqT> (m2 / "entities" *~ "Entity" ^^) .ids
// res12: Vector[String] = Vector(General, Context, Requirements)

// reqT> m2.enter("entities").restrictTails("Entity" ).tip.ids
// res13: Vector[String] = Vector(General, Context, Requirements)      
  
    lazy val oldModel = { Model(
      Ent("Entity") has (
        Ent("General") has List("Section", "Item", "Label").as(Ent).toModel,
        Ent("Context") has List("Stakeholder","Product","System", "Subdomain").as(Ent).toModel, 
        Ent("Requirement") has (
          Ent("GeneralReq") has List("Req", "Idea", "Feature").as(Ent).toModel, 
          Ent("IntentionalReq") has List("Goal","Wish").as(Ent).toModel
        )
      ),
      Ent("Attribute") has (
        Ent("String") has (Ent("Gist"), Ent("Spec"), Ent("Text"), Ent("Title")), 
        Ent("Int") has (Ent("Prio"), Ent("Cost")), 
        Ent("Enum") has (
          Ent("Cardinality") has (
            Ent("attrs") has Ent("Opt"),
            Ent("vals") has (
              Ent("NoOption"), Ent("Zero"), Ent("One"), Ent("ZeroOrOne"), Ent("OneOrMany"), Ent("ZeroOrMany")
            )
          )
        )
      ),
      Ent("Relation") has List("requires","relatesTo").as(Ent).toModel,
      Ent("defaults") has (
        Ent("String") has Attr("\"???\""),
        Ent("Int") has Attr("-99999999"),
        Ent("Cardinality") has Attr("NoOption")      
      )
    )   
  } }

  object make extends  MetaMetamodel {
    import scala.collection.immutable.ListMap
    
    def apply(): String = toScala
    //def apply(m: Model): String = toScala(m)  aaargh! generalize to make it work for any model
    
    
    override val enums = ListMap((model / "enums").tipIds.map(id=> (id, (model / "enums" / id).tipIds)) :_*)
    // override val enums = ListMap(
      // "Cardinality" -> List("NoOption", "Zero", "One", "ZeroOrOne", "OneOrMany", "ZeroOrMany")
    // )
    
    val attr = (model / "attributes").reverse(is,has)
    
    override val attributes = ListMap(attr.tipIds.map(id => (id, attr / id tipIds)):_*)
    // override val attributes = ListMap(
      // "String" -> List("Gist", "Spec", "Text", "Title"),
      // "Int" -> List("Cost", "Prio"),
      // "Cardinality" -> List("Opt")
    // )
    
    val defaults = model / "defaults"
    override val attributeDefault = ListMap(defaults.tipIds.map(id => (id, defaults / id / Attr)):_*)
    // override val attributeDefault = ListMap(
      // "String" -> "\"???\"",
      // "Int" -> "-99999999",
      // "Cardinality" -> "NoOption"
    // )
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

    override val generalEntities = model / "entities" *~ "General" tipIds
    //override val generalEntities = List("Section", "Item", "Label")
    override val contextEntities = model / "entities" *~ "Context" tipIds
    //override val contextEntities = List("Stakeholder","Product","System", "Subdomain")
    
    val reqs = model / "entities" *~ "Requirement" tipIds 
    override val requriementEntities = ListMap(reqs.map(r => (r,(model / "entities" *~ r).tipIds)):_*)
    // override val requriementEntities = ListMap(
      // "GeneralReq" -> List("Req", "Idea", "Feature"),
      // "IntentionalReq" -> List("Goal","Wish")
    // )
    override val defaultEntity = Ent
    override val defaultAttribute = Attr
    
    override val relations = model / "relations" tipIds
    //override val relations = List("requires","relatesTo")
  }
}

