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

package object metaprog {
  def makeMetamodel(m: Model = default.metamodel): String =
    new MetaReqT(m).toScala
}

package metaprog {
  //this file contains input to MetamodelGenerator

  object default {
    val metamodel = Model(
      Ent("entities") has (
        Seq("General","Context","Requirement").map(Ent(_) is Ent("Entity")) ++
        Seq("Item","Label","Section").map(Ent(_) is Ent("General")) ++
        Seq("Actor","Product","Release","Resource","Stakeholder","Subdomain","System").
          map(Ent(_) is Ent("Context")) ++
        Seq("GeneralReq","IntentionalReq","FunctionalReq", "QualityReq", "ScenarioReq").map(Ent(_) is Ent("Requirement")) ++
        Seq("Req", "Idea", "Feature").map(Ent(_) is Ent("GeneralReq")) ++
        Seq("Goal","Wish").map(Ent(_) is Ent("IntentionalReq")) ++
        Seq("Function","Interface","Design").map(Ent(_) is Ent("FunctionalReq")) ++
        Seq("Quality","Target","Barrier").map(Ent(_) is Ent("QualityReq")) ++
        Seq("Scenario","Task","TestCase","UserStory","UseCase").
          map(Ent(_) is Ent("ScenarioReq")) :_*
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
      Ent("enumDefaults") has (
        Ent("Cardinality") has Val("NoOption")             
      )
    )
  }
  
  class MetaReqT(val model: Model) extends MetamodelGenerator {
    import scala.collection.immutable.ListMap
    
    override val enums = 
      ListMap((model / "enums").tipIds.map(id=> (id, (model / "enums" / id).tipIds)) :_*)

    val attr = (model / "attributes").reverse(is,has)
    override val attributes = ListMap(attr.tipIds.map(id => (id, attr / id tipIds)):_*)
    
    val defaults = model / "enumDefaults"
    override val attributeDefault = 
      ListMap(defaults.tipIds.map(id => (id, defaults / id / Val)):_*)

    override val generalEntities = model / "entities" *~ "General" tipIds
    override val contextEntities = model / "entities" *~ "Context" tipIds
    
    val reqs = model / "entities" *~ "Requirement" tipIds 
    override val requriementEntities = 
      ListMap(reqs.map(r => (r,(model / "entities" *~ r).tipIds)):_*)
      
    override val defaultEntity = Ent
    override val defaultAttribute = Val
    override val defaultRelations = Vector(has, is)
    
    override val relations = model / "relations" tipIds
  }
}

