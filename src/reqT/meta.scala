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

package object meta {
  def gen(m: Model = model): String = new MetaReqT(m).toScala
  lazy val model = Model(
    Meta("Entity") superOf (
      Meta("General") superOf (
        Meta("Item"), Meta("Label"), Meta("Section")),
      Meta("Context") superOf (
        Meta("Actor"), Meta("Product"), Meta("Release"), Meta("Resource"), 
        Meta("Stakeholder"), Meta("Subdomain"), Meta("System")),
      Meta("Requirement") superOf (
        Meta("GeneralReq") superOf (
          Meta("Req"), Meta("Idea"), Meta("Feature"), Meta("Goal")), 
        Meta("FunctionalReq") superOf (
          Meta("Function"), Meta("Interface"), Meta("Design")), 
        Meta("QualityReq") superOf (
          Meta("Quality"), Meta("Target"), Meta("Barrier")), 
        Meta("ScenarioReq") superOf (
          Meta("Scenario"), Meta("Task"), Meta("TestCase"), 
          Meta("UserStory"), Meta("UseCase")))),
    Meta("RelationType") superOf (
      Meta("requires"),Meta("relatesTo")),
    Meta("Attribute") superOf (
      Meta("StringAttribute") superOf (
        Meta("Text"), Meta("Title"),Meta("Spec"), Meta("Gist"), Meta("Why"), 
        Meta("Example"), Meta("Input"), Meta("Output"), Meta("Expectation")),
      Meta("IntAttribute") superOf (Meta("Prio"), Meta("Cost")),
      Meta("CardinalityAttribute") superOf (Meta("Opt"))), 
    Meta("enums") has (
      Meta("Cardinality") has (
        Meta("NoOption"), Meta("Zero"), Meta("One"), 
        Meta("ZeroOrOne"), Meta("OneOrMany"), Meta("ZeroOrMany"))),
    Meta("enumDefaults") has (
      Meta("Cardinality") has Meta("NoOption")))  
}

package meta {
  class MetaReqT(val model: Model) extends MetaGen {
    import scala.collection.immutable.ListMap
    
    override val enums = 
      ListMap((model / "enums").tipIds.map(id=> (id, (model / "enums" / id).tipIds)) :_*)

    val attr = model/Meta("Attribute").superOf 
    override val attributes = 
      ListMap(attr.tipIds.map(id => 
        (id.replace("Attribute",""), attr / Meta(id).superOf tipIds)):_*) 
        
    val defaults = model / "enumDefaults"
    override val attributeDefaultValues = 
      ListMap(defaults.tipIds.map(id => (id, (defaults / id).tipIds.head)):_*)
    
    val ents = model/Meta("Entity").superOf
    override val generalEntities = ents/Meta("General").superOf tipIds

    override val contextEntities = ents/Meta("Context").superOf tipIds
    
    val reqs = ents/Meta("Requirement").superOf  

    override val requriementEntities = 
      ListMap(reqs.tipIds.map(r => (r,(reqs/Meta(r).superOf).tipIds)):_*)
      
    override val defaultEntities = Vector(Ent, Meta)
    override val defaultAttributes = Vector(Attr, Code)
    override val defaultInterpretedAttributes = Vector(Constraints)
    override val defaultRelations = Vector(has, is, superOf)
    
    override val relations = model / Meta("RelationType").superOf tipIds
  }
}

