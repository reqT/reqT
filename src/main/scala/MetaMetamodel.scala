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

trait MetaMetamodel extends MetamodelGenerators {
  def enumTypes: Map[String,List[String]]
  def attributesAndDefaults: Map[String, String]
  def generalEntities: List[String]
  def contextEntities: List[String]
  def requirementEntities: Map[String,List[String]]
  def defaultRelation: String
  def moreRelations: List[String]
  
  def generate: String = mkObjectMetamodel
  
}

trait MetamodelGenerators {
  self: MetaMetamodel =>
  def mkEntityTypes = "???"
  def mkAttributeTypes = "???"
  def mkObjectMetamodel = s"""
object metamodel {
  lazy val names: Vector[String] = types map (_.toString)
  lazy val indexOf: Map[String, Int] = names.zipWithIndex.toMap.withDefaultValue(-1)
  lazy val types: Vector[Type] = = entityTypes ++ attributeTypes ++ relationTypes 
  lazy val entityTypes: Vector[EntityType] = ${mkEntityTypes}
  lazy val attributeTypes: Vector[AttributeType[_]] = ${mkAttributeTypes}
  lazy val relationTypes: Vector[RelationType] = Vector(${(defaultRelation::moreRelations).mkString(",")})
}
  """  
}
