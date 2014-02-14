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

trait MetaMetamodel extends reqT.DSL with MetamodelToScala {
  import scala.collection.immutable.ListMap
  def enums: ListMap[String,List[String]]
  def attributes: ListMap[String,List[String]]
  def attributeDefault: ListMap[String, String]
  def generalEntities: List[String]
  def contextEntities: List[String]
  def requriementEntities: Map[String,List[String]]
  def defaultRelation: String
  def moreRelations: List[String]
}

trait MetamodelToScala {
  self: MetaMetamodel =>

  override def toScala: String = 
    mkPreamble + mkObjectMetamodel + mkEnumTraits + mkAttrTraits + 
      mkConcreteAttrs + mkAbstrReqs + mkConcreteEnts + mkConcreteRels + mkFactoryTraits

  lazy val mkPreamble = reqT.PREAMBLE.replaceAll("\n","\n//") + s"""
// *** THIS IS A GENERATED SOURCE FILE 
// *** Re-generate by reqT> reqT.metameta.model.toScala.save(<filename>)
// *** Generation date: ${(new java.util.Date).toString} 
// *** reqT version $reqT_VERSION build date $BUILD_DATE
"""  

  lazy val mkObjectMetamodel = s"""
object metamodel {
  lazy val types: Vector[MetaType] = = entityTypes ++ attributeTypes ++ relationTypes 
  lazy val entityTypes: Vector[EntityType] = generalEntities ++ contextEntities ++ requirementEntities  
  $mkEntityTypes
  lazy val attributeTypes: Vector[AttributeType[_]] = $mkAttributeTypes
  lazy val relationTypes: Vector[RelationType] = Vector($mkRelationTypes)
}
"""

  lazy val mkEntityTypes = s"""
  lazy val generalEntities = Vector(${generalEntities.mkString(", ")}) 
  lazy val contextEntities = Vector(${contextEntities.mkString(", ")})   
  lazy val requirementEntities = ${reqTypes.mkString(" ++ ")}
  $mkReqVectors
"""
  lazy val mkAttributeTypes = aggregateAttrTypes + attrVectors
  lazy val relations = (defaultRelation::moreRelations)
  lazy val entities = generalEntities ++ contextEntities ++ 
    requriementEntities.collect { case (_, rs) =>  rs  } .flatten
  lazy val mkRelationTypes = relations.mkString(", ")
  lazy val mkReqVectors = 
    reqTypes.map(r => (r, r.decapitalize + "s")).collect { case (r, decap)  =>
      s"""lazy val $decap = Vector(${requriementEntities(r).mkString(", ")})"""
    } .mkString("\n  ")
  lazy val mkEnumTraits = "\n//Enum traits\n" + 
    enums.keysIterator.map(e => enumToScala(e, enums(e), attributeDefault(e))).mkString("\n  ")  
  
  lazy val mkAttrTraits = "//Attribute traits\n" + attributes.collect { 
    case (at, as) => as.map(a => attrTraitToScala(a, at, attributeDefault(at))).mkString 
  } .mkString
  
  lazy val mkConcreteAttrs = "\n//Concrete attributes\n" + attributes.collect { 
    case (at, as) => as.map(a => attrToScala(a, at)).mkString 
  } .mkString
  
  lazy val mkAbstrReqs = "\n//Abstract requirement traits\n" + reqTypes.map(abstractReqToScala).mkString
  
  lazy val mkConcreteEnts = "\n//Concrete entities\n" +
    generalEntities.map(e => caseEntityToScala(e, "General")).mkString +
    contextEntities.map(e => caseEntityToScala(e, "Context")).mkString +
    requriementEntities.collect { case (rt, rs) =>  rs.map(r => caseEntityToScala(r, rt)).mkString } .mkString
  
  lazy val mkConcreteRels = "\n//Concrete relations\n" +
    relations.map(r => s"case object $r extends RelationType").mkString("\n")
    
  lazy val mkFactoryTraits = "//factory traits\n" + 
    mkRelationFactory + mkHeadFactory + mkHeadTypeFactory + mkImplicitFactoryObjects
  
  lazy val reqTypes = requriementEntities.keysIterator.toVector
  lazy val attrTypes = attributes.keysIterator.toVector
  lazy val aggregateAttrTypes = attrTypes.map(a => a.toLowerCase + "Attributes").mkString(" ++ ") + "\n"
  lazy val attrVectors = attrTypes.map( a => 
      s"""  lazy val ${a.toLowerCase}Attributes = Vector(${attributes(a).mkString(", ")})""" ).mkString("\n")
    
  def enumToScala(et: String, values: List[String], default: String) = s"""
trait $et extends Enum[$et] { val myType = $et }
trait ${et}Type extends EnumType[$et] with AttributeType[$et] { 
  val values = Vector(${values.mkString(", ")})
  val default = $default
}
case object $et extends ${et}Type
${values.map(v => s"case object $v extends $et").mkString("\n")}
   
"""

  def attrTraitToScala(at: String, tpe: String, default: String) = s"""
trait ${at}Attribute extends Attribute[$tpe]
trait ${at}Type extends AttributeType[$tpe] { val default = "$default"}
"""  

  def attrToScala(a: String, tpe: String) = s"""
case class $a(value: $tpe) extends ${tpe}Attribute { override val myType = $a }
case object $a extends ${tpe}Type 
"""

  def abstractReqToScala(r: String) = s"""
trait $r extends Requirement
case object $r extends AbstractSelector { type AbstractType = $r } 
"""

  def caseEntityToScala(e: String, extnds: String) = s"""
case class $e(id: String) extends $extnds { override val myType: EntityType = $e }
case object $e extends EntityType
"""

  def mkRelationFactory = s"""
trait RelationFactory {
  self: Entity =>
${relations.map(relationMethod).mkString("\n")}
}
"""  

  def relationMethod(r: String) = s"  def $r(elems: Elem*) = Relation(this, reqT.$r, Model(elems:_*))"

  def mkHeadFactory = s"""
trait HeadFactory {
  self: Entity =>
${relations.map(headMethod).mkString("\n")}
}
"""
  def headMethod(r: String) = s"  def $r = Head(this, reqT.$r)"

  def mkHeadTypeFactory = s"""
trait HeadTypeFactory {
  self: EntityType =>
${relations.map(headTypeMethod).mkString("\n")}
}
"""
  def headTypeMethod(r: String) = s"  def $r = HeadType(this, reqT.$r)"

  def mkImplicitFactoryObjects = s"""
trait ImplicitFactoryObjects extends CanMakeAttr { //mixed in by package object reqT
$mkImplicitAttrMakers
$mkEnumImplicits
  lazy val attributeFromString = Map[String, String => Attribute[_]](
$mkAttrFromStringMappings 
  )
  lazy val entityFromString = Map[String, String => Entity](
$mkEntFromStringMappings
  )
}
"""

  def mkImplicitAttrMakers = attributes.collect { 
    case (at, as) => as.map(a => attrMakerToScala(a, at)).mkString  } .mkString

  def attrMakerToScala(a: String, tpe: String) = s"""
  implicit object make$a extends AttrMaker[$a] { def apply(s: String): $a = $a(s.to$tpe) }"""
  
  def mkEnumImplicits = enums.collect { 
    case (e, _) => attributes(e).map(a => enumImplicits(a, e)).mkString  } .mkString

  def enumImplicits(a: String, e: String) = s"""
  implicit class StringTo$e(s: String) { def to$e = $a.valueOf(s)}
  implicit object make$a extends AttrMaker[$a] { def apply(s: String): $a = $a(s.to$e) }
"""
  def mkAttrFromStringMappings = attributes.collect { 
    case (_, as) => as.map(attrMapping).mkString(",\n")  } .mkString(",\n") 
  
  def mkEntFromStringMappings = entities.map(entMapping).mkString(",\n")
  
  def attrMapping(a: String) = s"""    "$a" -> makeAttr[$a] _ """
  def entMapping(e: String) =  s"""    "$e" -> $e.apply _ """

}


























