/****************************************************************     
**                  _______        
**                 |__   __|     reqT API  
**   _ __  ___   __ _ | |        (c) 2011-2013, Lund University  
**  |  __|/ _ \ / _  || |        http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
*****************************************************************/

package reqt {

  import scala.collection.immutable.{SortedSet, SortedMap, MapLike}
  import scala.collection.IndexedSeqLike
  import scala.collection.mutable.LinkedHashMap
  import scala.language.postfixOps
  
  final class Model private ( val mappings: collection.mutable.Map[Key, NodeSet])   
  extends Map[Key, NodeSet]
     with MapLike[Key, NodeSet, Model] 
     with CanGenerateScala {
    //--------------methods required for integration with Map and MapLike:  
    def get(key: Key):Option[NodeSet] = mappings.get(key)
    override def empty = new Model(LinkedHashMap.empty)  // LinkedHashMap keeps insert order
    def +[B1 >: NodeSet](kv: (Key, B1)): Model =  kv match {
      case (k: Key, ns: NodeSet) =>
        if (!ns.isEmpty || (ns.isEmpty && k.edge == has())) { //allow empty nodesets for Model(Feature("x"))
          val m2 = mappings.clone
          new Model( m2 + (k -> { if (!m2.isDefinedAt(k)) ns else m2(k).concatNodes(ns, Some(k))  } ))
        } else this
      case _ => warn("Key must map to NodeSet. Ignored:" + kv._2); this
    }    
    def -(k: Key) = new Model(mappings - k)
    def iterator:Iterator[(Key, NodeSet)] = mappings.iterator
    override def stringPrefix = "Model"    
    //--------------------- reqT-specific methods:
    //---- add methods
    def addIfNew(kv: (Key, NodeSet)): Model = 
      if (isNew(kv._1.entity)) this + kv else { warn("Id is not new. Add discarded: " + kv); this }
    def +?(kv: (Key, NodeSet)): Model = addIfNew(kv)
    
    def addIfExists(kv: (Key, NodeSet)): Model = 
      if (!isNew(kv._1.entity)) this + kv else { warn("Id does not exist. Overwrite discarded: " + kv); this }
    def +!(kv: (Key, NodeSet)): Model = addIfExists(kv)
    
    def merge(that: Model): Model = ++(that)
    def ++(that: Model) = super.++(that) : Model //inherited ++ returns Map but we want Model 
    
    def mergeIfNew(that: Model): Model = {
      val (newIds, oldIds) = that.partition(kv => this.isNew(kv._1.entity))
      warn("All ids are not new. Add discarded: " + oldIds.entities.mkString(", "))
      merge(newIds)
    }
    def ++?(that: Model): Model = mergeIfNew(that)
    
    def mergeIfExists(that: Model): Model = {
      val (newIds, oldIds) = that.partition(kv => this.isNew(kv._1.entity))
      warn("Some ids don't exist. Add discarded: " + newIds.entities.mkString(", "))
      merge(oldIds)
      }
    def ++!(that: Model): Model = mergeIfExists(that)
    
    def addEdgeToNodesToAll(el: EdgeToNodes):Model = { //add to all Key Entities
      var result = new Model(mappings) //make a copy of this model
      sources.map { case entity => (Key(entity, el.edge), el.nodes) } foreach (kns => result += kns)
      result
    }
    def +(el: EdgeToNodes):Model = addEdgeToNodesToAll(el)
    def +[T](as: Attribute[T] *): Model = addEdgeToNodesToAll(has(as : _*))
    //----- indexing methods
    lazy val indexed: Map[Int, (Key, NodeSet)] = { 
      val seq = mappings.toIndexedSeq 
      ( for (i <- 0 until seq.size) yield (i -> seq(i)) ) toMap
    }
    lazy val indexOf: Map[Key, Int] = {
      val seq = indexed.map(kns => kns._2._1).toIndexedSeq
      val m: Map[Key, Int] = seq.zip(0 until seq.size).toMap 
      m.withDefault(k => -1)
    }
    def apply(i: Int): (Key, NodeSet) = indexed(i)
    def slice(i: Int): Model = Model() + indexed(i)
    //we also have from Map inherited slice(from,until) 
    
    //----- pretty printing methods:
    def print { println(toScala) }
    def p { print }
    def pp { pp(attributeKinds.collect { case a: AttributeKind[_] => a } : _* ) }
    def pp(as: AttributeKind[_]*) {
      val prefixes = as.map(_.prefix)
      def select(prefMap: Map[String, Node[_]]): Seq[Node[_]] = 
        prefixes.collect { case s if prefMap.isDefinedAt(s) => prefMap(s) } .toSeq
      for (i <- 0 until size) println( i.toString.padTo(3," ").mkString + " " +  {
          val s = indexed(i)._1.entity.toScala.truncPad(Model.ppEntityWidth) + " " +
            indexed(i)._1.edge.toScala + " " +
            ( if (indexed(i)._1.edge == has()) select(indexed(i)._2.prefixMap).map { p => 
                  if (as.size > 1) p.toScala.trunc(Model.ppColumnWidth)
                  else p.toScala
              } else indexed(i)._2.nodes.map(_.toScala)
            ).mkString("(",", ",")")
          if (s.size > Model.ppLineLength) s.take(Model.ppLineLength) + "..." else s
        }        
      )
    }
    def ppa {
      val prefixes = attributeKinds.map(_.prefix)
      def select(prefMap: Map[String, Node[_]]): Seq[String] = 
        prefixes.collect { case s if prefMap.isDefinedAt(s) => s } .toSeq
      for (i <- 0 until size) println( i.toString.padTo(3," ").mkString + " " +  {
          val s = indexed(i)._1.entity.toScala.truncPad(Model.ppEntityWidth) + " " +
            indexed(i)._1.edge.toScala + " " + 
            ( if (indexed(i)._1.edge == has()) select(indexed(i)._2.prefixMap)
              else indexed(i)._2.nodes.map(_.toScala)
            ).mkString("(",", ",")")
          if (s.size > Model.ppLineLength) s.take(Model.ppLineLength) + "..." else s
        }        
      )    
    }
    def ppg { pp(Gist) }
	
    //----- apply, updated and sorted methods
    def apply[T](r: Reference[T]): T = r match {
      case ar: AttrRef[T] => this / ar.ent ! ar.attrKind get
      case sr: SubRef[T] => ( this / sr.ent ! Submodel get ).apply(sr.r)
    }
    
    def updated[T](r: Reference[T], v: T): Model = r match {
      case ar: AttrRef[T] =>  this + ar.ent.has(ar.attrKind(v))
      case sr: SubRef[T] => 
        val sm = this / sr.ent !! Submodel
        this + sr.ent.has(Submodel(sm.updated(sr.r, v)))
    }
       
    def -(entity: Entity, edge: Edge, node: Node[_]): Model = { //TODO is this really needed???
      //TODO recursive removal in submodels??
      val k = Key(entity, edge)
      mappings.get(k) match {
        case Some(ns) =>
          val newNodes = ns - node
          val m2 = mappings.clone
          if (newNodes.isEmpty) new Model(m2 - k) else new Model(m2 - k + (k -> (ns - node)))
        case None => this
      }
    }
	
    def -(kns: (Key, NodeSet)): Model = {
      //TODO recursive removal in submodels??
      val (k, nsToBeRemoved) = kns
      mappings.get(k) match {
        case Some(ns) => 
          val newNodes: NodeSet = ns.filterNot(n => nsToBeRemoved.exists{ case nk: NodeKind => n <==> nk; case any => n == any})
          if (newNodes.isEmpty) this - k else new Model(mappings.updated(k,newNodes)) 
        case None => this
      }
    }  
	
    def removeEdgeToNodesToAll(el: EdgeToNodes):Model = { 
      //TODO recursive removal in submodels??
        map { case (Key(en, ed), ns) => 
          val isSame: Boolean = el.edge match { case e: EdgeKind => e <==> ed; case e => e == ed } 
          if (isSame) (Key(en, ed), NodeSet(ns diff el.nodes)) else (Key(en, ed), ns) } 
    }    
    def -(el: EdgeToNodes): Model = removeEdgeToNodesToAll(el)
    def -(ns: Node[_] *): Model = { //recursive removal in submodels
      val removedInNodeSet = map { case (Key(en, ed), ns2) => (Key(en, ed), NodeSet((ns2.nodes diff ns.toSet).filterNot(n =>      
        ns.exists { 
          case nk: NodeKind => nk <==> n
          case _ => false
        } ))) 
      }
      val removedInKeys = removedInNodeSet.filterNot { 
        case (Key(en, _), _) =>  
          ns.exists(_ == en) || ns.exists{ case nk: NodeKind => nk <==> en; case _ => false } 
      } 
      removedInKeys updateAttributes { case Submodel(m) => Submodel(m.-(ns.toSeq:_*)) } 
    }
    def -(es: Set[Entity]): Model = this - (es.toSeq:_ *)
    //---- string methods    
    override def toString: String = 
      if (Model.overrideToStringWithToScala) toScala else super.toString
    def bodyToScala: String = {
      val nl = if (super.toString.size > 72) "\n" else ""
      def indent(i:Int) = if (nl != "") nl + List.fill(i)("  ").mkString else ""
      "(" + (
        { 
          for ((key, nodes) <- this)  
          yield indent(1) + keyNodesToScala(key, nodes) 
        } . mkString(",")
      ) + nl + ")"
    }
    override def toScala: String = stringPrefix + bodyToScala
    def toTable: String = toTable("\t")
    def toTable(columnSeparator: String, headers: Boolean = true) = {
      val rowSeparator = "\n"
      lazy val headRow = Model.tableHeadings.mkString("",columnSeparator,rowSeparator)  
      val table = for ((Key(entity, edge), NodeSet(nodes)) <- this) yield {
          val edgeValueStr = edge match {
            case lwa: RelationWithAttribute[_] => lwa.attribute.prefix + columnSeparator + lwa.attribute.value 
            case _ => "" + columnSeparator
          }
          val nodeList = nodes.toList.map(n => n.prefix + columnSeparator + n.value.toString.toScala)
          val rowStart = List(entity.prefix, entity.id.toScala, edge.prefix, edgeValueStr).mkString(columnSeparator)
          nodeList map { n => rowStart + columnSeparator + n + rowSeparator} mkString
      }  
      "" + ( if (headers) headRow else "" ) + table.mkString("","",rowSeparator)
    }
    def toHtml(documentTemplate: DocumentTemplate = defaultDocumentTemplate, 
      htmlGenerator: HtmlGenerator = defaultHtmlGenerator): String = htmlGenerator.generate(this, documentTemplate)
    def toHtml: String = toHtml()
    lazy val ids: Vector[String] = collect { case (Key(ent, _), _) => ent.id } .toVector.distinct  
    def idsToValues(objectName: String) = 
       Model.interpreter.get.interpret(s"object $objectName {" + (ids map { id => s"""val $id = "$id"; """ } mkString) + "}")
    //---- selection, restriction and exclusion methods
    def separate(elm: Element, 
      selection: (((Key, NodeSet)) => Boolean) => Model
    ): Model = elm match {
      case a: AttributeKind[_] => a match {
        case e: External.type => selection(ke => ke._2.nodes.exists(_.isInstanceOf[External[_]] ) )
        case _ => selection(ke => ke._2.nodes.exists(_ <==> a ) )
      }
      case a: Attribute[_] => selection(ke => ke._2.nodes.exists(_ == a ) )
      case n: NodeKind => selection(ke => ke._1.entity <==> n || 
          (ke._1.edge match { case rwa: RelationWithAttribute[_] => rwa.attribute <==> n; case _ => false } ) )
      case e: EdgeKind =>  selection(ke => ke._1.edge <==> e)
      case Context => selection(ke => ke._1.entity.isInstanceOf[Context] )
      case Requirement => selection(ke => ke._1.entity.isInstanceOf[Requirement] )
      case Scenario => selection(ke => ke._1.entity.isInstanceOf[Scenario] )
      case Relation => selection(ke => ke._1.edge.isInstanceOf[Relation])
      case k: Key => selection(ke => ke._1 == k)
      case _ => selection(ke => ke._1.entity == elm || 
          (ke._1.edge match { case rwa: RelationWithAttribute[_] => rwa.attribute == elm || rwa == elm; case _ => false } ) )
    } 
    def separateExtended(elm: Element, 
      selection: (((Key, NodeSet)) => Boolean) => Model
    ): Model = elm match {
      case a: AttributeKind[_] => a match {
        case e: External.type => selection(ke => ke._2.nodes.exists(_.isInstanceOf[External[_]] ) )
        case _ => selection(ke => ke._2.nodes.exists(_ <==> a ) )
      }
      case a: Attribute[_] => selection(ke => ke._2.nodes.exists(_ == a ) )
      case n: NodeKind => selection(ke => ke._1.entity <==> n || 
          ke._2.nodes.exists(_ <==> n) || //Extend with destinations
          (ke._1.edge match { case rwa: RelationWithAttribute[_] => rwa.attribute <==> n; case _ => false } ) )
      case e: EdgeKind =>  selection(ke => ke._1.edge <==> e)
      case Context => selection(ke => ke._1.entity.isInstanceOf[Context] || ke._2.nodes.exists(_.isInstanceOf[Context])) //Extend with destinations
      case Requirement => selection(ke => ke._1.entity.isInstanceOf[Requirement] || ke._2.nodes.exists(_.isInstanceOf[Requirement]))
      case Scenario => selection(ke => ke._1.entity.isInstanceOf[Scenario] || ke._2.nodes.exists(_.isInstanceOf[Scenario]))
      case Relation => selection(ke => ke._1.edge.isInstanceOf[Relation])
      case k: Key => selection(ke => ke._1 == k)
      case _ => selection(ke => ke._1.entity == elm || 
          ke._2.nodes.exists(_ == elm) || //Extend with destinations
          (ke._1.edge match { case rwa: RelationWithAttribute[_] => rwa.attribute == elm || rwa == elm; case _ => false } ) )
    } 
    def separateDestinations(elm: Element, 
      selection: (((Key, NodeSet)) => Boolean) => Model
    ): Model = elm match {
      case a: AttributeKind[_] => a match {
        case e: External.type => selection(ke => ke._2.nodes.exists(_.isInstanceOf[External[_]] ) )
        case _ => selection(ke => ke._2.nodes.exists(_ <==> a ) )
      }
      case a: Attribute[_] => selection(ke => ke._2.nodes.exists(_ == a ) )
      case n: NodeKind => selection(ke => ke._2.nodes.exists(_ <==> n))  
      case Context => selection(ke => ke._2.nodes.exists(_.isInstanceOf[Context])) 
      case Requirement =>  selection(ke => ke._2.nodes.exists(_.isInstanceOf[Requirement]))
      case Scenario => selection(ke => ke._2.nodes.exists(_.isInstanceOf[Scenario]))
      case _ => selection(ke => ke._2.nodes.exists(_ == elm)) //Only  destinations
    } 

    def restrictKeys(ks: Set[Key]): Model = collect { case (k,ns) if ks.contains(k) => (k, ns) } 
    def excludeKeys(ks: Set[Key]): Model = collect { case (k,ns) if !ks.contains(k) => (k, ns) } 
    //above restictKeys / excludeKeys are specal cases needed by ModelVector.split and set operations over keySet
    //  if Sets where co-variant we could generalize restrict(es : Set[Entity]) to Set[Element] :-( 

    def restrict(elm: Element): Model = separate(elm, filter) 
    def restrict(es: Set[Entity]): Model = collect { case (k,ns) if es.contains(k.entity) => (k, ns) } 
    def restrict(s: String): Model = collect { case (k,ns) if k.toScala.contains(s) => (k, ns) }
    def / (elm: Element): Model = restrict(elm)
    def / (es: Set[Entity]): Model = restrict(es)
    def / (s: String): Model = restrict(s)
    def exclude(elm: Element): Model = separate(elm, filterNot)
    def exclude(es: Set[Entity]): Model = collect { case (k,ns) if !(es.contains(k.entity)) => (k, ns) } 
    def exclude(s: String): Model = collect { case (k,ns) if !(k.toScala.contains(s)) => (k, ns) }
    def \ (elm: Element): Model = exclude(elm)
    def \ (es: Set[Entity]): Model = exclude(es)
    def \ (s: String): Model = exclude(s)
    def partition(elm: Element): (Model, Model) = (restrict(elm), exclude(elm))
    def partition(es: Set[Entity]): (Model, Model) = (restrict(es), exclude(es))
    def partition(s: String): (Model, Model) = (restrict(s), exclude(s))
    def | (elm: Element): (Model, Model) = partition(elm)
    def | (es: Set[Entity]): (Model, Model) = partition(es)
    def | (s: String): (Model, Model) = partition(s)
   
    def restrictExtended(elm: Element): Model = separateExtended(elm, filter)
    def restrictExtended(es: Set[Entity]): Model = collect { case (k,ns) if (es.contains(k.entity) || 
      (ns collect { case e: Entity => es.contains(e) }).fold( false )(_ || _)) => (k, ns) } 
    def restrictExtended(s: String): Model = collect { case (k,ns) if (k.toScala + ns.toScala).contains(s) => (k, ns) }
    def /+ (elm: Element): Model = restrictExtended(elm)
    def /+ (es: Set[Entity]): Model = restrictExtended(es)
    def /+ (s: String): Model = restrictExtended(s)
    def excludeExtended(elm: Element): Model = separateExtended(elm, filterNot)
    def excludeExtended(es: Set[Entity]): Model = collect { case (k,ns) if !((es.contains(k.entity) || 
      (ns collect { case e: Entity => es.contains(e) }).fold( false )(_ || _))) => (k, ns) } 
    def excludeExtended(s: String): Model = collect { case (k,ns) if !((k.toScala + ns.toScala).contains(s)) => (k, ns) }
    def \+ (elm: Element): Model = excludeExtended(elm)
    def \+ (es: Set[Entity]): Model = excludeExtended(es)
    def \+ (s: String): Model = excludeExtended(s)
    def partitionExtended(elm: Element): (Model, Model) = (restrictExtended(elm), excludeExtended(elm))
    def partitionExtended(es: Set[Entity]): (Model, Model) = (restrictExtended(es), excludeExtended(es))
    def partitionExtended(s: String): (Model, Model) = (restrictExtended(s), excludeExtended(s))
    def |+ (elm: Element): (Model, Model) = partitionExtended(elm)
    def |+ (es: Set[Entity]): (Model, Model) = partitionExtended(es)
    def |+ (s: String): (Model, Model) = partitionExtended(s)

    def restrictDestinations(elm: Element): Model = separateDestinations(elm, filter)
    def restrictDestinations(es: Set[Entity]): Model = collect { case (k,ns) if (
      (ns collect { case e: Entity => es.contains(e) }).fold( false )(_ || _)) => (k, ns) } 
    def restrictDestinations(s:String): Model = collect { case (k,ns) if (ns.toScala).contains(s) => (k, ns) }
    def /-> (elm: Element): Model = restrictDestinations(elm)
    def /-> (es: Set[Entity]): Model = restrictDestinations(es)
    def /-> (s:String): Model = restrictDestinations(s)
    def excludeDestinations(elm: Element): Model = separateDestinations(elm, filterNot)
    def excludeDestinations(es: Set[Entity]): Model = collect { case (k,ns) if !(
      (ns collect { case e: Entity => es.contains(e) }).fold( false )(_ || _)) => (k, ns) } 
    def excludeDestinations(s:String): Model = collect { case (k,ns) if !((ns.toScala).contains(s)) => (k, ns) }
    def \-> (elm: Element): Model = excludeDestinations(elm)
    def \-> (es: Set[Entity]): Model = excludeDestinations(es)
    def \-> (s:String): Model = excludeDestinations(s)
    def partitionDestinations(elm: Element): (Model, Model) = (restrictDestinations(elm), excludeDestinations(elm))
    def partitionDestinations(es: Set[Entity]): (Model, Model) = (restrictDestinations(es), excludeDestinations(es))
    def partitionDestinations(s:String): (Model, Model) = (restrictDestinations(s), excludeDestinations(s))
    def |-> (elm: Element): (Model, Model) = partitionDestinations(elm)
    def |-> (es: Set[Entity]): (Model, Model) = partitionDestinations(es)
    def |-> (s:String): (Model, Model) = partitionDestinations(s)
    
    def restrictDepthFirstSearch(e: Entity): Model = this / depthFirstSearch(Set(e))
    def restrictDepthFirstSearch(es: Set[Entity]): Model = this / depthFirstSearch(es)
    def /-->(e: Entity): Model = restrictDepthFirstSearch(e)
    def /-->(es: Set[Entity]): Model = restrictDepthFirstSearch(es)
    def excludeDepthFirstSearch(e: Entity): Model = this \ depthFirstSearch(Set(e))
    def excludeDepthFirstSearch(es: Set[Entity]): Model = this \ depthFirstSearch(es)
    def \-->(e: Entity): Model = excludeDepthFirstSearch(e)
    def \-->(es: Set[Entity]): Model = excludeDepthFirstSearch(es)
    def partitionDepthFirstSearch(e: Entity): (Model, Model) = (restrictDepthFirstSearch(e), excludeDepthFirstSearch(e))
    def partitionDepthFirstSearch(es: Set[Entity]): (Model, Model) = (restrictDepthFirstSearch(es), excludeDepthFirstSearch(es))
    def |-->(e: Entity): (Model, Model) = partitionDepthFirstSearch(e)
    def |-->(es: Set[Entity]): (Model, Model) = partitionDepthFirstSearch(es)
    
    // ---- set operations over keySet of this and that model
    def intersect(that: Model): Model = (this ++ that).restrictKeys( this.keySet intersect that.keySet )
    def & (that: Model): Model = intersect(that)
    def diff(that: Model): Model = (this ++ that).restrictKeys( this.keySet diff that.keySet )
    def -- (that: Model): Model = diff(that)
    def &~ (that: Model): Model = diff(that)
    
    // ---- different subsets of nodes in Model
    lazy val sources: Set[Entity] = collect { case (Key(e,r),ns) => e } .toSet
    lazy val sourceVector: Vector[Entity] = collect { case (Key(e,r),ns) => e } .toVector
    lazy val destinations: Set[Entity] = { for ((k, NodeSet(ns)) <- this; n <- ns; if (n.isEntity)) yield n.asInstanceOf[Entity] } toSet 
    lazy val destinationVector: Vector[Entity] = collect { case (_,ns) => ns.collect { case e: Entity => e } } .flatten.toVector
    lazy val entities: Set[Entity] = sources ++ destinations  
    lazy val entityVector: Vector[Entity] = (sourceVector ++ destinationVector).distinct
    lazy val entityIndex: Map[Entity, Int] = entityVector.zipWithIndex.toMap
    lazy val undefined: Set[Entity]  = for (e <- destinations; if (!sources.contains(e))) yield e
    lazy val attributes: Set[Attribute[_]] = { 
      for ((k, NodeSet(ns)) <- this; n <- (ns ++ ( k.edge match { 
        case rwa: RelationWithAttribute[_] => Set(rwa.attribute)
        case _ => Set() 
      } ) ); if (n.isAttribute))  yield n.asInstanceOf[Attribute[_]] } toSet
    lazy val relations: Set[Relation] = ( for (Key(entity, edge) <- this.keySet) yield edge ) collect { case r: Relation => r.kind }  
    lazy val relationSources: Set[Entity] = this \ has sources  
    lazy val relationDestinations: Set[Entity] = this \ has destinations
    lazy val relationEntities: Set[Entity] = relationSources ++ relationDestinations   
    lazy val attributeSources: Set[Entity] = this / has sources
    lazy val unrelated: Set[Entity] = entities diff relationSources diff relationDestinations
    lazy val unsourced: Set[Entity] = (relationSources ++ unrelated) diff destinations
    lazy val parents: Set[Entity] =  this / owns sources
    lazy val children: Set[Entity] =  this / owns destinations
    lazy val roots: Set[Entity] =  parents diff children
    
    def depthFirstSearch(startNodes: Set[Entity]): Set[Entity] = {
      def visit(nodes: Set[Entity], visited: Set[Entity]): Set[Entity] = {
        if (nodes.isEmpty) visited
        else visited ++ 
          (nodes filterNot(visited.contains) flatMap {
            e => visit(destinationsOf(e), visited + e) })
      }
      val expanded = startNodes flatMap { // so that m /--> Product adds all Product entitites to StartNodes
        case ek: EntityKind => this / ek sources
        case e => Set(e)
      }
      visit(expanded, Set()) 
    }
    
    //---- predicates
    def isSource(e: Entity): Boolean = sources.contains(e)
    def isDestination(e: Entity): Boolean = destinations.contains(e)
    def isUndefined(e: Entity): Boolean = undefined.contains(e)
    def isDefined(e: Entity): Boolean = !isUndefined(e)
    def hasAttribute[T](a: Attribute[T]):Boolean = attributes.contains(a)
    def isRoot(e: Entity): Boolean = roots.contains(e)
    def isParent(e: Entity): Boolean = parents.contains(e)  
    def isChild(e: Entity): Boolean = children.contains(e)  
    def isNew(e: Entity): Boolean = !ids.contains(e.id)
    lazy val isDeep = ( this / Submodel ).size > 0
    //---- extractions

    def sourcesOf(e: Edge, n: Node[_]): Set[Entity] = ( for ((k, ns) <- this; if ( k.edge <==> e && ns.contains(n) ) ) yield k.entity ).toSet
    def sourcesOf(e: Edge): Map[Entity, Set[Entity]] = {
      var result: Map[Entity, Set[Entity]] =
        SortedMap.empty[Entity,Set[Entity]](entityOrdering).withDefaultValue(Set[Entity]())
      (this / e).destinations foreach { d => result += d -> sourcesOf(e, d) }
      result
    } 
    lazy val ownerOf: Map[Entity, Set[Entity]] = sourcesOf(owns)  
    lazy val ownerCircles: Vector[Set[Entity]] = circles(owns)
    def circles(r: Relation): Vector[Set[Entity]] = {
      def visit(e: Entity, visited: Set[Entity], circle: Set[Entity]): Set[Set[Entity]] = 
        if (visited.contains(e)) Set(circle ++ visited)
        else ( this / e / r ).destinations.map(c => visit(c, visited + e, circle)).flatten
      entityVector.map(e => visit(e, Set(), Set())).flatten.distinct      
    }
    lazy val ownedDepthOf: Map[Entity, Int] = {
      def count(e: Entity, lvl: Int, visited: Set[Entity]): Int =
        if (visited.contains(e)) { warn("circular owns-relation detected including: " + e); -1 }
        else if (ownerOf(e).isEmpty) lvl 
        else count(ownerOf(e).head, lvl + 1, visited + e) //!!!! kolla oändlig loop om ej träd med visited
      val result = scala.collection.mutable.LinkedHashMap(entityVector.map(e => e -> 0):_*)
      for (e <- result.keys) result(e) = count(e, 0, Set()) 
      result.toMap
    }
    lazy val ownedMaxDepth = ownedDepthOf.collect { case (_,i) => i } .max
    lazy val ownedAtDepth: Map[Int, Set[Entity]] = {
      val result = scala.collection.mutable.Map[Int, Set[Entity]]((0 to ownedMaxDepth).map(i => i -> Set[Entity]()):_*)
      for ((e, i) <- ownedDepthOf) if (result.isDefinedAt(i)) result(i) = result(i) + e else result += i -> Set(e) 
      result.toMap
    }
    def parentsOf(e: Entity): Set[Entity] = (( this /-> e / owns) \ e).sources  //better use ownerOf - is this needed???
    def childrenOf(e: Entity): Set[Entity] = this / e children
    def destinationsOf(source: Entity):Set[Entity] = this / source destinations
    def attributesOf(source: Entity): Set[Attribute[_]] = this / source attributes
    
    //--- semantic checks and warnings
    lazy val multiOwners = ownerOf filter { case (e, es) => es.size > 1  }
    lazy val hasMultiOwners: Boolean = !multiOwners.isEmpty
    lazy val missingSpecs: Set[Entity] = filter { case (ke,ns) => 
      ns.exists{ case n: Status => n.value >= SPECIFIED; case _ => false } && 
        !ns.exists{  _ <==> Spec }
    } sources
    lazy val hasMissingSpecs: Boolean = !missingSpecs.isEmpty
    lazy val hasOwnerCircles: Boolean = !ownerCircles.isEmpty
    def check: Boolean = {
      if (hasMultiOwners) warn("Entities with more than one direct owner: " + 
        multiOwners.keySet.mkString(", "))
      if (hasMissingSpecs) warn("Entities with Status >= SPECIFIED and missing Spec: " + 
        missingSpecs.mkString(", "))
      if (hasOwnerCircles) warn("Entities have circular ownership. Model ownerCircles == " + ownerCircles)
      !hasMultiOwners && !hasMissingSpecs && !hasOwnerCircles
    }
    
    //---- attribute extraction methods
    def get[T](a: AttributeKind[T]): Option[T] = attributes.find(_ <==> a) match {
      case Some(attr) => attr match {
        case e: External[_] => Some(e.fromFile.value.asInstanceOf[T]) //TODO check if this is what you want
        case _ => Some(attr.value.asInstanceOf[T])
      }
      case _ => None
    }
    def get[T](r: Reference[T]): Option[T] = r match {
      case AttrRef(e, ak) => ( this / e  ).get(ak) 
      case SubRef(e, r) =>  ( ( this / e ) !! Submodel ).get(r) 
    }
    
    def ![T](a: AttributeKind[T]): Option[T] = get(a)
    def ![T](r: Reference[T]): Option[T] = get(r)

    def getOrDefault[T](a:AttributeKind[T]):T = get(a).getOrElse(a.default)
    def getOrDefault[T](r: Reference[T]): T = r match {
      case AttrRef(e, ak) => ( this / e  ).getOrDefault(ak) 
      case SubRef(e, r) =>  ( ( this / e ) !! Submodel ).getOrDefault(r) 
    }    
    
    def !![T](a:AttributeKind[T]):T = getOrDefault(a)
    def !![T](r: Reference[T]):T = getOrDefault(r)

    def attributeMap[T](a: AttributeKind[T]): Map[Entity, Attribute[T]] = collect { 
      case (Key(e,r),ns) if ns.exists(_ <==> a) => (e, ns.find(_ <==> a).get match {
          case attr: Attribute[T] => attr
        }
      ) : (Entity, Attribute[T])
     } .toMap
    def attributeValueMap[T](a: AttributeKind[T]): Map[Entity, T] = attributeMap(a).map( ea => (ea._1, ea._2.value))
    def !!![T](a: AttributeKind[T]): Map[Entity, T] = attributeValueMap(a)
     
    def attributeVector[T](a: AttributeKind[T]): Vector[Attribute[T]] = collect { 
      case (Key(e,r),ns) if ns.exists(_ <==> a) => ( ns.find(_ <==> a).get match {
          case attr: Attribute[T] => attr
        } 
      ) : Attribute[T]
     } .toVector
    def attributeValueVector[T](a: AttributeKind[T]): Vector[T] = attributeVector(a).map( a => a.value)
    def !!!![T](a: AttributeKind[T]): Vector[T] = attributeValueVector(a)
    
    lazy val constraints: Constraints = Constraints(( this !!!! Constraints ).flatten)
    lazy val constraintsAll: Constraints = constraints ++ (
      if (isDeep) submodels.map(_.constraintsAll).reduce(_ ++ _)
      else Constraints(Vector())
    )
    // ---- transformation methods  
    def replace(e1: Entity, e2: Entity): Model = { //safer than updateEntities 
      def updateNS(ns: NodeSet): NodeSet = ns.map { 
        case n if (n == e1) => e2 
        case sm: Submodel => Submodel(sm.value.replace(e1,e2)) //non-tail-recursive call
        case any => any
      }
      if (entities.contains(e2)) {
        warn(s"Discarding replace (perhaps in Submodel) from $e1 to existing entity $e2" + 
             s"\nEither remove $e2 first or use updateEntities.")
        this
      } else this collect {
        case (Key(e,r),ns) if (e == e1) => (Key(e2,r), updateNS(ns)) 
        case (Key(e,r),ns) => (Key(e,r),updateNS(ns)) 
      }
    }
         
    def updateEntities(pf: PartialFunction[Entity, Entity]): Model = {
      val pfoe = pf.orElse[Entity, Entity] { case e => e }
      def updateNS(ns: NodeSet): NodeSet = ns.nodes map {
        case sm: Submodel => Submodel(sm.value.updateEntities(pf)) //non-tail-recursive call 
        case e: Entity => pfoe(e) 
        case a => a 
      }
      map { case (Key(en, ed), ns) => (Key(pfoe(en), ed), updateNS(ns)) } 
    }
    
    def updateAttributes(pf: PartialFunction[Attribute[_], Attribute[_]]): Model = {
     val pfoe = pf.orElse[Attribute[_],Attribute[_]] { case n: Attribute[_] => n }
     def updateNS(ns: NodeSet): NodeSet = {
        val newNodeSet = ns.nodes map {
          case sm: Submodel => pfoe(Submodel(sm.value.updateAttributes(pf))) //non-tail-recursive call 
          case n: Attribute[_] => pfoe(n)
          case any => any
        }
        NodeSet().concatNodes(newNodeSet)
      }
      map { 
        case (Key(en, ed), ns) if ed == has() => (Key(en, ed), updateNS(ns)) 
        case any => any
      } 
    }

    def updateRelations(pf: PartialFunction[Relation, Relation]): Model = {
      val pfoe = pf.orElse[Relation,Relation] { case r => r }
      map { case (Key(en, ed), ns) => (ed match { case r: Relation => (Key(en, pfoe(r) : Edge), ns); case _ => (Key(en, ed), ns) } ) } 
    }
    
    def collectNodes[T](pf: PartialFunction[Node[_], T]): Seq[T] = {
      (collect { case (k,ns) => ns.toSeq.collect(pf) } ).toSeq.flatten
    }
    lazy val submodels: ModelVector = ModelVector( collectNodes { case Submodel(m) => m } toSeq :_* )
    lazy val models: ModelVector = ModelVector(this - Submodel) ++ this.submodels
    lazy val flatten: Model = models.merge ++ ownedBySubmodels
    lazy val flattenUnowned: Model = models.merge 

    lazy val flattenAll: Model = if (!flatten.isDeep) flatten else flatten.flattenAll
    lazy val flattenAllUnowned: Model = if (!flattenUnowned.isDeep) flattenUnowned else flattenUnowned.flattenAllUnowned

    lazy val depth: Int = if (!isDeep) 0  else 1 + (submodels.map(_.depth).reduce(Math.max(_,_)))
    
    def flatten(e: Entity): Model = if (this.contains(e.has)) this - e.has + e ++ ( this / e !! Submodel ) ++ ownedBySubmodel(e) else this
    def flattenUnowned(e: Entity): Model = if (this.contains(e.has)) this - e.has + e ++ ( this / e !! Submodel ) else this
    
    def ownedBySubmodel(e: Entity): Model = Model( ( this / e / Submodel).sources.toSeq.map( e => 
      e.owns(( this / e !! Submodel).entities.toSeq:_*) ) :_*)
    lazy val ownedBySubmodels: Model = ModelVector(this.entities.toVector.map(e => this.ownedBySubmodel(e)):_*).merge
    
    def deepen(e: Entity): Model = if (this.contains(e.owns)) {
        this - e.owns + (
          e has Submodel(
            Model(( this / e / owns).destinations.map(d => d.has((( this / d).attributes.toSeq):_*)).toSeq:_*) ++
              (this / e !! Submodel)
          )
        ) -- ( this / e / owns).destinations.map(d => d.has)
      } else this 
        
    lazy val deepen: Model = if (ownedMaxDepth > 0) {
      var newModel = this
      for (e <- ownedAtDepth(ownedMaxDepth-1)) newModel = newModel.deepen(e)
      newModel
    } else this
    
    lazy val deepenAll: Model = if (deepen.ownedMaxDepth <= 0) deepen else deepen.deepenAll  
   
    def up: Model = updateAttributes { case n: Status => n.up }
    def down: Model = updateAttributes { case n: Status => n.down }
    def up(e: Entity): Model = 
      if (this / e ! Status == None) { warn(e + "has no status!"); this} 
      else (this / e).up ++ (this \ e)
    def down(e: Entity): Model = 
      if (this / e ! Status == None) { warn(e + "has no status!"); this} 
      else (this / e).down ++ (this \ e)
    def drop: Model = this - ( this / Status(DROPPED)).entities
    def loadExternals: Model = updateAttributes { case n: External[_] => n.fromFile}
    
    //---- sorting methods

    def sorted(intAttr: AttributeKind[Int]): Model = {
      def attrLessThan(kns1:(Key, NodeSet), kns2:(Key, NodeSet)): Boolean = {
        val a1 = (this / kns1._1 ! intAttr).getOrElse(Int.MaxValue)
        val a2 = (this / kns2._1 ! intAttr).getOrElse(Int.MaxValue)
        a1 < a2  
      }
      val sortOrdering = Ordering.fromLessThan[(Key, NodeSet)](attrLessThan)
      val newMappings = LinkedHashMap.empty[Key, NodeSet]
      for (i <- mappings.toSeq.sorted(sortOrdering)) newMappings += i
      new Model(newMappings)
    }    
    
    def sorted: Model = {
      val newMappings = LinkedHashMap.empty[Key, NodeSet]
      for (i <- mappings.toSeq.sorted(keyNodeSetOrdering)) newMappings += i
      new Model(newMappings)
    }

    
    def backlog(r: Release): Vector[Entity] = {
      val content: Set[Entity] = ( this / r / implements ).destinations
      val unorderd = (( this / content ) \ Order).entities
      val orderMap = (( this / content).attributeMap(Order)).collect { case (e,Order(i)) => (e,i) }
      val ents = orderMap.keys.toVector.sorted(Ordering.fromLessThan[Entity]((a,b) => orderMap(a) < orderMap(b)))
      ents ++ unorderd.toVector
    }
    def backlog = {
      val unorderd = (( this / Requirement ) \ Order).entities
      val orderMap = (( this / Requirement).attributeMap(Order)).collect { case (e,Order(i)) => (e,i) }
      val ents = orderMap.keys.toVector.sorted(Ordering.fromLessThan[Entity]((a,b) => orderMap(a) < orderMap(b)))
      ents ++ unorderd.toVector
    }
    
    //---- visitor methods
    lazy val entityEdgeSet = keySet.collect { case Key(e,l) => (e,l) } 
    lazy val entityEdgeList = entityEdgeSet.toList.sortWith(_.toString < _.toString) 
    lazy val entityEdgeMap = {
      var result: Map[Entity, SortedSet[Edge]] = SortedMap.empty[Entity,SortedSet[Edge]](entityOrdering)
      entityEdgeSet foreach { ee => 
        if ( result.isDefinedAt(ee._1) ) result += ee._1 -> ( result(ee._1) + ee._2 ) else result += ee._1 -> SortedSet(ee._2)(edgeOrdering) 
      }
      result
    }
    
    //---
    def split(mv: ModelVector): ModelVector = mv.split(this)
   
    //--- integration with constraints:
    def impose[T](cs: Seq[Constr[T]]) = CSP( this , cs)
    def impose(cs: Constraints) = CSP( this , cs.value)    
   
    //--- code and testcase execution
    
    def run() = ( this / Code ).loadExternals.attributeMap(Code).collect { 
      case (ent, Code(c)) => (ent, Code(c).run(( this / ent ! Input).getOrElse(""))) 
    }
    def run(ent: Entity): String = Code( this / ent !! Code ).run(( this / ent ! Input).getOrElse(""))
    def tested() = {
      var newModel = this
      attributeMap(Code).collect { case (tc@TestCase(id), code@Code(c)) =>
        newModel += tc has Output(code.run(( this / tc ! Input).getOrElse("")))
      }
      newModel
    }
    lazy val testMap: Map[Entity,(String, String)] = {
      val output = attributeMap(Output)
      val expect = attributeMap(Expectation)
      val result = output collect { case (TestCase(ent), Output(str)) if expect.isDefinedAt(TestCase(ent))   => 
        (TestCase(ent),(str, expect(TestCase(ent)).value))
      }
      result.toMap
    }
    lazy val testFailed = testMap.filterNot { case (_,(s1,s2)) =>  s1 == s2 }
    lazy val testPassed = testMap.filter { case (_,(s1,s2)) =>  s1 == s2 }
    def isTestOk: Boolean = {
      val results = attributeMap(Expectation).map { case (TestCase(ent), Expectation(exp)) =>
        val res = this.run(TestCase(ent))
        if (exp == res) true
        else { 
          warn("FAILED: " + TestCase(ent) + s"\n  Output:    $res\n  Expectation: $exp" ) 
          false 
        } 
      } .toSeq
      results reduce (_ && _)
    }
  }

  object Model extends {
    // --- integration with collections:
    import scala.collection.mutable.{Builder, MapBuilder}
    import scala.collection.generic.CanBuildFrom
 
    def empty = new Model(LinkedHashMap.empty)  //was SortedMap.empty(keyOrdering)
    def apply(kvs: (Key, NodeSet)*): Model = {
      var a : Model = empty
      for (kv <- kvs) a = a + kv
      a
    }
    def newBuilder: Builder[(Key, NodeSet), Model] =
      new MapBuilder[Key, NodeSet, Model](empty)
    implicit def canBuildFrom: CanBuildFrom[Model, (Key, NodeSet), Model] =
      new CanBuildFrom[Model, (Key, NodeSet), Model] {
        def apply(from: Model): Builder[(Key, NodeSet), Model] = newBuilder
        def apply(): Builder[(Key, NodeSet), Model] = newBuilder
      }
    
    // --- reqT-specific members:
    def fromEntitySet(es: Set[Entity]): Model = {
      var result = Model()
      for (e <- es) {result += e has Gist("undefined")}
      result
    }
    def fromTable(inFile: String, rowSeparator: String = "\t"): String = {
      val linesH = loadLines(inFile) map {_.split(rowSeparator).toList}
      val lines = linesH.dropWhile(_ == tableHeadings)  
      lines collect { case List(ent, id, link, lAttr, lVal, node, nVal) => 
        ent + "(" + id + ") " + link + " " +  
          ( if (lAttr == "") "" else lAttr + "(" + lVal + ") to " ) + 
            node + "(" + nVal + ")"
      } mkString("Model(\n  ",",\n  ","\n)\n")
    }
    lazy val tableHeadings = List("ENTITY", "ENTITY id", "LINK", "LINK attr", "LINK val", "NODE", "NODE val") 
    var overrideToStringWithToScala = true
    var ppLineLength = 92
    var ppColumnWidth = 33
    var ppEntityWidth = 23
    val unitVisitor = (e: Entity, path: List[Entity], level: Int, count: Int, nSiblings: Int) => ()
    val printVisitor = (e: Entity, path: List[Entity], level: Int, count: Int, nSiblings: Int) => {
      println("Entity visited: " + e + " at level " + level + " count " + count + " #siblings " + nSiblings + "\n" + "Path: " + path + "\n")
      ()
    }
    var interpreter: Option[scala.tools.nsc.interpreter.IMain] = None 
    def interpreterWarning() {
      warn( "No interpreter avialable: result is empty Model()" +
            "\nTo make an interpreter available, you can do this:" +
            "\n  in the REPL: Model.interpreter = Some($intp)" +
            "\n  in Kojo:     Model.interpreter = Some(builtins.kojoInterp)"
      )
    }
    def interpret(s: String): Model = {
      interpreter match {
        case None => interpreterWarning() ; Model()
        case Some(i) => 
          val result = Array[reqt.Model](reqt.Model())
          i.beQuietDuring(i.bind("result", "Array[reqt.Model]", result))
          i.quietRun("result(0) = " + s)
          result(0)          
      }
    }
    
    def load(inFile: String): Model = interpret(reqt.load(inFile))
  }  

}   //end package org
