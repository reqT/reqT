package reqt {

  trait GraphVizGenerator {
    val rankdir="LR" //for Left-Right layout (or "TB" for Top-Down)
    val initSpec = 
      s"""  compound=true;overlap=false;rankdir=$rankdir;clusterrank=local;"""+'\n'+
      """  node [fontname="Helvetica", fontsize=12];"""+'\n'+
      """  edge [fontname="Helvetica", fontsize=12];"""+'\n'
    val entStyle = List("shape=oval")
    val attrStyle = List("shape=box", "style=rounded")
    val attrLinkStyle = List("arrowhead=none", "style=dotted")
    val entLinkStyle = List("arrowhead=normal", "style=filled")
    val clusterStyle = List("arrowhead=none", "label=Submodel", "style=dashed")
    val q = '\"'
    val strMaxLen = 30
    def trunc(s: String) = s.take(strMaxLen) + (if (s.size>strMaxLen) "..." else "")
    def quote(s: String) = ""+q+s+q
    def bracket(xs:List[String]) = xs.mkString("[",",","]")
    def comment(init: String, s: String) = init+"/* "+s+" */\n"
    def entityToGVNode(e: Entity) = ""+e.kind+"\\n"+e.id
    def attrToGVNode[T](e: Entity, a: Attribute[T]) = ""+e.kind+" "+e.id+" "+a.kind
    def attrToGVNodeLabel[T](e: Entity, a: Attribute[T]) = quote(attrToGVNode(e,a)) + 
      " [shape=box, label= " + quote(
          a match {
            case ex: External[_] => ex.kind+"["+ex.emptyAttr.kind+"]"+"("+ trunc(ex.value) + ")"
            case Submodel(sm) => "Submodel(???)"
            case s: StringValue => a.kind +"("+ trunc(s.value) + ")"
            case _ => a.toScala
          }  
        ) + "];"
    def entityAttrGVLink[T](e: Entity, a: Attribute[T]) =
      quote(entityToGVNode(e))+" -> "+quote(attrToGVNode(e,a))+" [label=has];"
    def entityToEntityGVLink(e1: Entity, rel: Edge, e2: Entity) = {
      val r = rel.toScala
      quote(entityToGVNode(e1))+" -> "+quote(entityToGVNode(e2))+s" [label=$r];"
    }
    def gvGen(
      m: Model, 
      init: String, 
      indent: String, 
      graphType: String, 
      graphName: String, 
      clusterInit: String,
      path: String
    ): String = {
      val m2 = if (rankdir=="LR") m.reverse else m
      val indent2 = indent + "  "
      val nl = "\n"+indent2
      val mnos = m2 - Submodel
      val msub = m2.attributeMap(Submodel)
      lazy val entityIndex = m2.entityVector.zipWithIndex.toMap
      def entId(e: Entity) = path+"!"+e
      def entName(e: Entity) = quote(entId(e))
      def entLabel(e: Entity) = quote(""+e.kind+"\\n"+e.id)
      def attrName[T](e: Entity, a: Attribute[T]) = quote(entId(e)+"!"+a.kind) 
      def attrLabel[T](e: Entity, a: Attribute[T]) = quote(
          a match {
            case ex: External[_] => ex.kind+"["+ex.emptyAttr.kind+"]"
            case Submodel(sm) => "Submodel"
            case s: StringValue => a.kind +"("+ trunc(s.value) + ")"
            case _ => a.toScala
          }  
        )
        
      def gvEntLabel(e: Entity) = 
        entName(e)+" "+bracket(("label="+entLabel(e))::entStyle)+";" 
      def gvAttrLabel[T](e: Entity, a: Attribute[T]) =
        attrName(e,a)+" "+bracket(("label="+attrLabel(e,a))::attrStyle)+";"
      def gvLinkEntAttr[T](e: Entity, a: Attribute[T]) =
        entName(e)+" -> "+attrName(e,a)+" "+bracket(attrLinkStyle)+";"
      def gvLinkEntRelEnt(e1: Entity, rel: Edge, e2: Entity) = 
        entName(e1)+" -> "+entName(e2)+" "+bracket(("label="+rel.toScala)::entLinkStyle)+";"
      
      val entNodes =  comment(nl, "=== Entity node styles ===") +
        mnos.entityVector.map(e => gvEntLabel(e)).mkString(nl,nl,nl)
      val attrNodes = comment(nl,"=== Attribute node styles ===") +
        mnos.entityVector.map( e => (mnos / e).attributes.map(a =>
          gvAttrLabel(e,a))).flatten.mkString(nl,nl,nl)
      
      val attrLinks = comment(nl,"=== Entity-Attrubute links ===") +
        mnos.entityVector.map( e => (mnos / e).attributes.map(a =>
          gvLinkEntAttr(e,a))).flatten.mkString(nl,nl,nl)
      val entityLinks = comment(nl,"=== Entity-Entity links ===") +
        (mnos \ has).collect { case (Key(e1,edg),ns) => ns.collect {  
          case e2: Entity => gvLinkEntRelEnt(e1,edg,e2) 
        } } .toSet.flatten.mkString(nl,nl,nl)

      val clusters = comment(nl,s"=== Submodels of $path ===") + (
        if (msub.isEmpty) "" else msub.collect { case (e1, Submodel(sm)) =>
          if (sm.isEmpty) "" else {
            val path2 = path+"_"+e1.kind+entityIndex(e1)
            val clusterName = clusterInit+path2
            val e2 = sm.entityVector.head
            entName(e1)+" -> "+quote(path2+"!"+e2)+" "+
              bracket(s"lhead=$clusterName"::clusterStyle)+";\n\n" +
                gvGen(sm, "", indent2, "subgraph", clusterName, clusterName, path2)
          }
        }.mkString(nl,nl,nl) 
      )
      val graphBody = Seq(entNodes,attrNodes,entityLinks,attrLinks,clusters).mkString
      s"$indent$graphType $graphName {\n$init$graphBody$indent}\n"
    }
    def toGraphViz(m: Model) = gvGen(m, initSpec, "", "digraph", "Model", "cluster", "")
  }

}