package reqT
package export

//exporters with apply method that generates string output

object toStringSimple extends StringExporter {
  override def body(m: Model): String = m.toStringSimple
}

object toStringCompact extends ModelToString 
object toStringPaired extends ModelToString with NewLineEnding
object toScalaCompact extends ModelToString with ScalaGenerators 
object toScalaPaired extends ModelToString with ScalaGenerators with NewLineEnding
object toNestedGV extends GraphVizGenerator  

trait Exporter[T] { def apply(m: Model): T }

trait StringExporter extends Exporter[String] {

  override def apply(m: Model): String = preamble(m) + body(m) + ending(m)
  def body(m: Model): String 
  def preamble(m: Model): String = ""
  def ending(m: Model): String = ""

  //string utilities:  (move to Utils ??)
  val q: String = '\"'.toString
  val q3: String = q*3
  val nl = "\n"
  def nlLitteral = """\n"""
  def indent(n: Int): String = " " * (n * Settings.intentSpacing)
}

trait ModelToString extends StringExporter {
  def emptyModelString: String = "()"

  def indentCheck(m: Model, path: NodePath): String = {
    val space = "\n" + indent(path.level + 1)
    if (m.toStringBody.length > (Settings.lineLength - space.length)) space else ""
  }
  
  def modelPre: String = "("  
  def modelPost(m: Model, path: NodePath): String = ")"  
  def elemSep: String = ", "
    
  def exportModel(sub: Model, path: NodePath): String = 
    if (sub.isEmpty) ""
    else if (sub.size == 1) exportElem(sub.elems.head, sub, path)
    else sub.elems.map(exportElem(_, sub, path)).mkString(elemSep) 
  
  def exportElem(elem: Elem, sub: Model, path: NodePath): String = elem match {
    case NoElem => ""
    case e: Entity => indentCheck(sub, path) + exportEntity(e, path / e) 
    case a: Attribute[_] => indentCheck(sub, path) + exportAttribute(a, path / a)
    case r: Relation => 
      val isSimple = (r.tail.size == 1 && r.tail.elems.head.isNode)
      indentCheck(sub, path) + exportHead(r.head, path / r.head) + 
        ( if (!isSimple) modelPre else "" ) + 
          exportModel(r.tail, path / r.head) + 
            ( if (!isSimple) modelPost(r.tail, path) else "" )
  }
    
  def exportHead(h: Head, path: NodePath): String = 
    exportEntity(h.entity, path) + " " + h.link + " "
  def exportEntity(e: Entity, path: NodePath): String = e.toString
  def exportAttribute[T](a: Attribute[T], path: NodePath): String =  a.toString
  
  override def preamble(m: Model): String = "Model("
  override def ending(m: Model): String = ")"
  override def body(m: Model): String = exportModel(m, /)
}


trait NewLineEnding { self: ModelToString =>
  override def modelPost(m: Model, path: NodePath) = indentCheck(m, path) + ")"
  override def ending(m: Model) = if (m.toStringBody.length > Settings.lineLength) "\n)" else ")" 
}  


trait ScalaGenerators { self: ModelToString =>
  override def exportEntity(e: Entity, path: NodePath): String = e.toScala
  override def exportAttribute[T](a: Attribute[T], path: NodePath): String =  a.toScala
}

trait GraphVizGenerator extends StringExporter {
  def formats = """
  compound=true;overlap=false;rankdir=LR;clusterrank=local;
  node [fontname="Sans", fontsize=9];
  edge [fontname="Sans", fontsize=9];
"""
  
  def style(elem: Elem): String = elem match {
    case e: Entity => 
      val (row1, row2) = (e.myType, e.id) 
      s" [label=$q$row1$nlLitteral$row2$q, shape=box]"
    case a: Attribute[_] => 
      val (row1, row2) = (a.myType, a.value) 
      s" [label=$q$row1$nlLitteral$row2$q, shape=box, style=rounded]"
    case _ => ""
  }
  
  def node(e: Elem, path: NodePath): String = s"  $q$path$e$q"
  
  def singleSubnodeLink(from: Entity, link: RelationType, to: Elem, path: NodePath): String = 
    indent(path.level) + node(from, path) + style(from) + ";\n" +
    indent(path.level) + node(to, path/from) + style(to) + ";\n" +
    indent(path.level) + node(from, path) + " -> " + node(to, path/from) + s"[label=$link]" + ";\n"
      
  def subGraphPre(from: Entity, link: RelationType, to: Elem, path: NodePath): String =
    indent(path.level) + node(from, path) + style(from) + ";\n" +
    indent(path.level) + node(from, path) + " -> " + node(to, path/from) + 
    s" [label=$link, lhead=${q}cluster_$from$q]" + ";\n" +
    indent(path.level) + s"  subgraph ${q}cluster_$from$q { \n"

  def exportModel(m: Model, path: NodePath): String = m.collect {
    case n: Node => indent(path.level) + node(n, path) + style(n) +";\n"
    case Relation(e1,l1,sub) => sub match {
      case Model() => indent(path.level) + node(e1, path) + style(e1) +";\n" 
      case Model(e2) if e2.isNode => singleSubnodeLink(e1, l1, e2, path)
      case Model(Relation(e2, _ , Model())) => singleSubnodeLink(e1, l1, e2, path)
      case Model(Relation(e2, l2, sub2)) if sub2.tip.size == 1 => 
        singleSubnodeLink(e1, l1, e2, path) + 
        singleSubnodeLink(e2, l2, sub2.tip.elems.head, path/e1) +
        exportModel(sub2, path/e1/e2)
      case _ => 
        subGraphPre(e1, l1, sub.tip.elems.head, path) +
        exportModel(sub, path/e1)  + indent(path.level + 1) + "}\n"
    }
  } .mkString
    
  override def preamble(m: Model): String = s"""digraph ${q}reqT.Model${q} { $nl$formats$nl"""
  override def ending(m: Model): String = "\n}"
  override def body(m: Model): String = exportModel(m.reverse,/)
}






















