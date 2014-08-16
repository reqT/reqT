package reqT
package export

//exporters with apply method that generates string output

object toStringSimple extends StringExporter {
  override def body(m: Model): String = m.toStringSimple
}

object toStringCompact extends ModelToString 
object toStringPaired extends ModelToString with NewLineEnding
object toScalaCompact extends ModelToString with ScalaGenerators 
object toScalaCompactBody extends ModelToString with ScalaGenerators {
  override def indent(n: Int): String = " " * ((n-1) * Settings.indentSpacing)
  override def apply(m: Model): String = body(m).trim
}
object toScalaPaired extends ModelToString with ScalaGenerators with NewLineEnding
object toScalaExpanded extends ModelToString with ScalaGenerators  {
  override def indentCheck(m: Model, path: NodePath) = "\n" + indent(path.depth + 1)
}
object toGraphVizNested extends GraphVizNested  
object toGraphVizFlat extends GraphVizFlat  
object toPathTable extends TableExporter
object toHtml extends HtmlExporter

trait Exporter[T] { def apply(m: Model): T }

trait ExporterUtils {
  //string utilities:  (move to Utils ??)
  val q: String = '\"'.toString
  val q3: String = q*3
  val nl = "\n"
  def nlLitteral = """\n"""
  def indent(n: Int): String = " " * (n * Settings.indentSpacing)
  def makeString(a: Any): String = a.toString //ready to override
}

trait StringExporter extends Exporter[String] with ExporterUtils {
  override def apply(m: Model): String = preamble(m) + body(m) + ending(m)
  def body(m: Model): String 
  def preamble(m: Model): String = ""
  def ending(m: Model): String = ""
}

trait ModelToString extends StringExporter {
  def emptyModelString: String = "()"

  def indentCheck(m: Model, path: NodePath): String = {
    val space = "\n" + indent(path.depth + 1)
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
  def exportEntity(e: Entity, path: NodePath): String = makeString(e)
  def exportAttribute[T](a: Attribute[T], path: NodePath): String =  a match {
    case xs: VectorAttribute[_] => 
      val space = "\n"+indent(path.depth + 1) 
      xs.value.map(makeString).mkString(xs.prefix+"("+space,","+space,")")
    case _ => makeString(a)
  }
  
  override def preamble(m: Model): String = "Model("
  override def ending(m: Model): String = ")"
  override def body(m: Model): String = exportModel(m, /)
}


trait NewLineEnding { self: ModelToString =>
  override def modelPost(m: Model, path: NodePath) = indentCheck(m, path) + ")"
  override def ending(m: Model) = if (m.toStringBody.length > Settings.lineLength) "\n)" else ")" 
}  

trait ScalaGenerators { self: ModelToString =>
  override def makeString(a: Any) = a match {
    case d : DSL => d.toScala
    case _ => a.toString 
  }
}
  
trait GraphViz extends StringExporter {
  def formats = """
  compound=true;overlap=false;rankdir=LR;clusterrank=local;
  node [fontname="Sans", fontsize=9];
  edge [fontname="Sans", fontsize=9];
"""
  override def preamble(m: Model): String = s"""digraph ${q}reqT.Model${q} { $nl$formats$nl"""
  override def ending(m: Model): String = "\n}"  
}

trait GraphVizNested extends GraphViz {
  
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
    indent(path.depth) + node(from, path) + style(from) + ";\n" +
    indent(path.depth) + node(to, path/from) + style(to) + ";\n" +
    indent(path.depth) + node(from, path) + " -> " + node(to, path/from) + s"[label=$link]" + ";\n"
      
  def subGraphPre(from: Entity, link: RelationType, to: Elem, path: NodePath): String =
    indent(path.depth) + node(from, path) + style(from) + ";\n" +
    indent(path.depth) + node(from, path) + " -> " + node(to, path/from) + 
    s" [label=$link, lhead=${q}cluster_$from$q]" + ";\n" +
    indent(path.depth) + s"  subgraph ${q}cluster_$from$q { \n"

  def exportModel(m: Model, path: NodePath): String = m.toVector.collect {
    case n: Node => indent(path.depth) + node(n, path) + style(n) +";\n"
    case Relation(e1,l1,sub) => sub match {
      case Model() => indent(path.depth) + node(e1, path) + style(e1) +";\n" 
      case Model(e2) if e2.isNode => singleSubnodeLink(e1, l1, e2, path)
      case Model(Relation(e2, _ , Model())) => singleSubnodeLink(e1, l1, e2, path)
      case Model(Relation(e2, l2, sub2)) if sub2.tip.size == 1 => 
        singleSubnodeLink(e1, l1, e2, path) + 
        singleSubnodeLink(e2, l2, sub2.tip.elems.head, path/e1) +
        exportModel(sub2, path/e1/e2)
      case _ => 
        subGraphPre(e1, l1, sub.tip.elems.head, path) +
        exportModel(sub, path/e1)  + indent(path.depth + 1) + "}\n"
    }
  } .mkString
    
  override def body(m: Model): String = exportModel(m.reverseElems,/)
}

trait GraphVizFlat extends GraphViz {
  
  def node(elem: Elem): String = elem match {
    case n: Node => s"$q$n$q" 
    case _ => ""
  }
  
  def nodeStyle(elem: Elem): String = elem match {
    case e: Entity => 
      val (row1, row2) = (e.myType, e.id) 
      node(e) + s" [label=$q$row1$nlLitteral$row2$q, shape=box];\n"
    case a: Attribute[_] => 
      val (row1, row2) = (a.myType, a.value) 
      node(a) + s" [label=$q$row1$nlLitteral$row2$q, shape=box, style=rounded];\n"
    case _ => ""
  }
  
  override def body(m: Model): String = m.atoms.map {
    case n: Node => nodeStyle(n)
    case Relation(from,link,Model(to)) => 
      nodeStyle(from) + nodeStyle(to) +
      node(from) + " -> " + node(to) + s" [label=$link]" + ";\n" 
    case _ => ""
  } .mkString

}

trait TableExporter extends StringExporter {
  def body(m: Model): String = {
    val row =  m.mapLeafPaths { n => 
      Seq(n.init.toScala, n.lastNode.myType, n.lastNode.toScalaBody) .
        mkString(Settings.columnSeparator) 
    }
    row.mkString(Settings.rowSeparator)
  }
}

trait FileExporter extends Exporter[Unit] with ExporterUtils {
  def defaultOutputDir: String = "output"
  def defaultOutputFile: String = "index"
  def defaultTitle: String = "Untitled Model"
  def titleOrDefault(m: Model) =  m.get(Title).getOrElse(defaultTitle) 
  def apply(m: Model, outDir: String, fileName: String): Unit = 
    exportModelToFile(m, outDir, fileName)
  def exportModelToFile(m: Model, outDir: String, fileName: String): Unit
  override def apply(m: Model): Unit = apply(m, defaultOutputDir, defaultOutputFile)
}


trait HtmlExporter extends FileExporter {
  def preamble(m: Model): String = s"""
     |<!DOCTYPE html>
     |<html>
     |  <head>
     |    <title>${titleOrDefault(m)}</title>
     |  </head>
  """.stripMargin
  def ending(m: Model): String = "</html>\n"
  def body(m: Model): String = 
    "  <body>\n" + exportTopModel(m) + "\n  </body>\n"

  def topLevelSections(m: Model): Seq[String] = m.tip.collect { case s: Section => s.id } 

  def topLevelSectionsContents(m: Model): String = topLevelSections(m).
    map(section => s"""<li><a href="#$section">$section</a></li>""").
    mkString("\n      ")
  
  def sections(m: Model): Map[String, Model] = 
    topLevelSections(m).map(section => (section, m / section)).toMap 
  
  def topExceptSections(m: Model): Model = m * !Section

  def cut(n: Int): Int = Math.min(n,6)

  def renderModelHead(m: Model, level: Int): String = {
    val lvl = cut(level)
    m.get(Title).map(title => s"<h$lvl>$title</h$lvl>").getOrElse("") +
    m.get(Text).map(text => s"<p>$text</p>").getOrElse("")    
  }
  
  def renderModelBody(m: Model, level: Int): String = {
      val lvl = cut(level)
      val elemHead = if ((m.tip - Title - Text).isEmpty || level > 1) "" else s"""
        <h$lvl> <a id="Elements">Elements</a></h$lvl>
      """
      def elemBody(m: Model, indent: String): String = m.toVector.map {
        case Title(t) if level <= 2 => "" //rendered in renderModelHead
        case Text(t) if level <= 2 => "" //rendered in renderModelHead
        case a: Attribute[_]  => indent + s"<li>${a.myType}: ${a.value}</li>"
        case sect: Section => indent + s"TODO: Subsection $sect"
        case e: Entity     => indent + s"<li>${e.myType} ${e.id}</li>"
        case Relation(e, link, submodel) => indent +
          s"<li>${e.myType} ${e.id} $link</li>\n" + elemBody(submodel, indent+(" " * 2))    
      }.mkString("\n" + indent + "<ul>\n", "\n"+indent,"</ul>\n")  
      elemHead + elemBody(m, " " * 8)
  }
  
  def renderSections(m: Model): String = sections(m).map {
    case (section, submodel) => s"""
       <h1>$section</h1>
       ${renderModelHead(submodel, 2)}
       ${renderModelBody(submodel, 2)}
    """     
  }.mkString("\n        ")
  
  def contents(m: Model) = {
    val elementsItem = if ((m.tip - Title - Text).isEmpty) "" else s"""
      <a href="#Elements">Elements</a>
    """
    s"""
      <h1>Contents</h1>
      <ul>
        <li>${elementsItem}</li>
        ${topLevelSectionsContents(m)}
        <li><a href="#model-code">Model Code</a></li>
      </ul>
    """
  }
  def exportTopModel(m: Model) = s"""
    |${renderModelHead(topExceptSections(m), 1)}
    |${contents(m)}
    |${renderModelBody(topExceptSections(m), 1)}
    |${renderSections(m)}
    |<h1><a id="model-code">Model Code</a></h1>
    |<pre>
    |${m.toString} 
    |</pre>
  """.stripMargin

  override def exportModelToFile(m: Model, outDir: String, fileName: String) {
    ( new java.io.File(outDir) ).mkdirs
    val modelString = preamble(m) + body(m) + ending(m)
    modelString.save(outDir+fileUtils.fileSep+fileName)
  }

}




















