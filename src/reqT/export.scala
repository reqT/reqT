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
object toPathTable extends PathTableExporter
object toReleaseAllocationTable extends ReleaseAllocationTableExporter
object toHtml extends HtmlExporter
object toText extends ModelToTextExporter
object toLatex extends LatexExporter

object toQuperSpec {
  def apply(m: Model) = {
    import quper._
    def mapOf(et: EntityType): Map[String,Estimate] =
      m.atoms.collect{ case Relation(e,l,t) if e.myType == et && t.isDefinedAt(Value) => (e.id,Estimate(t/Value)) }.toMap
    val refs = (m.tip * !(Target || Barrier || Breakpoint)).entities.toSet
    val refMap = m.atoms.collect{ case Relation(e,l,t) if refs.contains(e) && t.isDefinedAt(Value) => (e.id,Estimate(t/Value)) }.toMap
    QuperSpec(mapOf(Breakpoint), mapOf(Barrier), mapOf(Target), refMap)
  }
}

trait Exporter[T] { def apply(m: Model): T }

trait ExporterUtils {
  //string utilities:  (move to Utils ??)
  val q: String = '\"'.toString
  val q3: String = q*3
  val nl = "\n"
  def nlLitteral = """\n"""
  def indent(n: Int): String = " " * (n * Settings.indentSpacing)
  def makeString(a: Any): String = a.toString //ready to override
  //model stuff used by exporters (move some to settings?)
  def defaultTitle: String = "Untitled Model"
  def titleOrDefault(m: Model) =  m.get(Title).getOrElse(defaultTitle)
  def topLevelSections(m: Model): Seq[String] = m.tip.collect { case s: Section => s.id }   
  def submodelOfSectionId(m: Model): Map[String, Model] = 
    topLevelSections(m).map(section => (section, m / Section(section))).toMap 
  def topExceptSections(m: Model): Model = m * !Section
  def titleOrSectionId(m: Model, sectId: String): String = 
    (m / Section(sectId)).get(Title).getOrElse(sectId)
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

trait PathTableExporter extends StringExporter {
  def body(m: Model): String = {
    val row =  m.mapLeafPaths { n => 
      Seq(n.init.toScala, n.lastNode.myType, n.lastNode.toScalaBody) .
        mkString(Settings.columnSeparator) 
    }
    row.mkString(Settings.rowSeparator)
  }
}

trait ReleaseAllocationTableExporter extends StringExporter {
  def body(m: Model): String = {
    def ids(et: EntityType) = m.entities.collect{case e if e.myType == et => e.id}.sorted
    val (feats, rels, ress, shs) = (ids(Feature), ids(Release), ids(Resource), ids(Stakeholder))
    def whichRel(f: String): Option[String] = rels.find(r => (m/Release(r)/Feature(f)/Cost) > 0 )
    def whichCost(f: String, res: String, rel: String) =
       (m / Release(rel) / Resource(res) / Feature(f)).get(Cost)
    val headRow = Vector(Vector("Feature id"),Vector("Release id"), 
      ress.map(_ + " Cost"), shs.map(s => s + " Benefit Prio " + (m/Stakeholder(s)/Prio))).flatten
    val rows = feats.map { f => 
      val rel = whichRel(f).getOrElse("?")
      val costs = ress.map(res => whichCost(f, res, rel).getOrElse(-1111)).toVector
      val benefits = shs.map(s => m/Stakeholder(s)/Feature(f)/Benefit).toVector
      (Vector(f,rel) ++ costs ++ benefits).mkString(Settings.columnSeparator)
    }
    headRow.mkString(Settings.columnSeparator) + Settings.rowSeparator + 
      rows.mkString(Settings.rowSeparator)    
  }
}

trait FileExporter extends StringExporter {
  def defaultOutputDir: String = "output"
  def defaultOutputFile: String 
  def apply(m: Model, outDir: String, fileName: String): Unit = 
    exportModelToFile(m, outDir, fileName)
  def exportModelToFile(m: Model, outDir: String, fileName: String): Unit
}


trait HtmlExporter extends FileExporter {
  override def defaultOutputFile: String = "index.html"
  override def preamble(m: Model): String = s"""
     |<!DOCTYPE html>
     |<html>
     |<head>
     |<title>${titleOrDefault(m)}</title>
     |<link rel="stylesheet" type="text/css" href="reqT-style.css">
     |</head>
  """.stripMargin
  override def ending(m: Model): String = "</html>\n"
  override def body(m: Model): String = 
    "<body>\n" + exportTopModel(m) + "\n</body>\n"

  def topLevelSectionsContents(m: Model): String = topLevelSections(m).
    map(section => s"""<li><a href="#$section">$section</a></li>""").
    mkString("\n      ")

  def cut(n: Int): Int = Math.min(n,6)

  def renderModelHead(m: Model, level: Int): String = {
    val lvl = cut(level)
    m.get(Title).map(title => s"<h$lvl>$title</h$lvl>").getOrElse("") +
    m.get(Text).map(text => s"<p>$text</p>").getOrElse("")    
  }
  
  def mkEnt(e: Entity) = 
            s"""<span class="entityColor">${e.myType}</span> ${e.id}"""
    
  def mkAttr[T](a: Attribute[T]) = 
            s"""<span class="attributeColor">${a.myType}</span>: ${a.value}"""
  
  def mkRel(r: RelationType) = 
            s"""<span class="relationColor">${r.myType}</span>"""
  
  def renderModelBody(m: Model, level: Int): String = {
      def elemBody(m: Model, indent: String): String = m.toVector.map {
        case NoElem => ""
        case Title(t) => "" //rendered in renderModelHead
        case Text(t)  => "" //rendered in renderModelHead
        case Image(f) =>  s"""<img src="$f" alt="$f"/>""" 
        case a: Attribute[_]  => indent + s"<li>${mkAttr(a)}</li>"
        //case sect: Section => indent + s"TODO: Subsection $sect"
        case e: Entity     => indent + s"<li>${mkEnt(e)}</li>"
        case Relation(e, r, submodel) => indent +
          s"<li>${mkEnt(e)} ${mkRel(r)}</li>\n" + elemBody(submodel, indent+("  "))    
      }.mkString("\n" + indent + "<ul>\n", "\n"+indent,"</ul>\n")  
      elemBody(m, "  " * level)
  }
  
  def renderSections(m: Model, level: Int): String = submodelOfSectionId(m).map {
    case (section, submodel) => 
      val lvl = cut(level)
      s"""
       <h$lvl><a id="$section">$section</a></h$lvl>
       ${renderModelHead(submodel, level+1)}
       ${renderModelBody(submodel, level+1)}
      """     
  }.mkString("\n        ")
  
  def contents(m: Model, level: Int) = {
    val tlsc = topLevelSectionsContents(m)
    val lvl = cut(level)
    val modelCodeHeading = 
      if (level < 3 || !Settings.isGeneratingHtmlRawModel) "" 
      else s"""<li><a href="#model-code">Raw model</a></li>"""
    if (tlsc.isEmpty) ""
    else s"""
      |<h$lvl>Contents</h$lvl>
      |<ul>
      |  $tlsc
      |  $modelCodeHeading
      |</ul>
      """.stripMargin 
  }
  def modelCode(m: Model) = if (!Settings.isGeneratingHtmlRawModel) "" else s"""
    |<h2><a id="model-code">Raw model</a></h2>
    |<pre>
    |${m.toString} 
    |</pre>
  """
  def exportTopModel(m: Model) = s"""
    |${renderModelHead(topExceptSections(m), 1)}
    |${renderModelBody(topExceptSections(m), 1)}
    |${contents(m, 2)}
    |${renderSections(m, 2)}
    |${modelCode(m)}
  """.stripMargin

  override def exportModelToFile(m: Model, outDir: String, fileName: String) {
    ( new java.io.File(outDir) ).mkdirs
    apply(m).save(outDir+fileUtils.fileSep+fileName)
  }

}

trait ModelToTextExporter extends StringExporter {
  def emptyModelString: String = "()"

  def indentBy(path: NodePath): String = " " * (Settings.indentSpacing * (path.depth - 1))
  
  override def makeString(a: Any): String = a match {
    case e: Entity => 
      val s = e.prefix match {
        case "Section" => "#"
        case "Item" => "*"
        case p => p
      }      
      s + " " +e.id
    case a: Attribute[_] => (a.prefix match { case "Text" => ""; case p => p +" "} )+ a.value
    case _ => a.toString
  }
  
  def exportModel(m: Model, path: NodePath): String = m.toVector.map(exportElem(_, path)).mkString 
  
  def exportElem(elem: Elem, path: NodePath): String = elem match {
    case NoElem => ""
    case e: Entity => exportEntity(e, path / e) 
    case a: Attribute[_] => exportAttribute(a, path / a)
    case r: Relation => exportHead(r.head, path / r.head) + exportModel(r.tail, path / r.head) 
  }
  
  def exportLink(l: RelationType): String = if (l == has) "" else " " + l.toString   
  def exportHead(h: Head, path: NodePath): String = 
    indentBy(path) + makeString(h.entity) + exportLink(h.link) + "\n"
  def exportEntity(e: Entity, path: NodePath): String = indentBy(path) + makeString(e) + "\n"
  def exportAttribute[T](a: Attribute[T], path: NodePath): String =  a match {
    case xs: VectorAttribute[_] => 
      xs.value.map(x => indentBy(path) + xs.prefix + " "+ x).mkString("\n")
    case _ => indentBy(path) + makeString(a) + "\n"
  }
  
  override def preamble(m: Model): String = ""
  override def ending(m: Model): String = ""
  override def body(m: Model): String = exportModel(m, /)
}

trait LatexExporter extends FileExporter {
  import Settings.gui.{entRGB, attrRGB, relRGB}
  import metamodel.{entityTypes, attributeTypes, relationTypes}
  override def defaultOutputFile: String = "model.tex"
  override def preamble(m: Model): String = s"""
\\documentclass[11pt,a4paper,oneside]{report}
\\usepackage{hyperref}
\\hypersetup{colorlinks=true, linkcolor=blue, urlcolor=blue}
\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\\definecolor{entityColor}{RGB}{${entRGB._1},${entRGB._2},${entRGB._3}}
\\definecolor{attributeColor}{RGB}{${attrRGB._1},${attrRGB._2},${attrRGB._3}}
\\definecolor{relationColor}{RGB}{${relRGB._1},${relRGB._2},${relRGB._3}}
\\usepackage{listings}
\\lstdefinestyle{reqT}{
  belowcaptionskip=1\\baselineskip,
  breaklines=true,
  showstringspaces=false,
  basicstyle=\\footnotesize\\sffamily,
  emph={${entityTypes.mkString(",")}},
  emphstyle=\\bfseries\\color{entityColor},
  emph={[2]${relationTypes.mkString(",")}},
  emphstyle={[2]\\bfseries\\color{relationColor}},
  emph={[3]${attributeTypes.mkString(",")}},
  emphstyle={[3]\\color{attributeColor}},  
}
\\lstset{style=reqT}

\\begin{document}
\\title{${titleOrDefault(m)}}
\\author{Generated by \\href{http://reqT.org}{reqT.org}}
\\maketitle
\\tableofcontents  
  """
  override def ending(m: Model): String = "\\end{document}"
  
  lazy val heads = Vector("chapter","section","subsection","subsubsection")
  
  override def body(m: Model): String = {
    val firstChapter = m.get(Title).getOrElse("Model")
    val init = if ((topExceptSections(m) - Title).isEmpty) "" 
      else s"\\${heads(0)}{${firstChapter}}\n" + (m.get(Text).getOrElse("")) 
    s"""
${init}
${renderModelListing(topExceptSections(m - Text - Title))}
${renderSections(m, 0)}
    """.trim
  } 
  
  def cut(n: Int): Int = Math.min(n,heads.size-1)
  
  def renderModelListing(m: Model) = s"""
\\begin{lstlisting}
${m.toText}
\\end{lstlisting}
"""

  def renderSubmodel(title: String, m: Model, level: Int) = 
    if (level >= heads.size) 
      renderModelListing(m)
    else {
      val init = s"\\${heads(level)}{${title}}\n" + (m.get(Text).getOrElse("")) 
      s"""
${init}
${renderModelListing(topExceptSections(m - Text - Title))}
${renderSections(m, level)}
      """.trim
    }
  
  def renderSections(m: Model, level: Int): String = submodelOfSectionId(m).map {
    case (title, submodel) => s"""
       ${renderSubmodel(submodel.get(Title).getOrElse(title), submodel, level+1)}
    """     
  }.mkString("\n        ")
  
  override def exportModelToFile(m: Model, outDir: String, fileName: String) {
    ( new java.io.File(outDir) ).mkdirs
    val docPath = outDir+fileUtils.fileSep+fileName.newFileType("-doc.tex")
    val bodyPath = outDir+fileUtils.fileSep+fileName 
    lazy val modelDoc = preamble(m) + s"""\\input{$fileName}\n""" + ending(m)
    if (!( new java.io.File(docPath) ).exists) modelDoc.save(docPath)
    body(m).save(bodyPath)
  }

}


















