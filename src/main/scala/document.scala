package reqt {

  case class Text(paragraphs: List[String]) {
    def toString(wrapper: Any => String) = paragraphs.map(wrapper).mkString
  }
  object Text {
    def apply(p: String *) = new Text(p.toList)
  }

  trait DocItem {
    def title: String 
    def intro: Text
    def extractor: Model => Model
  }

  case class Chapter(
    title: String = "", 
    intro: Text = Text(), 
    extractor: Model => Model = m => Model() 
  ) extends DocItem

  case class Section (
    title: String = "", 
    intro: Text = Text(), 
    extractor: Model => Model = m => Model() 
  ) extends DocItem

  case class DocumentTemplate(title: String, intro: Text, items: List[DocItem]) 
  object DocumentTemplate {
    def apply(title: String, intro: Text, items: DocItem *) = 
      new DocumentTemplate(title, intro, items.toList)
  }
  
  trait DocGenerator {  
    def wrap(pre: Any, post: Any)(s: Any): String = pre.toString + s.toString + post.toString
    def tabulate(m: Model): String
    def document(title: String, intro: Text, rest: String): String
    def chapter(c: Chapter, m: Model): String
    def section(s: Section, m: Model): String
    def text(t: Text): String
    def image(url: String): String 
    def generate(m: Model, dt: DocumentTemplate): String = 
      document(dt.title, dt.intro, dt.items. collect {
          case c: Chapter => chapter(c, m) 
          case s: Section => section(s, m)
        } .mkString  
      )
  }

  trait HtmlGenerator extends DocGenerator {
    def br(s: String): String = s.replaceAllLiterally("\n","<br>")
    def head = wrap("<head>\n","</head>\n") _
    def para = wrap("<p>","</p>\n") _
    def h1 = wrap("<H1>","</H1>\n") _
    def h2 = wrap("<H2>","</H2>\n") _
    def h3 = wrap("<H3>","</H3>\n") _
    def pre = "<!DOCTYPE html>\n<html>"
    def post = "</html>"
    def meta = """<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >"""
    def css = "<link rel=\"stylesheet\" type=\"text/css\" href=\"reqT.css\"> \n"
    def head(title:String) = "<head>\n" + css + meta + "<title>" + title + "</title>\n</head>\n\n"
    def text(t: Text): String = t.toString(para)
    def image(url: String): String = "<p><img src=\"" + url  + "\" alt=\""+ url +"\"></p>\n" 
    def document(title: String, intro: Text, rest: String): String = pre + head(title) + 
      "<body>\n" + h1(title) + text(intro) + rest + "</body>" + post 
    def chapter(c: Chapter, m: Model): String = h2(c.title) + text(c.intro) + "\n" + tabulate(c.extractor(m)) + "\n"
    def section(s: Section, m: Model): String = h3(s.title) + text(s.intro) + "\n" + tabulate(s.extractor(m)) + "\n"
    def tabulate(m: Model): String = {
      def entityDivision = wrap("<div class=\"entity\">\n", "</div>\n") _
      def nodeTable = wrap("<table class=\"nodelist\">\n","</table>\n") _
      val thAttr = "<th>Attributes</th><th>Values</th>"
      val thRel = "<th>Relations</th><th>Destinations</th>"
      def tr = wrap("  <tr>","  </tr>\n") _
      def td = wrap("<td>","  </td>") _
      def tdName = wrap("  <td class=\"name\">","  </td>") _   
      val result: String = m.sources.toList.sorted map { e =>
        val select = m / e 
        val gist = br(select ! Gist getOrElse(""))
        val img = select ! Image match { case Some(url) => image(url); case None => "" }
        val heading = "" + h3("<b>" + e.prefix + " " + e.id + ": </b><i>" + gist + "</i>") + "\n"
        val attr = ( select / has map { case (_,ns) => (ns filterNot( _ <==> Gist)).toList.sorted(nodeOrdering) map { node =>
            val n: Node[_] = node match { case l: External[_] =>  l.fromFile ; case a => a}
            "" + tr(tdName(longName(n.prefix) + ":")  + br(td(n.value))) 
        } mkString("\n")  } ).mkString
        val rel = ( select \ has map { case (Key(_, edge),ns) => "" + tr(tdName(edge.toScala) + td(ns map ( n => 
            "" + n.prefix + " "  + n.value)  mkString(", ") )) } ).mkString
        entityDivision(heading + img +
          { if (attr != "") nodeTable(tr(thAttr) + attr) else "" } + 
          { if (rel != "") nodeTable(tr(thRel) + rel) else "" }
        )
      } mkString("\n") 
      result
    }
  }
  
} //end package reqt