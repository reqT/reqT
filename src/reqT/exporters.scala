package reqT
package exporters

trait ModelExporter[T] {
  def apply(model: Model): T
}

case object Simple extends ModelExporter[String] {
  def apply(model: Model): String = model.toStringSimple
}

trait ModelToString extends ModelExporter[String] {
  def emptyModelString: String = "()"
  
  def levelTab(level: Int): String   = " " * (level * Settings.intentSpacing)
  def indent(n: Int): String = "\n" + levelTab(n)
  
  def indentCheck(m: Model, path: NodePath): String = {
    val space = indent(path.level + 1)
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
  
  def preamble: String = "Model("
  def ending(m: Model): String = ")"

  def apply(model: Model): String = preamble + exportModel(model, /) + ending(model)
}


trait NewLineEnding { self: ModelToString =>
  override def modelPost(model: Model, path: NodePath) = indentCheck(model, path) + ")"
  override def ending(model: Model) = if (model.toStringBody.length > Settings.lineLength) "\n)" else ")" 
}  

case object PrettyCompact extends ModelToString 

case object Pretty extends ModelToString with NewLineEnding

trait ScalaGenerators { self: ModelToString =>
  override def exportEntity(e: Entity, path: NodePath): String = e.toScala
  override def exportAttribute[T](a: Attribute[T], path: NodePath): String =  a.toScala
}

case object ScalaCompact extends ModelToString with ScalaGenerators 

case object Scala extends ModelToString with ScalaGenerators with NewLineEnding





















