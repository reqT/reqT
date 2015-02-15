/***     
**                  _______        
**                 |__   __|   reqT - a requirements engineering tool  
**   _ __  ___   __ _ | |      (c) 2011-2014, Bjorn Regnell, Lund University  
**  |  __|/ _ \ / _  || |      http://reqT.org
**  | |  |  __/| (_| || |   
**  |_|   \___| \__  ||_|   
**                 | |      
**                 |_|      
** reqT is open source, licensed under the BSD 2-clause license: 
** http://opensource.org/licenses/bsd-license.php 
**************************************************************************/
package reqT
import scala.language.implicitConversions

object svgUtil {
  val prettyPrinter = new scala.xml.PrettyPrinter(80, 2)
  val svgPreamble = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1">""" 
  def svgDoc(e: scala.xml.Elem): String = svgPreamble + "\n" + prettyPrinter.format(e) + "\n</svg>"
  object xmlDSL {
    type XmlMap = Map[String,String]
    implicit class RichXmlMap(sm: Map[String,String]) {
      def toXmlAttr: String = 
        sm.filterNot{ case (id, value) => value == "" }
          .map{ case (id, value) => s"""$id="$value"""" } .mkString(" ")
      def *(that: Map[String,String]) = sm ++ that
      def ->(tag: String, inner: String = "") = 
        scala.xml.XML.loadString(s"<$tag ${toXmlAttr}>$inner</$tag>")
      def ->(tag: String, inner: scala.xml.Elem, more: scala.xml.Elem*) = { 
        val first = prettyPrinter.format(inner)
        val rest = more.map(e => "\n"+prettyPrinter.format(e)).mkString
        scala.xml.XML.loadString(s"<$tag ${toXmlAttr}>${first+rest}</$tag>")
      }
    }
    def <> = Map[String,String]() 
    def pos(): XmlMap = pos(0,0)
    def pos(x: Double, y: Double) = Map("x" -> x.toString, "y" -> y.toString)
    def size(w: Double, h: Double) = Map("width" -> w.toString, "height" -> h.toString)
    def font(family: String = "Arial, Helvetica, sans-serif", size: Double = 20.0) = Map("font-family" -> family, "font-size" -> size.toString)
    def pen(stroke:String="black", fill: String="", width: Double=2) =
      Map("stroke" -> stroke, "fill" -> fill, "strokeWidth" -> width.toString)
    def textBox(t: String, midX: Double, midY: Double, w: Double = 200, h: Double = 80, textOffset: Double = 8) = {
      val (x,y) = (midX - w/2, midY - h/2 - textOffset)
      <> -> ("g",
        pos(x,y)*size(w,h)*pen("black","white")  -> "rect",
        pos(midX,midY)*font() + ("text-anchor" -> "middle") -> ("text",t)
      )
    }
  }
}

object svgDiagrams {
  import svgUtil._
  import xmlDSL._
  trait InteractorShape
  case object SystemBox extends InteractorShape
  case object Strawman extends InteractorShape
  case class Interactor(
               name: String, 
               interface: Option[String] = None, 
               shape: InteractorShape = SystemBox)
  case class ContextDiagram(system: String, interactions: Interactor*) {
    val n = interactions.size
    lazy val shapes = for (i <- 1 to n) yield {
      import math._    
      val name = interactions(i-1).name
      val angle = Pi*2 / n
      textBox(name, 500 + cos(i*angle)*400, 500 + sin(i*angle)*400)
    }
    def toSvgElem: scala.xml.Elem = size(1000,1000) -> (
      "svg", textBox(system,500,500), shapes:_*
    )
    def toSvgDoc = svgDoc(toSvgElem)
  }
}