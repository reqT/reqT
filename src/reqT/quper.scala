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
import scala.language.implicitConversions

object quper {

  trait Estimate {
    def min: Int
    def max: Int
    def value: Int
  }
  case class PointEstimate(value: Int) extends Estimate {
    override val min = value
    override val max = value
  }
  case class RectangleEstimate(min: Int, max: Int) extends Estimate {
    override val value = math.round((min-max)/2.0).toInt
  }
  case class TriangleEstimate(min: Int, value: Int, max: Int) extends Estimate {
    assert( if (min < max) value >= min && value <= max 
            else           value <= min && value >= max,
      s"triangle estimation value $value outside interval ($min,$max)")
  }
  case object Estimate {
    def apply(min: Int, max: Int) = RectangleEstimate(min, max)
    def apply(value: Int) = PointEstimate(value)
    def apply(min: Int, value: Int, max: Int) = value match {
      case _ if min < max && (value < min || value > max) =>  RectangleEstimate(min, max)
      case _ if min > max && (value > min || value < max) =>  RectangleEstimate(min, max)
      case _ => TriangleEstimate(min, value, max)
    }
  }
  implicit def intToEstimate(i: Int): Estimate = Estimate(i)

  object svg {
    
    def breakpoint(name: String, level: Double, 
           color: String = "lime", dx: Int = 0, dy: Int = 0) = {
      <svg>
        <path d={ s"M${dx + level},${dy + 100} L${dx + level - 50},${dy + 30} L${dx + level + 50},${dy + 30} Z" } style={ s"fill:$color;stroke:gray;stroke-width:5" }/>
        <circle cx={ s"${dx + level}" } cy={ (dy + 100).toString } r="7"/>
        <text x={ s"${dx + level}" } y={ (dy + 120).toString }>{ s"$name $level" }</text>
      </svg>
    }
      
    def axis(name: String, size: Double, dx: Int = 0, dy: Int = 0) = {
      <svg>
        <path d={ s"M${dx + 0},${dy + 100} L${dx + size},${dy + 100}" } style="stroke:gray;stroke-width:5"/>
      </svg>
    }
    
    lazy val color =
      Map("Saturation" -> "dodgerblue",
        "Differentiation" -> "lime",
        "Utility" -> "orangered",
        "Barrier" -> "yellow").withDefaultValue("gray")
    val prettyPrinter = new scala.xml.PrettyPrinter(80, 2)
    val pre = """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1">""" 

    def doc(e: scala.xml.Elem) = pre + "\n" + prettyPrinter.format(e) + "\n</svg>"
  }
  
  case class QuperSpec(
      breakpoints: Map[String, Estimate],  
      barriers: Map[String, Estimate] = Map(),  
      targets: Map[String, Estimate] = Map(),
      references: Map[String, Estimate] = Map()) {
    lazy val values = Vector[Estimate]() ++
      breakpoints.values ++ barriers.values ++ targets.values ++ references.values
    lazy val maxValue = values.map(_.value).max
    lazy val minValue = values.map(_.value).min

    def toSvgElem(dx: Int, dy: Int): scala.xml.Elem = {   
      <svg>
        { quper.svg.axis("", 800, dx, dy) }
        { breakpoints.map{case (b, e) => quper.svg.breakpoint(b, e.value, svg.color(b), dx, dy)} }
      </svg>
    }
      
    def toSvgElem: scala.xml.Elem = toSvgElem(0, 0)
    def toSvgDoc = svg.doc(toSvgElem)
  }
  
  def test1 = QuperSpec(Map("bp1" -> 100, "bp2" -> 200, "bp3" -> 400))
}