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
    
    def breakpoint(name: String, qualityLevel: Int, normalizer: Int => Double, 
                   color: String = "lime", dx: Int = 0, dy: Int = 0) = {
      val level = normalizer(qualityLevel)
      val (x, b, h, len, txt, ang) = (dx + level, 50, 30, 100, 120, 90) 
      val opac="fill-opacity:0.8;opacity:0.8;"
      <svg id={s"breakpoint_$name"}>
        <path d={ s"M$x,${dy+len} L${x-b},${dy+h} L${x+b},${dy+h} Z" } style={ s"fill:$color;stroke:gray;stroke-width:5;$opac" }/>
        <circle cx={ s"$x" } cy={ (dy+len).toString } r="7"/>
        <text x={ s"$x" } y={ (dy+txt).toString } transform={s"rotate($ang ${x-5},${dy+txt+2})"}>{ s"$qualityLevel $name" }</text>
      </svg>
    }
    
    def barrier(name: String, qualityLevel: Int, normalizer: Int => Double, 
                color: String = "rgb(255,220,0)", dx: Int = 0, dy: Int = 0) = {
      val level = normalizer(qualityLevel)
      val (x,b,h,len,txt,ang) = (dx+level,15,90,100,120,90)
      val opac="fill-opacity:0.8;opacity:0.8;"
      <svg id={s"barrier_$name"}> 
        <path d={ s"M${x-b},${dy+len} L${x-b},${dy+len-h} L${x+b},${dy+len-h} L${x+b},${dy+len} Z" } style={ s"fill:$color;stroke:gray;stroke-width:5;$opac" }/>
        <text x={ s"${x-10}" } y={ (dy+txt).toString }>{ s"$qualityLevel Barrier $name" }</text>
      </svg>
    }
    
    def marker(markerType: String, name: String, qualityLevel: Int, normalizer: Int => Double,
               angle: Double = -30, color: String = "blue", dx: Int = 0, dy: Int = 0) = {
      val level = normalizer(qualityLevel)
      val (x, len, txt, markerSize) = (dx + level, 100, 120, 10) 
      val opac="fill-opacity:0.8;opacity:0.8;"
      val fontW = "bold"
      <svg id={s"${markerType}_$name"}> 
        <circle cx={ s"$x" } cy={ (dy+len).toString } r={s"$markerSize"} style={ s"stroke:$color;fill:$color;$opac" }/>  
        <text x={ s"$x" } y={ (dy+txt).toString } transform={s"rotate($angle ${x-60},${dy+txt-20})"} font-weight={fontW}>{ s"$qualityLevel $markerType $name" }</text>
      </svg>
    }
      
    def axis(name: String, size: Double, dx: Int = 0, dy: Int = 0) = {
      <svg id="x_axis">
        <path d={ s"M${dx + 0},${dy + 100} L${dx + size},${dy + 100}" } style="stroke:gray;stroke-width:5"/>
      </svg>
    }
    
    lazy val color = Map(
        "satu" -> "dodgerblue",
        "diff" -> "lime",
        "util" -> "orangered",
        "barr" -> "rgb(255,220,0)",
        "targ" -> "red",
        "refe" -> "rgb(80,45,185)").withDefaultValue("gray")
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
      val (axisLength, imageHeight) = (600.0, 300)
      def normalize(value: Int): Double = axisLength*(value-minValue)/(maxValue-minValue)    
      <svg width={s"${axisLength+200}"} height={s"$imageHeight"} font-family="sans-serif">
        { quper.svg.axis("", axisLength, dx, dy) }
        { breakpoints.map{case (b, e) => 
            quper.svg.breakpoint(b, e.value, normalize, svg.color(b.toLowerCase.take(4)), dx, dy)} }
        { barriers.map{case (b, e) => 
            quper.svg.barrier(b, e.value, normalize, svg.color("barr"), dx, dy)}}
        { targets.map{case (t, e) => 
            quper.svg.marker("Target",t, e.value, normalize, -30, svg.color("targ"), dx, dy)}}
        { references.map{case (t, e) => 
            quper.svg.marker("Ref",t, e.value, normalize, -20, svg.color("refe"), dx, dy)}}
      </svg>
    }
      
    def toSvgElem: scala.xml.Elem = toSvgElem(100, 0)
    def toSvgDoc = svg.doc(toSvgElem)
  }
  
  def test1 = QuperSpec(
    breakpoints=Map("Utility" -> 100, "Differentiation" -> 200, "Saturation" -> 400),
    barriers=Map("1" -> 220, "2" -> 370), targets= Map("easy"-> 259, "stretch"->390),
    references= Map("competitor"-> 129, "old"->190))
}