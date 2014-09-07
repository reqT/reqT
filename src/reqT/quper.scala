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

object quper {

  object svg {
    
    def breakpoint(name: String, level: Double, 
           color: String = "lime", dx: Int = 0, dy: Int = 0) =
      <svg>
        <path d={ s"M${dx + level},${dy + 100} L${dx + level - 50},${dy + 30} L${dx + level + 50},${dy + 30} Z" } style={ s"fill:$color;stroke:gray;stroke-width:5" }/>
        <circle cx={ s"${dx + level}" } cy={ (dy + 100).toString } r="7"/>
        <text x={ s"${dx + level}" } y={ (dy + 120).toString }>{ s"$name $level" }</text>
      </svg>
      
    def axis(name: String, size: Double, dx: Int = 0, dy: Int = 0) =
      <svg>
        <path d={ s"M${dx + 0},${dy + 100} L${dx + size},${dy + 100}" } style="stroke:gray;stroke-width:5"/>
      </svg>
    
    lazy val color =
      Map("Saturation" -> "dodgerblue",
        "Differentiation" -> "lime",
        "Utility" -> "orangered",
        "Barrier" -> "yellow").withDefaultValue("gray")
  }
  
  case class QuperSpec(
      breakpoints: Map[String, Double],  //Min - max ????
      barriers: Map[String, Double] = Map(),  
      targets: Map[String, Double] = Map(),
      references: Map[String, Double] = Map()) {
    lazy val values = Vector[Double]() ++
      breakpoints.values ++ barriers.values ++ targets.values ++ references.values
    lazy val maxValue = values.max
    lazy val minValue = values.min

    def toSvg(dx: Int, dy: Int): scala.xml.Elem =
      <svg>
        { quper.svg.axis("", 800, dx, dy) }
        { breakpoints.map{case (b, v) => quper.svg.breakpoint(b, v, svg.color(b), dx, dy)} }
      </svg>
    def toSvg: scala.xml.Elem = toSvg(0, 0)
  }
}