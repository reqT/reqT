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

@volatile
object Settings {
  var indentSpacing = 2
  var lineLength = 72
  var columnSeparator = ";"
  var rowSeparator = "\n"
  var defaultModelToString: export.StringExporter = export.toScalaCompact
  var defaultModelToTable: export.StringExporter = export.toPathTable
  var defaultModelToGraph: export.StringExporter = export.toGraphVizNested
  var isGeneratingHtmlRawModel = false
  var defaultTitle: String = "untitled"
  var defaultModelFileName: String = defaultTitle+".reqt"
  var warningPrinter: String => Unit = (msg) => println(s"WARNING: $msg")
  object gui {
    val entRGB  = (0,100,200) //blueish
    val attrRGB = (0,100,50) //greenish
    val relRGB = (160,0,30) //reddish
    val strRGB = (200,90,40) //orange-like
    val entityColor    = new java.awt.Color(entRGB._1,entRGB._2,entRGB._3) 
    val attributeColor = new java.awt.Color(attrRGB._1,attrRGB._2,attrRGB._3) 
    val relationColor  = new java.awt.Color(relRGB._1, relRGB._2, relRGB._3)  
    val stringColor    = new java.awt.Color(strRGB._1, strRGB._2, strRGB._3) 
    var editorFonts    = List("Consolas", "Liberation Mono", "DejaVu Sans Mono", "Monospace")
    var fontSize       = 14
  }
}

