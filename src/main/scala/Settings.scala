package reqt

@volatile
object Settings {
  var indentSpacing = 2
  var lineLength = 72
  var columnSeparator = ";"
  var rowSeparator = "\n"
  // var defaultModelToString: export.StringExporter = export.toScalaCompact
  // var defaultModelToTable: export.StringExporter = export.toPathTable
  // var defaultModelToGraph: export.StringExporter = export.toGraphVizNested
  var isGeneratingHtmlRawModel = false
  var isMarkdownSymbolsInToText = false
  var defaultTitle: String = "untitled"
  var defaultModelFileName: String = defaultTitle+".reqt"
  var warningPrinter: String => Unit = (msg) => println(s"WARNING: $msg")
  object gui {
    val entRGB  = (0,30,200) //blueish
    val intAttrRGB = (0,120,50) //greenish
    val strAttrRGB = (180,100,40) //orange-like
    val relRGB = (160,0,30) //reddish
    val strRGB = (200,90,40) //orange-like
    private def col(t: (Int, Int, Int)) = new java.awt.Color(t._1, t._2, t._3)
    val entityColor    = col(entRGB)
    val intAttributeColor = col(intAttrRGB)
    val strAttributeColor = col(strAttrRGB)
    val relationColor  = col(relRGB)
    val stringColor    = col(strRGB)
    val scalaReservedWordColor = col((0,0,125))
    val defaultEditorFont = "DejaVu Sans Mono"
    var editorFonts    = List("Source Code Pro Medium" , "Fira Code Medium", "DejaVu Sans Mono", "JetBrains Mono Medium" , "Consolas", "Liberation Mono", "Monospace")
    var fontSize       = 14
  }
}