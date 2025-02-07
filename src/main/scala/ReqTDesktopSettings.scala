package reqt

/** Settings for reqT **/
object ReqTDesktopSettings:
  @volatile var indentSpacing = 2
  @volatile var lineLength = 72
  @volatile var columnSeparator = ";"
  @volatile var rowSeparator = "\n"
  // var defaultModelToString: export.StringExporter = export.toScalaCompact
  // var defaultModelToTable: export.StringExporter = export.toPathTable
  // var defaultModelToGraph: export.StringExporter = export.toGraphVizNested
  @volatile var isGeneratingHtmlRawModel = false
  @volatile var isMarkdownSymbolsInToText = false
  @volatile var defaultTitle: String = "untitled"
  @volatile var defaultModelFileName: String = defaultTitle+".reqt"
  @volatile var warningPrinter: String => Unit = (msg) => println(s"WARNING: $msg")

  object gui: 
    @volatile var isPlatformSpecificLookAndFeel = false //true
 
    @volatile var fontSize = 16
    @volatile var editorFonts =  //Mono space fonts in priority order
      List("Liberation Mono", "Fira Code Medium", "JetBrains Mono Medium" , "Cascadia Mono", "Courier New", "Noto Mono", "Consolas", "Droid Sans Mono", "DejaVu Sans Mono", "Source Code Pro Medium", "Monospaced", "Monospace")
    val defaultEditorFont = editorFonts.head
    
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
    val treeBackground = col(230,255,230)
    val logForeground = col(10,10,50)
    val logBackground = col(255,230,230)
  end gui
end ReqTDesktopSettings