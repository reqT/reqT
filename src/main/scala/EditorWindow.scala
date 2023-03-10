package reqt

import reqt.SwingPlatform.*
import reqt.Sys.newFileType
import reqt.Sys.saveTo
import reqt.Sys.loadLines

import java.awt.event.KeyEvent.*
import java.awt.event.ActionEvent.{CTRL_MASK => CTRL, ALT_MASK => ALT, SHIFT_MASK => SHIFT}

import javax.swing.JPanel
import javax.swing.JFrame
import javax.swing.JComponent
import javax.swing.UIManager
import javax.swing.plaf.FontUIResource
import java.awt.Font
import javax.swing.JEditorPane
import javax.swing.JScrollPane
import java.awt.Dimension
import javax.swing.WindowConstants
import javax.swing.JSplitPane
import javax.swing.JMenuBar
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.Action
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import javax.swing.text.DefaultCaret

object EditorWindow:
  val x = 42
  @volatile private var n = 0
  def initFileName = s"untitled-$n.reqt"

  val initMessage = 
    s"""|WELCOME to the reqT requirements model editor! https//reqt.github.io/
        |Press CTRL+SPACE for completion. Completion is not case-sensitive.
        |Entities are blue and bold. 
        |Attributes are bold + italic. String Attributes are orange. Int Attributes are green.
        |Relations are red and bold + underlined.
        |""".stripMargin

class EditorWindow() extends JFrame:
  EditorWindow.n += 1
  @volatile private var isSaved = true
  def saveNeeded(): Unit = {isSaved = false; updateTitle() }
  def didSave(): Unit = {isSaved = true; updateTitle() }

  val initModel: Model = Model()
  val windowType = "reqT Editor"
  val frame = this

  val initEditorWidth = 80
  val initEditorHeight = 30
  val maxFontSize = 80
  val bigFontSize = 48
  val mediumFontSize = 18
  val minFontSize = 6
  
  private var _fileName = Sys.workDir + "/" + EditorWindow.initFileName
  def fileName = _fileName 
  
  def windowTitle = fileName + "  -  " + windowType

  def updateTitle() = frame.setTitle(windowTitle + (if isSaved then "" else " * unsaved"))
  
  def updateFileName(fn: String) = { _fileName = fn; updateTitle() }

  def doFileNew(): Unit = new EditorWindow()
  
  def doOpen(): Unit = 
    for f <- chooseFile() do 
      val t = loadLines(f).mkString("\n")
      textArea.setText(t)
      updateFileName(f)
      didSave()
  
  def doSave(): Unit = 
    textArea.getText().saveTo(fileName)
    didSave()
    

  val initMenus =
    AppMenus(
      Menu("File", mnemonic = VK_F,
        Item("New Window", VK_N, VK_N, CTRL){ doFileNew() },
        Item("Open File ...", VK_O, VK_O, CTRL){ doOpen() },
        Item("Save", VK_S, VK_S, CTRL){ doSave() },
      ),
      Menu("Edit", mnemonic = VK_E, Seq()*)
    )

  val menuMap: Map[String, JComponent] = initMenus.installTo(frame)

  def doMsg(msg: String): Unit = msgInfo(msg, parent = Some(frame))
  
  val defaultGlobalFontSize = 12 + fontDeltaByScreenHeight

  def fontDeltaByScreenHeight =
    java.awt.Toolkit.getDefaultToolkit.getScreenSize.getHeight match {
      case n if n <= 600 => 0
      case n if n <= 720 => 1
      case n if n <= 800 => 2
      case n if n <= 1024 => 6
      case n if n <= 1080 => 7
      case n if n <= 1440 => 8
      case _         => 10
    }

  def setGlobalSwingFontSize(size: Int): Unit = {
    import scala.jdk.CollectionConverters.*

    val enumKeys: Iterator[Object] = UIManager.getLookAndFeelDefaults.keys.asScala

    enumKeys
      .map(x => (x, UIManager.get(x))) // k => (k, v)
      .filter(_._2.isInstanceOf[FontUIResource])
      .map{ case (k, v) => (k, v.asInstanceOf[FontUIResource]) }
      .foreach{ case (k, v) => UIManager.put(k, new FontUIResource(v.getFamily, v.getStyle, size)) }

    val ff = frame.getFont

    if ff != null then 
      frame.setFont(new Font(ff.getFamily, ff.getStyle, size))
      setEditorFont(frame.getFont.getSize) // handle override of editor font size

    javax.swing.SwingUtilities.updateComponentTreeUI(frame)
  }

  //--- begin rsyntaxtextarea stuff  TODO
  // import org.fife.ui.autocomplete._
  import org.fife.ui.rtextarea.*
  import org.fife.ui.rsyntaxtextarea.*

  def setEditorFont(fontSize: Int, fontFamily: String = "") = runInSwingThread:
    val fn = if (fontFamily == "") textArea.getFont.getFamily else {
      val available = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
      val possible = (fontFamily :: Settings.gui.editorFonts).filter(available.contains(_))
      possible.headOption.getOrElse(Font.MONOSPACED)
    }
    val fPlain = new Font(fn, Font.PLAIN, fontSize)
    val fBold = new Font(fn, Font.BOLD, fontSize)

    import java.awt.font.TextAttribute
    val ta: java.util.Map[TextAttribute, Object] = new java.util.HashMap()
    ta.put(TextAttribute.FONT, fBold)
    ta.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON)
    val fBoldUL = Font.getFont(ta)

    val fBoldItalic = new Font(fn, Font.BOLD | Font.ITALIC, fontSize)

    textArea.setFont(fPlain)
    
    textArea.getSyntaxScheme.setStyle(ReqTTokenMaker.EntTokenType, new Style(Settings.gui.entityColor, Style.DEFAULT_BACKGROUND, fBold))
    
    textArea.getSyntaxScheme.setStyle(ReqTTokenMaker.StrAttrTokenType,   new Style(Settings.gui.strAttributeColor, Style.DEFAULT_BACKGROUND, fBoldItalic))
    
    textArea.getSyntaxScheme.setStyle(ReqTTokenMaker.IntAttrTokenType,   new Style(Settings.gui.intAttributeColor, Style.DEFAULT_BACKGROUND, fBoldItalic))

    textArea.getSyntaxScheme.setStyle(ReqTTokenMaker.RelTokenType,    new Style(Settings.gui.relationColor, Style.DEFAULT_BACKGROUND, fBoldUL))
    
    // textArea.getSyntaxScheme.setStyle(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, new Style(Settings.gui.stringColor))
    
    // textArea.getSyntaxScheme.setStyle(TokenTypes.RESERVED_WORD, new Style(Settings.gui.scalaReservedWordColor, Style.DEFAULT_BACKGROUND, fBold)) // more discrete coloring???

    val lnf = textPane.getGutter.getLineNumberFont
    val lnfNew = new Font(lnf.getFamily, lnf.getStyle, fontSize - 3)
    textPane.getGutter.setLineNumberFont(lnfNew)
  end setEditorFont
  
  val panel = JPanel(java.awt.BorderLayout())
  val textArea = new RSyntaxTextArea(initEditorHeight, initEditorWidth) 
  //textArea.setSyntaxEditingStyle(org.fife.ui.rsyntaxtextarea.SyntaxConstants.SYNTAX_STYLE_JAVA)
  //textArea.setSyntaxEditingStyle(org.fife.ui.rsyntaxtextarea.SyntaxConstants.SYNTAX_STYLE_SCALA)

  // textArea.getSyntaxScheme.setStyle(org.fife.ui.rsyntaxtextarea.TokenTypes.IDENTIFIER,
  //   new org.fife.ui.rsyntaxtextarea.Style(Settings.gui.attributeColor, 
  //     org.fife.ui.rsyntaxtextarea.Style.DEFAULT_BACKGROUND, new Font(java.awt.Font.MONOSPACED, Font.BOLD, mediumFontSize)))

  textArea.setSyntaxEditingStyle("text/reqT")

  textArea.setCodeFoldingEnabled(true)
  textArea.setAntiAliasingEnabled(true)
  textArea.setAutoIndentEnabled(true)

  textArea.setBracketMatchingEnabled(true)
  textArea.setLineWrap(true)
  textArea.setWrapStyleWord(true)
  textArea.setTabSize(2)
  textArea.setTabsEmulated(true)
  
  textArea.setMatchedBracketBGColor(new java.awt.Color(247, 247, 247))
  textArea.setMatchedBracketBorderColor(new java.awt.Color(192, 192, 192))
  textArea.setAnimateBracketMatching(true)
  
  setEditorFont(mediumFontSize, Settings.gui.defaultEditorFont)
  val textPane = new org.fife.ui.rtextarea.RTextScrollPane(textArea) with AntiAliasing
  
  import org.fife.ui.autocomplete.*
  val provider = new DefaultCompletionProvider()
  meta.entityNames.foreach: t =>
    provider.addCompletion( new BasicCompletion(provider, t.toString, "Entity"))
  meta.strAttrNames.foreach: t =>
      provider.addCompletion( new BasicCompletion(provider, t.toString, "String Attribute"))
  meta.intAttrNames.foreach: t =>
      provider.addCompletion( new BasicCompletion(provider, t.toString, "Integer Attribute"))
  meta.relationNames.foreach: t =>
      provider.addCompletion( new BasicCompletion(provider, t.toString, "Relation"))

  val ac = new AutoCompletion(provider)
  ac.install(textArea)

  val editMenu: JMenu = menuMap("Edit").asInstanceOf[JMenu]

  def createEditMenuItem(action: Action): JMenuItem = 
    val item = new JMenuItem(action)
    item.setToolTipText(null) // Swing annoyingly adds tool tip text to the menu item
    item 

  def addEditMenuAction(actions: Int*): Unit = 
    actions.foreach(a => editMenu.add(createEditMenuItem(RTextArea.getAction(a))))

  def createEditMeny(): Unit =
    import RTextArea.{UNDO_ACTION, REDO_ACTION, CUT_ACTION, COPY_ACTION, PASTE_ACTION, DELETE_ACTION, SELECT_ALL_ACTION} 
    addEditMenuAction(UNDO_ACTION, REDO_ACTION)
    editMenu.addSeparator()
    addEditMenuAction(CUT_ACTION, COPY_ACTION, PASTE_ACTION, DELETE_ACTION)
    editMenu.addSeparator()
    addEditMenuAction(SELECT_ALL_ACTION)
  
  createEditMeny()
  

  //--- end rsyntaxtextarea stuff  TODO
  val messageArea = new javax.swing.JTextArea(10, initEditorWidth)
  messageArea.setEditable(false);
  messageArea.setFont(new Font("Monospace", Font.PLAIN, 18))
  val caret = textArea.getCaret().asInstanceOf[javax.swing.text.DefaultCaret]
  caret.setUpdatePolicy(javax.swing.text.DefaultCaret.ALWAYS_UPDATE)
  messageArea.setLineWrap(true)
  messageArea.append(EditorWindow.initMessage)
  val messagePane = new javax.swing.JScrollPane(messageArea)

  val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT)
  splitPane.setTopComponent(textPane)
  splitPane.setBottomComponent(messagePane)
  val (startHeight, startWidth) = (768, 1024)
  val smallestDim = new Dimension(100, 1)
  val prefferedDim = new Dimension(startWidth, startHeight)
  textPane.setMinimumSize(smallestDim)
  messagePane.setMinimumSize(smallestDim)
  splitPane.setPreferredSize(prefferedDim)
  

  panel.add(splitPane)
  setContentPane(panel)

  setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)  //EXIT_ON_CLOSE

  textArea.getDocument().addDocumentListener( 
    new DocumentListener:
      override def changedUpdate(e: DocumentEvent): Unit = saveNeeded()
      override def insertUpdate(e: DocumentEvent): Unit = saveNeeded()
      override def removeUpdate(e: DocumentEvent): Unit = saveNeeded()
  )

  textPane.updateUI
  pack()
  setLocationRelativeTo(null)
  setVisible(true)
  splitPane.setDividerLocation(0.8)
  updateTitle()

  // ---- Body of DesktopGUI

  // //--- begin instead of rsyntaxtextarea
  // val editor = new JEditorPane();
  // editor.setEditable(true);
  // editor.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 20));
  // editor.setContentType("text/html");
  // val editorView = new JScrollPane(editor)
  // def updateEditor() = editorView.updateUI
  
  // // --- end instead of rsyntaxtextarea

  //val splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT)

  //val splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT)
  //splitPane.setTopComponent(treeView)
  //splitPane.setTopComponent(editorView)
  //splitPane.setBottomComponent(editorView)
  //val (startHeight, startWidth) = (768, 1024)
  //val smallestDim = new Dimension(100, 100)
  //val prefferedDim = new Dimension(startWidth, startHeight)
  //editorView.setMinimumSize(smallestDim)
  //editorView.setPreferredSize(prefferedDim)
  //  //treeView.setMinimumSize(smallestDim)
  //splitPane.setPreferredSize(prefferedDim)
  //add(splitPane)
  //add(editorView)

  //frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  //frame.add(this)
  //frame.pack()
  //setGlobalSwingFontSize(defaultGlobalFontSize)
  //setEditorFont(Settings.gui.fontSize + fontDeltaByScreenHeight,
  //  Settings.gui.editorFonts.headOption.getOrElse(Font.MONOSPACED))
  //frame.setVisible(true)
  //splitPane.setDividerLocation(0.5)  //(startWidth / 2)
  //updateEditor()
end EditorWindow