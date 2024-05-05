package reqt

import reqt.Sys.saveTo
import reqt.Sys.loadLines

import reqt.SwingPlatform.mkMenuItem
import reqt.SwingPlatform.runInSwingThread

import java.awt.Dimension
import java.awt.Font
import java.awt.event.ActionEvent.{CTRL_MASK => CTRL, ALT_MASK => ALT, SHIFT_MASK => SHIFT}
import java.awt.event.KeyEvent.*
import java.awt.event.WindowEvent
import java.awt.event.WindowListener
import java.awt.event.WindowAdapter

import javax.swing.JPanel
import javax.swing.JFrame
import javax.swing.JComponent
import javax.swing.JEditorPane
import javax.swing.JScrollPane
import javax.swing.JSplitPane
import javax.swing.JMenuBar
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.Action
import javax.swing.LookAndFeel
import javax.swing.UIManager
import javax.swing.WindowConstants
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import javax.swing.text.DefaultCaret
import javax.swing.plaf.FontUIResource
import javax.swing.JTextArea
import javax.swing.event.TreeSelectionListener
import javax.swing.event.TreeSelectionEvent
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.JTree
import javax.swing.tree.TreeSelectionModel
import javax.swing.tree.TreePath
import javax.swing.tree.DefaultTreeModel
import javax.swing.DropMode
import javax.swing.tree.TreeNode

object EditorWindow:
  val initLookAndFell = javax.swing.UIManager.getLookAndFeel()
  SwingPlatform.swingInit(isPlatformSpecific = Settings.gui.isPlatformSpecificLookAndFeel)

  private val started = collection.mutable.Buffer.empty[EditorWindow]
  
  @volatile private var n = 0
  
  def nbrWindows: Int = n

  def get(i: Int): Option[EditorWindow] = started.lift(i)

  def newWindow(): Unit = runInSwingThread(started.append(EditorWindow())) 

  def initFileName = s"untitled-$n.md"

  val reqTGist = 
    s"""|* System: reqT has
        |  * Gist: reqT is a requirements tool.
        |  * Feature: showConcepts has
        |    * Spec: Show all concept definitions.
        |    * Design: showConceptUI has
        |      * Spec: Show concepts in Log pane. Shortcut: Alt+C.
        |""".stripMargin.toModel.toMarkdown

  val initMessage = 
    s"""|WELCOME to reqT - a requirements modeling tool! 
        |
        |Read the docs: https//github.com/reqT/reqT
        |
        |Three independent panes: Tree, Editor, Log
        |
        |F1 for help text to Log.
        |F9 to Toggle Orientation.
        |F10 and arrows to discover all short-cuts.
        |F11 to toggle full screen.
        |CTRL+TAB toggle pane focus: editor or log.
        |CTRL+A Select all in focused Editor or Log.
        |PAGE UP/DOWN Scroll focused pane.
        |CTRL+PAGE UP/DOWN Scroll focused pane top/bottom.
        |CTRL+DEL Delete from cursor to end of line.
        |
        |The syntax is based om bullet lists,
        |with asterisk followed by entity or attribute.
        |Indent when relations connect sub-elements.
        |Colons are optional.
        |
        |Example:
        |
        |$reqTGist
        |Use CTRL+SPACE for completion in editor.
        |""".stripMargin

  trait ModelTreeSelectionListener extends TreeSelectionListener:
    override def valueChanged(e: TreeSelectionEvent): Unit = 
      //println(s"ModelTreeSelectionListener event valueChanged: $e")
      //println("TODO: push to Undo-stack for Tree")
      ()

  /** A handle to the root node of the tree pane */
  class TreeRoot(val title: String): 
    override def toString = s"Tree $title"

  enum TreeItemShow { case Markdown, Factory, Structure }

  type TreeItem = Link | Ent | Attr[?]
  class TreeItemBox(val item: TreeItem, ew: EditorWindow):
    override def toString: String = 
        ew.treeItemShow match 
          case TreeItemShow.Markdown => item match
              case l: Link    => s"${l.e.t}: ${l.e.id} ${l.t.toString.toLowerCase}"
              case e: Ent     => s"${e.t}: ${e.id}" 
              case a: Attr[?] => s"${a.t}: ${a.value}"
          case TreeItemShow.Factory => item match
            case l: Link    => l.show
            case e: Ent     => e.show 
            case a: Attr[?] => a.show
          case TreeItemShow.Structure => item match
              case l: Link    => s"Rel(${l.e},${l.t},..."
              case e: Ent     => e.toString 
              case a: Attr[?] => a.toString

  

class EditorWindow private () extends JFrame with EditorWindow.ModelTreeSelectionListener:
  EditorWindow.n += 1
  @volatile private var isSavedTree = true
  @volatile private var isSavedEditor = true

  def saveTreeNeeded(): Unit = { isSavedTree = false; updateTitle() }
  def didSaveTree(): Unit = { isSavedTree = true; updateTitle() }

  def saveEditorNeeded(): Unit = { isSavedEditor = false; updateTitle() }
  def didSaveEditor(): Unit = { isSavedEditor = true; updateTitle() }

  val initModel: Model = Model()  
    // TODO: make initModel a class param an implement menu item "revert to initModel"
  
  val windowType = s"reqT Editor v${Main.reqTVersion}"
  val frame = this

  val initEditorWidth = 80
  val initEditorHeight = 30
  val maxFontSize = 80
  val bigFontSize = 48
  val mediumFontSize = Settings.gui.fontSize
  val minFontSize = 6
  
  private var _fileName = EditorWindow.initFileName
  private var _workDir = Sys.workDir
  def workDir = _workDir
  def fileName = _fileName 
  def filePath = workDir + "/" + fileName
  
  def windowTitle = filePath + "  -  " + windowType

  def updateTitle() = 
    var unsaved = Set.empty[String]
    def isUnsaved = !isSavedTree || !isSavedEditor 
    if !isSavedTree then unsaved += " Tree"
    if !isSavedEditor then unsaved += " Editor"
    val unsavedText = 
      if unsaved.isEmpty then "" 
      else unsaved.toSeq.sorted.reverse.mkString(" unsaved:", ", ", "")
    frame.setTitle(windowTitle + unsavedText)
  
  def updateFileName(fn: String) = { _fileName = fn; updateTitle() }

  object SplitPaneState:
    // all this state mirroring is needed as split pane is set by fraction and not absolute
    // because of missing methods in swing api and we want to remember the pane state 
    // before full screen not to scramble the split location when F11 toggle
    val initialSplit = JSplitPane.HORIZONTAL_SPLIT

    val savedSplitLocation = collection.mutable.Map[State, Double](
      State.FullV -> 0.75, State.FullH -> 0.52, State.NormV -> 0.75, State.NormH -> 0.52
    )  

    var savedWH = (getWidth(), getHeight())

    def isFull     = SwingPlatform.fullScreen.isFullScreen
    def isVertical = splitPane.getOrientation == JSplitPane.VERTICAL_SPLIT
    def location   = splitPane.getDividerLocation
    def splitWidth = splitPane.getDividerSize
    def fraction: Double = math.round(location * 100 / length.toDouble) / 100.0
    def length = if isVertical then splitPane.getHeight() else splitPane.getWidth()

    def debug(msg: String): Unit = 
      println(s"DEBUG: =========== SplitPaneState")
      println(msg)
      println(s"isVertical = $isVertical")
      println(s"location = $location")
      println(s"length = $length")
      println(s"splitWidth = $splitWidth")
      println(s"fraction = $fraction")

    def init(location: Double = 0.5): Unit = // must be done after setVisible(true) see further below
      splitPane.setDividerLocation(savedSplitLocation(currentState))

    def toggle(): Unit = 
      if isVertical 
      then splitPane.setOrientation(JSplitPane.HORIZONTAL_SPLIT)
      else splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT)

    def save(): Unit = savedSplitLocation(currentState) = fraction

    def restore(): Unit = splitPane.setDividerLocation(savedSplitLocation(currentState))
    
    enum State { case FullV, FullH, NormV, NormH}
    
    def currentState: State = isFull match
      case true  => if isVertical then State.FullV else State.FullH
      case false => if isVertical then State.NormV else State.NormH


  end SplitPaneState

  def doFileNew(): Unit = new EditorWindow()

  def doOpen(): Unit = runInSwingThread:
    for f <- SwingPlatform.chooseFile() do
      log(s"Open new Tree from $f")
      val t = loadLines(f).mkString("\n")
      setTopTo(t.toModel)
      didSaveTree()

  def doLoadToEditor(): Unit = runInSwingThread:
    for f <- SwingPlatform.chooseFile() do 
      val t = loadLines(f).mkString("\n")
      textArea.setText(t)
      updateFileName(f)
      log(s"Loaded $f to Editor.")
      didSaveEditor()
  
  def doSaveTree(): Unit = runInSwingThread:
    log(s"TODO: Save tree to $fileName")
    didSaveTree()

  def doSaveTreeAs(): Unit = runInSwingThread:
    log(s"TODO: Save tree As...  will update window title etc")
    didSaveTree()

  def doSaveEditorAs(): Unit = runInSwingThread:
    log(s"TODO: Save Editor as")
    didSaveEditor()
    //textArea.getText().saveTo(???)

  def askKeepEditing(action: String): Boolean = 
    SwingPlatform.isOK(s"""WARNING! You have unsaved changes! 
                          |Do you want to continue editing?
                          |Yes: Continue editing.
                          |No: $action without saving!""".stripMargin
                          , Some(this))

  def doClose(): Unit = runInSwingThread:
      dispatchEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING))
  
  def doQuit(): Unit = runInSwingThread:
    val isAllSaved = EditorWindow.started.forall(e => e.isSavedTree && e.isSavedEditor)
    if isAllSaved || !isAllSaved && !askKeepEditing("Quit") then 
      scala.sys.exit(0) // This is a brutal quit
    else ()

  def doEditNode() = runInSwingThread: 
    fromTreeToEditor()
    saveEditorNeeded()

  def doReplaceNode() = runInSwingThread:
    val m = textArea.getText().toModel
    updateSelection(m, isReplace = true)
    setFoldingAll(topPath, isExpand = true)
    saveTreeNeeded()
  
  def doInsertNode() = runInSwingThread:
    val m = textArea.getText().toModel
    updateSelection(m, isReplace = false)
    val s = tree.getSelectionPath().getParentPath()
    setFoldingAll(s, isExpand = true)
    saveTreeNeeded()

  def doToggleOrientation() = runInSwingThread:
    SplitPaneState.save()
    SplitPaneState.toggle()
    SplitPaneState.restore()

  def doToggleFullScreen() = runInSwingThread:
    SplitPaneState.save()
    if !SplitPaneState.isFull then SplitPaneState.savedWH = (getWidth, getHeight())
    SwingPlatform.fullScreen.toggleFullScreen(this)
    if !SplitPaneState.isFull then setSize(SplitPaneState.savedWH._1, SplitPaneState.savedWH._2)
    SplitPaneState.restore()
  
  def doExitFullScreen() = runInSwingThread: 
    if SplitPaneState.isFull then 
      SplitPaneState.save()
      SwingPlatform.fullScreen.exitFullScreen(this)
      setSize(SplitPaneState.savedWH._1, SplitPaneState.savedWH._2)
      SplitPaneState.restore()

  def doTogglePostIt() = runInSwingThread: 
    SplitPaneState.save()
    SwingPlatform.fullScreen.toggleDecorations(this)
    SplitPaneState.restore()

  def doIncrGlobalFontSize() = runInSwingThread:
      val s = frame.getFont.getSize
      if s < maxFontSize then setGlobalSwingFontSize(s + 1) 

  def doDecrGlobalFontSize() = runInSwingThread: 
    val s = frame.getFont.getSize
    if s > minFontSize then setGlobalSwingFontSize(s - 1)

  def doIncrFontSize(ta: JTextArea) = runInSwingThread:
    def incr(i: Int) = i match 
      case _ if i >= maxFontSize => maxFontSize
      case _ if i >= mediumFontSize => (i * 1.2).toInt
      case _ if i >= minFontSize => i + 1
      case _  => minFontSize
    setTextAreaFont(ta,incr(ta.getFont.getSize))

  def doDecrFontSize(ta: JTextArea) = runInSwingThread:
    def decr(i: Int) = i match 
      case _ if i > mediumFontSize => (i * 0.8).toInt
      case _ if i > minFontSize => i - 1
      case _  => i
    setTextAreaFont(ta,decr(ta.getFont.getSize))

  def doLineWrap(ta: JTextArea, isOn: Boolean) = runInSwingThread:
    ta.setLineWrap(isOn)

  def doClearMsg() = runInSwingThread(clearMessage())
  def doHelpToLog() = runInSwingThread(addMessage(EditorWindow.initMessage))
  def doConceptsToLog() = runInSwingThread(addMessage(meta.csv("\t")))
  def log(msg: String, logLevel: Int = 0) = runInSwingThread(addMessage(msg))

  def doFormatAll() = runInSwingThread:
    val txt = textArea.getText()
    val formatted = txt.toModel.toMarkdown 
    textArea.setText(formatted)

  def doFormatSelection() = runInSwingThread:
    // TODO: this needs more work 
    //      to expand selection to rows
    //      to analyze indentation and keep it good etc 
    val txt = textArea.getSelectedText()
    if txt != null then
      val formatted = txt.toModel.toMarkdown 
      textArea.replaceSelection(formatted)

  def doModelRawToLog() = runInSwingThread:
    val txt = Option(textArea.getText()).getOrElse("")
    addMessage(txt.toModel.toString)

  def doAppendIdPairs() = runInSwingThread:
    val txt = Option(textArea.getText()).getOrElse("")
    val ids = txt.toModel.ids
    val pairs = ids.combinations(2).map(xs => xs(0) + " > " + xs(1)).mkString("\n")
    if !txt.endsWith("\n") then textArea.append("\n")
    textArea.append(s"* Constraints:\n${pairs.trimIndent(2)}")

  var isEditorAppend = true

  def doTemplateToEditor(exampleKey: String) = runInSwingThread:
    val md = examples.menu(exampleKey).toMarkdown
    val txt = Option(textArea.getText()).getOrElse("")
    if isEditorAppend then 
      if txt.trim == "" then textArea.setText(md)
      else 
        if !txt.endsWith("\n") then textArea.append("\n")
        textArea.append(md) 
    else 
      log("TODO: make indentation of insertion match selection/cursor")
      if txt.trim == "" then textArea.setText(md)
      else textArea.replaceSelection(md)

  def doToggleFocus(): Unit = runInSwingThread:
    if !tree.hasFocus() then tree.requestFocus() else textArea.requestFocus()

  val exampleMenuItems: Seq[SwingPlatform.Item] = 
    val keys = examples.menu.keySet.toSeq.sorted
    for key <- keys yield SwingPlatform.Item(key, 0, 0, 0) { doTemplateToEditor(key) }

  val initMenus =
    import SwingPlatform.{AppMenus,Menu,Item,MenuSeparator,MenuRadioGroup}
    AppMenus(
      Menu("File", mnemonic = VK_F,
        Item("New Window", VK_N, VK_N, CTRL){ doFileNew() },
        Item("Open Model in Tree...", VK_O, VK_O, CTRL){ doOpen() },
        Item("Load Textfile to Editor...", VK_L, VK_L, CTRL){ doLoadToEditor() },
        Item("Save Model in Tree", VK_S, VK_S, CTRL){ doSaveTree() },
        Item("Save Model in Tree As...", VK_S, VK_S, CTRL+SHIFT){ doSaveTreeAs() },
        Item("Save Editor Text As...", VK_S, VK_S, ALT){ doSaveEditorAs() },
        MenuSeparator,
        Item("Close Window", VK_W, VK_W, CTRL){ doClose() },
        Item("Quit",VK_Q, VK_Q, CTRL){doQuit()},
      ),
      Menu("Tree", mnemonic = VK_T, 
        Item("Edit Tree Node in Editor", VK_E, VK_E, CTRL){ doEditNode()},
        Item("Update Tree Node from Editor", VK_U, VK_U, CTRL){ doReplaceNode()},
        Item("Insert After Node from Editor", VK_I, VK_I, CTRL){ doInsertNode()},
        MenuSeparator,
        Item("Toggle Focus Tree/Editor", VK_F,VK_T,CTRL) { doToggleFocus() },
        Item("Collapse All", VK_C, VK_LEFT, ALT){ runInSwingThread(setFoldingAll(topPath, isExpand = false))},
        Item("Expand All", VK_C, VK_RIGHT, ALT){ runInSwingThread(setFoldingAll(topPath, isExpand = true))},
        MenuSeparator,
        Item("Delete selected node", VK_D, VK_DELETE, 0){ log("TODO delete node")},
        Item("Revert to Initial Tree Model...", VK_V, VK_R, CTRL+SHIFT){ log("TODO revert")},
        MenuSeparator,
        MenuRadioGroup("treeNodeShow", Map[String, () => Unit](
          "Markdown" -> ( () => { runInSwingThread{treeItemShow = EditorWindow.TreeItemShow.Markdown; tree.updateUI()} } ),
          "Scala DSL"  -> ( () => { runInSwingThread{treeItemShow = EditorWindow.TreeItemShow.Factory; tree.updateUI()} } ),
          "Metamodel"  -> ( () => { runInSwingThread{treeItemShow = EditorWindow.TreeItemShow.Structure; tree.updateUI()} } ),
        ), default = "Markdown"),
      ),
      Menu("Editor", mnemonic = VK_E, 
        MenuRadioGroup("editorWrapToggle", Map[String, () => Unit](
          "Editor Line Wrap On" -> ( () => { doLineWrap(textArea, isOn = true)} ),
          "Editor Line Wrap Off"  -> ( () => { doLineWrap(textArea, isOn = false)} )
        ), default = "Editor Line Wrap Off"),
        MenuSeparator,
        Item("Format All", VK_F, VK_F, CTRL) { doFormatAll() },
        MenuSeparator,
        Item("Increase Editor Font Size", VK_T, VK_PLUS, CTRL)  { doIncrFontSize(textArea) },
        Item("Decrease Editor Font Size", VK_S, VK_MINUS, CTRL) { doDecrFontSize(textArea) },
      ),
      Menu("Log", mnemonic = VK_L,
        MenuRadioGroup("logWrapToggle", Map[String, () => Unit](
          "Log Line Wrap On" -> ( () => { doLineWrap(messageArea, isOn = true) } ),
          "Log Line Wrap Off"  -> ( () => { doLineWrap(messageArea, isOn = false) } )
        ), default = "Log Line Wrap Off"),
        MenuSeparator,
        Item("Increase Log Font Size", VK_L, VK_PLUS, CTRL+SHIFT)  { doIncrFontSize(messageArea) },
        Item("Decrease Log Font Size", VK_O, VK_MINUS, CTRL+SHIFT) { doDecrFontSize(messageArea) },
        MenuSeparator,
        Item("Clear Log", VK_C, VK_DELETE, ALT) { doClearMsg() },
      ),
      Menu("View", mnemonic = VK_V,
        Item("Toggle Orientation", VK_O, VK_F9, 0) { doToggleOrientation() },
        Item("Toggle Full Screen", VK_F, VK_F11, 0) { doToggleFullScreen()},
        Item("Toggle Post-It", VK_P, VK_F12, 0) { doTogglePostIt() },
        Item("Exit Full Screen", VK_E, VK_ESCAPE, 0) { doExitFullScreen() },
        MenuSeparator,
        Item("Increase Menu Size", VK_I, VK_PLUS, ALT+SHIFT) { doIncrGlobalFontSize() },
        Item("Decrease Menu Size", VK_D, VK_MINUS, ALT+SHIFT) { doDecrGlobalFontSize() },
      ),
      Menu("Templates", mnemonic = VK_M, (Seq(
        MenuRadioGroup("modelToEditorToggle", Map[String, () => Unit](
          "Append to Editor" -> ( () => { isEditorAppend = true } ),
          "Insert at Editor Cursor"  -> ( () => { isEditorAppend = false } ),
        ), default = "Append to Editor"),
        MenuSeparator,
      ) ++ exampleMenuItems)*),
      Menu("Tools", mnemonic = VK_O,
        Item("Parse to Log", VK_1, VK_1, CTRL+SHIFT) { doModelRawToLog() },
        Item("Append id pairs", VK_2, VK_2, CTRL+SHIFT) { doAppendIdPairs() },
        Item("Tool3", VK_3, VK_3, CTRL+SHIFT) { log("TODO TOOL 3")},
        Item("Tool4", VK_4, VK_4, CTRL+SHIFT) { log("TODO TOOL 4")},
      ),
      Menu("Help", mnemonic = VK_H,
        Item("Help Text to Log", VK_H, VK_F1, 0) { doHelpToLog() },
        Item("Concepts to Log", VK_C, VK_C, ALT) { doConceptsToLog()},
      ),
    )

  val menuMap: Map[String, JComponent] = initMenus.installTo(frame)

  def doMsg(msg: String): Unit = SwingPlatform.msgInfo(msg, parent = Some(frame))
  
  val defaultGlobalFontSize = Settings.gui.fontSize + fontDeltaByScreenHeight

  def fontDeltaByScreenHeight =
    java.awt.Toolkit.getDefaultToolkit.getScreenSize.getHeight match {
      case n if n <= 600 => 0
      case n if n <= 720 => 1
      case n if n <= 800 => 1
      case n if n <= 1024 => 2
      case n if n <= 1080 => 2
      case n if n <= 1440 => 3
      case _         => 2
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
      setTextAreaFont(textArea, textArea.getFont.getSize) // handle override of editor font size

    javax.swing.SwingUtilities.updateComponentTreeUI(frame)
  }

  //--- begin rsyntaxtextarea stuff  TODO
  // import org.fife.ui.autocomplete._
  import org.fife.ui.rtextarea.*
  import org.fife.ui.rsyntaxtextarea.*

  def setTextAreaFont(textArea: JTextArea, fontSize: Int, fontFamily: String = "") = SwingPlatform.runInSwingThread:
    val fn = if fontFamily == "" then textArea.getFont.getFamily else 
      val available = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
      val possible = (fontFamily :: Settings.gui.editorFonts).filter(available.contains(_))
      possible.headOption.getOrElse(Font.MONOSPACED)
    
    val fPlain = new Font(fn, Font.PLAIN, fontSize)
    val fBold = new Font(fn, Font.BOLD, fontSize)

    import java.awt.font.TextAttribute
    val textAttr: java.util.Map[TextAttribute, Object] = new java.util.HashMap()
    textAttr.put(TextAttribute.FONT, fBold)
    textAttr.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON)
    val fBoldUL = Font.getFont(textAttr)

    val fBoldItalic = new Font(fn, Font.BOLD | Font.ITALIC, fontSize)

    textArea.setFont(fPlain)
    
    textArea match 
      case ta: RSyntaxTextArea => 
        ta.getSyntaxScheme.setStyle(ReqTTokenMaker.EntTokenType, 
          new Style(Settings.gui.entityColor, Style.DEFAULT_BACKGROUND, fBold))

        ta.getSyntaxScheme.setStyle(ReqTTokenMaker.StrAttrTokenType,   
          new Style(Settings.gui.strAttributeColor, Style.DEFAULT_BACKGROUND, fBoldItalic))

        ta.getSyntaxScheme.setStyle(ReqTTokenMaker.IntAttrTokenType,   
          new Style(Settings.gui.intAttributeColor, Style.DEFAULT_BACKGROUND, fBoldItalic))

        ta.getSyntaxScheme.setStyle(ReqTTokenMaker.RelTokenType,    
          new Style(Settings.gui.relationColor, Style.DEFAULT_BACKGROUND, fBoldUL))
      case _ => // don't set syntax styles as this is not a syntax aware text area
    // textArea.getSyntaxScheme.setStyle(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, new Style(Settings.gui.stringColor))
    // textArea.getSyntaxScheme.setStyle(TokenTypes.RESERVED_WORD, new Style(Settings.gui.scalaReservedWordColor, Style.DEFAULT_BACKGROUND, fBold)) // more discrete coloring???
    val lnf = textPane.getGutter.getLineNumberFont
    val lnfNew = new Font(lnf.getFamily, lnf.getStyle, fontSize)
    textPane.getGutter.setLineNumberFont(lnfNew)
  end setTextAreaFont
  
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
  textArea.setLineWrap(false)
  textArea.setWrapStyleWord(true)
  textArea.setTabSize(2)
  textArea.setTabsEmulated(true)
  
  textArea.setMatchedBracketBGColor(new java.awt.Color(247, 247, 247))
  textArea.setMatchedBracketBorderColor(new java.awt.Color(192, 192, 192))
  textArea.setAnimateBracketMatching(true)
  
  setTextAreaFont(textArea, defaultGlobalFontSize, Settings.gui.defaultEditorFont)
  val textPane = new org.fife.ui.rtextarea.RTextScrollPane(textArea) with SwingPlatform.AntiAliasing
  
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

  val editMenu: JMenu = menuMap("Editor").asInstanceOf[JMenu]

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

  val caret = textArea.getCaret().asInstanceOf[javax.swing.text.DefaultCaret]
  caret.setUpdatePolicy(javax.swing.text.DefaultCaret.ALWAYS_UPDATE)

  //--- end rsyntaxtextarea stuff  
  
  val messageArea = new javax.swing.JTextArea(10, initEditorWidth)
  messageArea.setEditable(false)
  setTextAreaFont(messageArea, defaultGlobalFontSize, Settings.gui.defaultEditorFont)

  messageArea.setBackground(Settings.gui.logBackground)
  messageArea.setForeground(Settings.gui.logForeground)

  //messageArea.setLineWrap(true)

  val messagePane = new javax.swing.JScrollPane(messageArea)

  def scrollMsgToEnd(): Unit = 
    val sb = messagePane.getVerticalScrollBar()
    sb.setValue(sb.getMaximum())

  def scrollMsgToTop(): Unit = 
    val sb = messagePane.getVerticalScrollBar()
    sb.setValue(sb.getMinimum())

  def addMessage(msg: String): Unit =
    messageArea.append(msg + "\n")
    scrollMsgToEnd()

  def clearMessage(): Unit = messageArea.setText("")

  addMessage(EditorWindow.initMessage)

  //--- panes inside window


  val splitPane = new JSplitPane(SplitPaneState.initialSplit)  // (JSplitPane.HORIZONTAL_SPLIT) // see also below after setVisible(true)
  splitPane.setTopComponent(textPane)
  splitPane.setBottomComponent(messagePane)
  val (startHeight, startWidth) = (1200-250, 800)  // TODO: this should be saved in settings
  val smallestDim = new Dimension(100, 1)
  val prefferedDim = new Dimension(startWidth, startHeight)
  textPane.setMinimumSize(smallestDim)
  messagePane.setMinimumSize(smallestDim)
  splitPane.setPreferredSize(prefferedDim)

  // --- tree stuff
  import EditorWindow.{TreeItem, TreeItemBox, TreeRoot, TreeItemShow}
  val ThisTreeRoot = TreeRoot(fileName)
  var treeItemShow = TreeItemShow.Markdown
  val top = new DefaultMutableTreeNode(ThisTreeRoot)
  val tree = new JTree(top)
  val topPath = new TreePath(top)
  def treeModel: DefaultTreeModel = tree.getModel().asInstanceOf[DefaultTreeModel]
  def rootPath: TreePath = new TreePath(top)
  def mkNode(n: TreeItem) = new DefaultMutableTreeNode(TreeItemBox(n, this))

  def mkTreeFromModelAtNode(m: Model, node: DefaultMutableTreeNode): Unit = 
    m.elems.foreach: 
      case a: Attr[_] => node.add(mkNode(a))
      case e: Ent => node.add(mkNode(e))
      case Rel(e,l,t) =>
        val link = mkNode(Link(e,l))
        mkTreeFromModelAtNode(t, link)
        node.add(link)
  
  def setTopTo(m: Model): Unit = 
    top.removeAllChildren
    treeModel.nodeStructureChanged(top)
    mkTreeFromModelAtNode(m, top)
    treeModel.nodeStructureChanged(top)
    tree.setSelectionPath(topPath)
    tree.requestFocus
    tree.updateUI()

  def currentSelectionPath: TreePath = tree.getSelectionPath()

  def selectedOpt: Option[DefaultMutableTreeNode] = 
    if currentSelectionPath == null then None 
    else currentSelectionPath.getLastPathComponent match
      case n: DefaultMutableTreeNode => Some(n)
      case any => 
        log("ERROR: Unknown TreePath component:" + any)
        log("in def selectedOpt: path=" + currentSelectionPath.getPath.toVector) 
        None 

  def createModelFromTreeNode(fromNode: DefaultMutableTreeNode): Model = 

    def recur(node: DefaultMutableTreeNode): Model = 
      var elems: Vector[Elem] = Vector()
      val n = treeModel.getChildCount(node)
      for i <- 0 until n do
        val child = treeModel.getChild(node, i).asInstanceOf[DefaultMutableTreeNode]
        child.getUserObject match 
          case tib: TreeItemBox => tib.item match
            case e: Node => elems = elems :+ e
            case l: Link => elems = elems :+ Rel(l.e, l.t, recur(child))
          case ThisTreeRoot => 
            log("ERROR: ThisTreeRoot found in reucur in createModelFromTreeNode")
          case any => log("ERROR: match failed on TreeItemBox in createModelFromTreeNode: " + any)
      end for
      elems.toModel
    end recur 

    def sub = if (!fromNode.isLeaf) recur(fromNode) else Model()

    fromNode.getUserObject match 
      case tib: TreeItemBox => tib.item match
        case e: Node => Model(e)
        case l: Link => Model(Rel(l.e, l.t, sub))
      case ThisTreeRoot => recur(fromNode)
      case any =>
        throw new Error("match failed in createModelFromTreeNode: " + any)

  end createModelFromTreeNode

  def fromTreeToEditor(): Unit = {
    val currentSelection: TreePath = tree.getSelectionPath()
    if (currentSelection != null) {
      val currentNode =
        currentSelection.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
      //println("currentNode = " + currentNode)
      val t = createModelFromTreeNode(currentNode).toMarkdown
      textArea.setText(t)
      textArea.requestFocus
    } else 
      val t = createModelFromTreeNode(top).toMarkdown
      textArea.setText(t)
      textArea.requestFocus
  }

  def setFoldingAll(parent: TreePath, isExpand: Boolean): Unit = {
    val node = parent.getLastPathComponent().asInstanceOf[TreeNode];
    if (node.getChildCount() >= 0) {
      import scala.jdk.CollectionConverters.* 
      for (e <- node.children.asScala) {
        val n = e.asInstanceOf[TreeNode]
        val path = parent.pathByAddingChild(n).asInstanceOf[TreePath]
        setFoldingAll(path, isExpand);
      }
    }
    if (isExpand) tree.expandPath(parent)
    else {
      tree.collapsePath(parent)
      treeModel.reload
    }
  }

  def toTreePath(node: DefaultMutableTreeNode): TreePath = {
    val pathArray = treeModel.getPathToRoot(node)
    var treePath = new TreePath(pathArray(0))
    for (i <- 1 until pathArray.size) {
      treePath = treePath.pathByAddingChild(pathArray(i))
    }
    treePath
  }

  def removeCurrentNode(): Unit = {
    if (currentSelectionPath == null) () else {
      val currentNode =
        currentSelectionPath.getLastPathComponent().asInstanceOf[DefaultMutableTreeNode]
      val parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
      if (parent != null) {
        treeModel.removeNodeFromParent(currentNode)
        treeModel.nodeStructureChanged(parent)
        tree.setSelectionPath(toTreePath(parent))
      } else {
        top.removeAllChildren
        treeModel.nodeStructureChanged(top)
        tree.setSelectionPath(topPath)
      }
      // _currentModel = createModelFromTreeNode(top) // why??
      tree.requestFocus
    }
  }

  def expandSelectFocus(path: TreePath) = {
    tree.expandPath(path)
    tree.setSelectionPath(path)
    tree.requestFocus
  }

  def updateSelection(newModel: Model, isReplace: Boolean = true) = {
    selectedOpt match {
      case None => // nothing selected
        if isReplace then setTopTo(newModel)
        else 
          if newModel.elems.nonEmpty then setTopTo(createModelFromTreeNode(top) :++ newModel)
          else () // do nothing if newModel is empty
        tree.updateUI()
        //setFoldingAll(rootPath, false)

      case Some(currentNode) if currentNode == top =>  //top selected
        //log(s"DEBUG: tree root is selected, isReplace=$isReplace")
        val s = tree.getSelectionPath()
        if isReplace then setTopTo(newModel)
        else if newModel.elems.nonEmpty then 
          val m = createModelFromTreeNode(top)
          //log(s"DEBUG: createModelFromTreeNode(top) = $m")
          //log(s"DEBUG: newModel = $newModel")
          val combined = newModel :++ m
          //log(s"DEBUG: combined = $combined")
          setTopTo(combined)
          tree.setSelectionPath(s)
        tree.updateUI()
        //setFoldingAll(rootPath, false)

      case Some(currentNode) => //a node inside the tree was selected
        //log("DEBUG: selected node inside tree")
        val s = tree.getSelectionPath()
        iter(newModel.elems, isReplace, currentNode)  //new try; was: iter(newModel.toVector.reverse, isReplace, currentNode)
        if s != null then tree.setSelectionPath(s)
        tree.updateUI()
    }
    //recursive replace/insert
    def iter(elems: Vector[Elem], isReplace: Boolean, currentNode: DefaultMutableTreeNode): Unit = {
      elems.headOption match {
        case None => //empty elems
          if (isReplace) removeCurrentNode()
          else () //do nothing; inserting empty model yields no change
        case Some(elem) =>

          def update(node: DefaultMutableTreeNode) = {
            treeModel.nodeChanged(node)
            treeModel.nodeStructureChanged(node)
          }

          val parent = currentNode.getParent().asInstanceOf[DefaultMutableTreeNode]
          val parentPath = toTreePath(parent)
          val currentPath = currentSelectionPath
          if isReplace then //replace currentNode with elem
            currentNode.removeAllChildren
            update(currentNode)
            elem match {
              case Rel(e,t,submodel) =>
                currentNode.setUserObject(TreeItemBox(Link(e,t), this))
                update(currentNode)
                mkTreeFromModelAtNode(submodel, currentNode)
                update(currentNode)
              case e: Ent =>
                currentNode.setUserObject(TreeItemBox(e, this))
                update(currentNode)
              case a: Attr[?] => 
                currentNode.setUserObject(TreeItemBox(a, this))
                update(currentNode)

            }
            expandSelectFocus(currentPath)
            if elems.size > 1 then iter(elems.tail, false, currentNode) //recursively insert rest of model
          else //insert elem after currentNode
            var ix = parent.getIndex(currentNode)
            val newNode: DefaultMutableTreeNode = elem match {
              case Rel(e,t,submodel) =>
                val n = mkNode(Link(e,t))
                mkTreeFromModelAtNode(submodel, n)
                n
              case e: Ent => mkNode(e)
              case a: Attr[?] => mkNode(a)
            }
            parent.insert(newNode, ix+1)   ///new try; was: parent.insert(newNode, ix)
            update(parent)
            val newPath = toTreePath(newNode)
            tree.setSelectionPath(newPath)
            update(parent)
            expandSelectFocus(newPath)
            if elems.size > 1 then iter(elems.tail, false, newNode) //recursively insert rest of model
      }
    }
  }



  //tree.setEditable(true) ??? how much work is it to enable editing directly in the tree???
  tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
  tree.setSelectionPath(new TreePath(top))
  tree.addTreeSelectionListener(this)
  //tree.setEditable(true)  ???
  //tree.setDropMode(DropMode.INSERT) ???
  tree.setScrollsOnExpand(true)
  tree.setBackground(Settings.gui.treeBackground)
  tree.setAutoscrolls(true)
  tree.setDragEnabled(true) 
  tree.setDropMode(DropMode.ON_OR_INSERT)
  tree.setTransferHandler(new drag.JTreeTransferHandler())
  tree.getSelectionModel().setSelectionMode(TreeSelectionModel.CONTIGUOUS_TREE_SELECTION)


  //tree.addFocusListener(onFocusGained{ gui._lastFocused = Some(this) })  ??? from old reqT gui to allow repl to access current window in focus???
  val treeView = new JScrollPane(tree)

  // --- Create outer JSplitPane

  val topSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT)
  topSplitPane.setTopComponent(treeView)
  topSplitPane.setBottomComponent(splitPane)
  topSplitPane.setPreferredSize(new Dimension(1200, 800))
  topSplitPane.setDividerLocation(250)

  // --- Create outer panel

  panel.add(topSplitPane)
  setContentPane(panel)

  //setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)  //EXIT_ON_CLOSE
  setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)  

  addWindowListener:
    new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit = 
        if isSavedTree || !isSavedTree && !askKeepEditing("Close") 
        then frame.dispose()
        else ()
  
  textArea.getDocument().addDocumentListener( 
    new DocumentListener:
      override def changedUpdate(e: DocumentEvent): Unit = saveEditorNeeded()
      override def insertUpdate(e: DocumentEvent): Unit = saveEditorNeeded()
      override def removeUpdate(e: DocumentEvent): Unit = saveEditorNeeded()
  )

  textPane.updateUI
  pack()
  setLocationByPlatform(true)
  setVisible(true)
  SplitPaneState.init() // splitPane.setDividerLocation must be done after setVisible(true) !!!
  //SplitPaneState.debug()
  updateTitle()
  SwingPlatform.setAppIcon(this)
  setGlobalSwingFontSize(defaultGlobalFontSize)
  setTextAreaFont(textArea, defaultGlobalFontSize)
end EditorWindow