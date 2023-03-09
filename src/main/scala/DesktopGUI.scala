package reqt

import reqt.SwingPlatform.*
import reqt.Sys.newFileType

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

object DesktopGUI:
  val x = 42


class DesktopGUI extends JFrame:
  val initModel: Model = Model()
  val windowType = "reqT Editor Window"
  val frame = this

  val initEditorWidth = 80
  val initEditorHeight = 30
  val maxFontSize = 80
  val bigFontSize = 48
  val mediumFontSize = 18
  val minFontSize = 6
  
  private var _fileName = "untitled.reqt"
  def fileName = _fileName 
  def windowTitle = fileName + "  -  " + windowType
  def updateTitle() = frame.setTitle(windowTitle) 
  def updateFileName(fn: String) = { _fileName = fn; updateTitle() }

  val initMenus =
    AppMenus(
      MenuBranch("File", mnemonic = VK_F,
        MenuLeaf("New Window", shortcut = VK_N, accelerator = VK_N, mask = CTRL){ doMsg("new window") }
      )
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
  // import org.fife.ui.rtextarea._
  // import org.fife.ui.rsyntaxtextarea._

  def setEditorFont(fontSize: Int, fontFamily: String = "") = //runInSwingThread:
    val fn = if (fontFamily == "") textArea.getFont.getFamily else {
      val available = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames
      val possible = (fontFamily :: Settings.gui.editorFonts).filter(available.contains(_))
      possible.headOption.getOrElse(Font.MONOSPACED)
    }
    println(s"font = $fn")
    val fPlain = new Font(fn, Font.PLAIN, fontSize)
    textArea.setFont(fPlain)
    val fBold = new Font(fn, Font.BOLD, fontSize)
    // editor.getSyntaxScheme.setStyle(ENTITY_TOKEN, new Style(Settings.gui.entityColor, Style.DEFAULT_BACKGROUND, fBold))
    // editor.getSyntaxScheme.setStyle(ATTR_TOKEN,   new Style(Settings.gui.attributeColor, Style.DEFAULT_BACKGROUND, fBold))
    // editor.getSyntaxScheme.setStyle(REL_TOKEN,    new Style(Settings.gui.relationColor, Style.DEFAULT_BACKGROUND, fBold))
    // editor.getSyntaxScheme.setStyle(TokenTypes.LITERAL_STRING_DOUBLE_QUOTE, new Style(Settings.gui.stringColor))
    // editor.getSyntaxScheme.setStyle(TokenTypes.RESERVED_WORD, new Style(Settings.gui.scalaReservedWordColor, Style.DEFAULT_BACKGROUND, fBold)) // more descrete coloring???
    // val lnf = editorView.getGutter.getLineNumberFont
    // val lnfNew = new Font(lnf.getFamily, lnf.getStyle, fontSize - 2)
    // editorView.getGutter.setLineNumberFont(lnfNew)
  
  val panel = JPanel(java.awt.BorderLayout())
  val textArea = new org.fife.ui.rsyntaxtextarea.RSyntaxTextArea(initEditorHeight, initEditorWidth) 
  //textArea.setSyntaxEditingStyle(org.fife.ui.rsyntaxtextarea.SyntaxConstants.SYNTAX_STYLE_JAVA)
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
  //textArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 20))
  setEditorFont(mediumFontSize, "Monospaced")

  val textPane = new org.fife.ui.rtextarea.RTextScrollPane(textArea) with AntiAliasing
  
  panel.add(textPane)
  setContentPane(panel)
  setTitle("TITLE TODO")
  setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)  //EXIT_ON_CLOSE
  
  textPane.updateUI
  pack()
  setLocationRelativeTo(null)
  setVisible(true)
  updateTitle()

  //--- end rsyntaxtextarea stuff  TODO

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
end DesktopGUI