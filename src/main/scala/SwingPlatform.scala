package reqt

import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.KeyEvent
import java.awt.event.KeyListener
import java.awt.event.FocusListener
import java.awt.event.FocusEvent

import javax.swing.JOptionPane
import javax.swing.JFileChooser
import javax.swing.JFrame
import javax.swing.JMenuBar
import javax.swing.JComponent
import javax.swing.JMenuItem
import javax.swing.JMenu
import javax.swing.ButtonGroup
import javax.swing.JRadioButtonMenuItem
import javax.swing.JSeparator
import javax.swing.KeyStroke

//import javax.swing.plaf.FontUIResource
//import java.awt.{Component => AWTComponent, List => AWTList}

object SwingPlatform:
  private var _isSwingInit = false
  def isSwingInit = _isSwingInit

  /** Init the Swing GUI toolkit and set platform-specific look and feel.*/
  def swingInit(): Unit = if (!_isSwingInit) then
    setPlatformSpecificLookAndFeel()
    _isSwingInit = true

  def installedLookAndFeels: Vector[String] =
    javax.swing.UIManager.getInstalledLookAndFeels.toVector.map(_.getClassName)

  def findLookAndFeel(partOfName: String): Option[String] =
    installedLookAndFeels.find(_.toLowerCase.contains(partOfName))

  def setLookAndFeel(laf: String): Unit  = javax.swing.UIManager.setLookAndFeel(laf)

  def setPlatformSpecificLookAndFeel(): Unit = 
    if      Sys.isLinux   then findLookAndFeel("gtk").foreach(setLookAndFeel)
    else if Sys.isWindows then findLookAndFeel("win").foreach(setLookAndFeel)
    else if Sys.isMacOS   then findLookAndFeel("apple").foreach(setLookAndFeel)
    else javax.swing.UIManager.setLookAndFeel(
      javax.swing.UIManager.getSystemLookAndFeelClassName()
    )
  
  def msgInfo(msg: String, parent: Option[JFrame] = None) =  
    JOptionPane.showMessageDialog(parent.getOrElse(null), msg, 
      "Information", JOptionPane.INFORMATION_MESSAGE)

  def msgError(msg: String, parent: Option[JFrame] = None) =  
    JOptionPane.showMessageDialog(parent.getOrElse(null), msg, 
      "ERROR", JOptionPane.ERROR_MESSAGE)

  def isOK(msg: String, parent: Option[java.awt.Component] = None): Boolean =
    JOptionPane.showConfirmDialog(parent.getOrElse(null), msg, 
      "Confirm", JOptionPane.YES_NO_OPTION) == 0

  val fileChooser = JFileChooser(java.io.File(Sys.workDir))
  
  def chooseFile(
    preselected: String = "", 
    action: String = "Select file",
    parent: Option[java.awt.Component] = None, 
  ): Option[String] = 
    fileChooser.setSelectedFile(java.io.File(preselected))
    if fileChooser.showDialog(parent.getOrElse(null), action) == JFileChooser.APPROVE_OPTION
    then Some(fileChooser.getSelectedFile().getCanonicalPath)
    else None
  
  def runnable(code: => Unit): Runnable = new Runnable { override def run = code }

  def runInSwingThread(code: => Unit): Unit = 
    javax.swing.SwingUtilities.invokeLater(runnable(code)) 
  
  def onEvent(act: ActionEvent => Unit): ActionListener = new ActionListener:
    override def actionPerformed(e: ActionEvent) = act(e)

  def onAction(act: => Unit): ActionListener = onEvent( _ => act)
  
  def onKeyPressed(act: KeyEvent => Unit) = new KeyListener: 
    override def keyTyped(e: KeyEvent): Unit = { }
    override def keyPressed(e: KeyEvent): Unit = { act(e) }
    override def keyReleased(e: KeyEvent): Unit = { }

  @annotation.nowarn // getModifiers deprecated, use getModifiersEx since java 9
  def onCtrlEnter(action: => Unit): KeyListener = onKeyPressed: e =>
    if e.getKeyCode == KeyEvent.VK_ENTER && e.getModifiers == ActionEvent.CTRL_MASK 
    then action
  
  @annotation.nowarn// getModifiers deprecated, use getModifiersEx since java 9
  def onAltEnter(action: => Unit): KeyListener = onKeyPressed: e =>
    if e.getKeyCode == KeyEvent.VK_ENTER && e.getModifiers == ActionEvent.ALT_MASK
    then action

  def onFocusGained(action: => Unit): FocusListener = new FocusListener:
    override def focusGained(e: FocusEvent): Unit = action
    override def focusLost(e: FocusEvent): Unit = { }

  // ---------- A little DSL for declarative specification of menu trees
  
  def ===> = MenuBranch
  def ---> = MenuLeaf
  def ---  = MenuSeparator

  case class AppMenus(menus: MenuBranch*):
    def installTo(frame: JFrame): Map[String, JComponent]  = 
      val menuBar = JMenuBar()
      frame.setJMenuBar(menuBar)
      menus.map(_.addTo(menuBar)).reduceLeft(_ ++ _)

  sealed trait MenuTree

  case class MenuLeaf(name: String, shortcut: Int, accelerator: Int, mask: Int)(block: => Unit) extends MenuTree:
      def action = new ActionListener:
        override def actionPerformed(e: ActionEvent) = block

  case class MenuRadioGroup(groupName: String, actionMap: Map[String,() => Unit], default: String) extends MenuTree:
      def action = new ActionListener:
        override def actionPerformed(e: ActionEvent) = 
          val buttonText = e.getActionCommand
          if actionMap.isDefinedAt(buttonText) then actionMap(buttonText)()
  
  case object MenuSeparator extends MenuTree

  case class MenuBranch(name: String, mnemonic: Int, menus: MenuTree*) extends MenuTree:
    def addTo(parent: JComponent): Map[String, JComponent] = 
      var menuMap: Map[String, JComponent] = Map()

      def addToMenuMap(name: String, menu: JComponent) =
        if !menuMap.isDefinedAt(name) 
        then menuMap += name -> menu
        else throw new Error("Menu name not unique: " + name)

      def loop(parent: JComponent, menus: Seq[MenuTree]): Unit = menus.foreach: menu => 
        menu match 
        case m: MenuLeaf =>
          val jmi = new JMenuItem(m.name, m.shortcut)
          jmi.addActionListener(m.action)
          if m.accelerator > 0 then jmi.setAccelerator(KeyStroke.getKeyStroke(m.accelerator, m.mask))
          parent.add(jmi)
          addToMenuMap(m.name, jmi)

        case m: MenuBranch =>
          val jm = new JMenu(m.name)
          if (m.mnemonic>0) jm.setMnemonic(m.mnemonic)
          parent.add(jm)
          addToMenuMap(m.name, jm)
          loop(jm, m.menus)
        
        case m: MenuRadioGroup =>
          val group = new ButtonGroup()
          m.actionMap.foreach { case (buttonText, _ )  =>
            val radioButton = new JRadioButtonMenuItem(buttonText)
            if (buttonText == m.default) radioButton.setSelected(true)
            radioButton.addActionListener(m.action)
            group.add(radioButton)
            parent.add(radioButton)
            addToMenuMap(m.groupName+buttonText, radioButton)
          }
        
        case MenuSeparator =>
          val js = new JSeparator()
          parent.add(js)
      end loop

      loop(parent, Seq(this))
      menuMap
    end addTo

  def mkMenuItem(name: String, menu: JMenuItem, shortcut: (Int, Int, Int))
      (action: => Unit): Unit = {
    val mi = new JMenuItem(name, shortcut._1)
    mi.addActionListener( onAction { action } )
    if (shortcut._2 != 0)
      mi.setAccelerator(KeyStroke.getKeyStroke(shortcut._2, shortcut._3))
    menu.add(mi)
  }

  def mkMenu(name: String, mnemonic: Int): JMenu = {
    val jm = new JMenu(name)
    jm.setMnemonic(mnemonic)
    jm
  }

  def keyCode(c: Char) = java.awt.AWTKeyStroke.getAWTKeyStroke(c.toUpper.toString).getKeyCode()

  object fullScreen:
    val device = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice

    def isSupported = device.isFullScreenSupported
    
    def isFullScreen = device.getFullScreenWindow != null
    
    private def exitFS = device.setFullScreenWindow(null)
    
    def exitFullScreen(w: java.awt.Window): Unit = { exitFS; setUndecorated(w, false) }
    
    def toggleFullScreen(w: java.awt.Window): Unit = 
      if isFullScreen then exitFullScreen(w) 
      else 
        scala.util.Try { setUndecorated(w, true); device.setFullScreenWindow(w) }
          .recover{ case e: Exception => println(s"Full screen failed: $e") }
        
    private def setUndecorated(w: java.awt.Window, state: Boolean): Unit =  w match 
      case f: JFrame =>
        f.dispose(); f.setUndecorated(state);  f.pack(); f.setVisible(true)
      case _ => ()
    
    def toggleDecorations(w: java.awt.Window): Unit = w match 
      case f: JFrame => 
        if f.isUndecorated then setUndecorated(f, false) else setUndecorated(f, true)
      case _ => ()

  end fullScreen

  trait AntiAliasing extends JComponent:
    override def paintComponent(g: java.awt.Graphics): Unit = 
        val g2 = g.asInstanceOf[java.awt.Graphics2D]
        g2.setRenderingHint(
          java.awt.RenderingHints.KEY_TEXT_ANTIALIASING,
          java.awt.RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
        g2.setRenderingHint(
          java.awt.RenderingHints.KEY_RENDERING,
          java.awt.RenderingHints.VALUE_RENDER_QUALITY)
        super.paintComponent(g2)
  end AntiAliasing
  
  

